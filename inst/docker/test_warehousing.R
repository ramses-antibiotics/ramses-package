

library(DBI)
library(odbc)
library(dbplyr)
library(dplyr)
devtools::load_all()

# conPostgreSQL <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
#                                 user = "user", password = "password", 
#                                 host = "db-postgres", dbname="RamsesDB")
# 
# conSQLServer <- dbConnect(
#   odbc::odbc(),
#   .connection_string = "Driver={ODBC Driver 17 for SQL Server}; Server=db-mssql,1433;Uid=Sa; Pwd=bidultrukCestLeurTruc!;", timeout = 10)

conSQLite <- connect_db_local("inst/ramses-db.sqlite")

drug_data <- prepare_drugs()

validate_prescriptions(drug_data$drug_rx)
 

test_that("Ramses on SQLite", {
  test_warehousing(conSQLite, drug_data, overwrite = T)
  
})

statement <- readLines("inst/SQL/drug_prescriptions_edges_SQLite.sql")
# remove comments
statement <- sapply(statement, gsub, pattern = "--.*", replacement = "")
# concatenate on single line
statement <- paste(statement, collapse = " ")





test_that("Ramses on PosgreSQL", {
  test_warehousing(conPostgreSQL, drug_data, overwrite = T)
  
})


test_that("Ramses on MS SQL Server", {
  test_warehousing(conMS, drug_data, overwrite = T)

})



prepare_drugs <- function() {
  
  drug_rx <- Ramses::drug_prescriptions
  drug_admins <- Ramses::drug_administrations
  
  drug_rx$ab <- gsub("Vancomycin protocol", "Vancomycin", drug_rx$tr_DESC)
  drug_rx$ab <- as.character(AMR::as.ab(drug_rx$ab))
  drug_rx$drug_name <- AMR::ab_name(drug_rx$ab)
  ## recoding route of administration
  drug_rx <- mutate(
    drug_rx, 
    ATC_route = case_when(
      route %in% c(NULL) ~ "Implant", 
      route %in% c("NEB", "INHAL") ~ "Inhal", 
      route %in% c("TOP", "EYE", "EYEL", "EYER", "EYEB", 
                   "EAR", "EARL", "EARR", "EARB") ~ "Instill", 
      route %in% c("NASAL", "NOST", "NOSTL", "NOSTR", "NOSTB") ~ "N", 
      route %in% c("ORAL", "NAS", "PEG") ~ "O", 
      route %in% c("IV", "IVB", "IVI", "IMI", "IT", "IVT") ~ "P", 
      route %in% c("PR") ~ "R", 
      route %in% c("BUCC", "SB", "OROM", "SUBL") ~ "SL", 
      route %in% c("SC", "ID") ~ "TD", 
      route %in% c("PV") ~ "V", 
      TRUE ~ "NA_character_"
    ))
  
  ## compute_ddd
  drug_rx$ATC_code <- AMR::ab_atc(drug_rx$ab)
  drug_rx$ATC_group <- AMR::ab_atc_group1(drug_rx$ab)
  
  # prepare DDD extraction
  compound_strength_lookup <- data.frame(list(
    ab = c("AMC", "AMC", "TZP", "SMX"),
    route = c("oral", "oral", "oral", "oral"),
    dose = c(625, 1.2, 4.5, 480),
    units = c("mg", "g", "g", "mg"),
    strength = c(500, 1, 4, 400),
    basis_of_strength = c("AMX", "AMX", "PIP", "SMX")
  ), stringsAsFactors = F)
  
  drug_rx <- merge(drug_rx, compound_strength_lookup, all.x = T)
  drug_rx <- drug_rx %>% 
    mutate(strength = if_else(is.na(strength), dose, strength),  
           basis_of_strength = if_else(is.na(basis_of_strength),
                                       as.character(ab), basis_of_strength))
  
  drug_rx <- merge(drug_rx, LU_frequency, by = "freq", all.x = T)
  
  # computing the prescription DDD the reference DDD from the ATC
  drug_rx <- drug_rx %>% 
    mutate(daily_dose = strength * daily_freq) %>% 
    mutate(DDD = compute_DDDs(
      ab = basis_of_strength,
      administration = ATC_route,
      dose = daily_dose,
      unit = units),
      duration_days = if_else(
        daily_freq == -1,
        "one-off", paste( difftime(
          prescription_end, prescription_start, units = "days"),
        "days"))) %>% 
    transmute(patient_id,
              prescription_id,
              prescription_text = paste0(
                drug_name, " ", route, " ", dose, units,
                " ",  duration_days),
              drug_id = ab,
              drug_name = drug_name,
              drug_display_name = drug_name,
              ATC_code,
              ATC_group, 
              ATC_route,
              authoring_date = authored_on,
              prescription_start,
              prescription_end,
              prescription_status = "completed",
              prescription_context = "inpatient",
              dose,
              unit = units,
              route,
              frequency = freq,
              daily_frequency = daily_freq,
              DDD)
  
  
  ## prepare_drug_admin
  drug_admins$ab <- gsub("Vancomycin protocol", "Vancomycin", drug_admins$tr_DESC)
  drug_admins$ab <- as.character(AMR::as.ab(drug_admins$ab))
  drug_admins$drug_name <- AMR::ab_name(drug_admins$ab)
  # recoding route of administration
  drug_admins <- mutate(
    drug_admins, 
    route_atc = case_when(
      route %in% c(NULL) ~ "Implant", 
      route %in% c("NEB", "INHAL") ~ "Inhal", 
      route %in% c("TOP", "EYE", "EYEL", "EYER", "EYEB", 
                   "EAR", "EARL", "EARR", "EARB") ~ "Instill", 
      route %in% c("NASAL", "NOST", "NOSTL", "NOSTR", "NOSTB") ~ "N", 
      route %in% c("ORAL", "NAS", "PEG") ~ "O", 
      route %in% c("IV", "IVB", "IVI", "IMI", "IT", "IVT") ~ "P", 
      route %in% c("PR") ~ "R", 
      route %in% c("BUCC", "SB", "OROM", "SUBL") ~ "SL", 
      route %in% c("SC", "ID") ~ "TD", 
      route %in% c("PV") ~ "V", 
      TRUE ~ "NA_character_"
    ))
  drug_admins$ATC_code <- AMR::ab_atc(drug_admins$ab)
  drug_admins$ATC_group <- AMR::ab_atc_group1(drug_admins$ab)

  drug_admins <- merge(drug_admins, compound_strength_lookup, all.x = T)
  drug_admins <- drug_admins %>% 
    mutate(strength = if_else(is.na(strength), dose, strength),
           basis_of_strength = if_else(is.na(basis_of_strength),
                                       as.character(ab), basis_of_strength))
  
  drug_admins <- drug_admins %>% 
    mutate(ddd = compute_DDDs(
      ab = basis_of_strength,
      administration = route_atc,
      dose = dose,
      unit = units 
    ))
  
  return(list(
    drug_rx = drug_rx,
    drug_admins = drug_admins
  ))
  
}



test_warehousing <- function(con, drugs = prepare_drugs(), overwrite = T){

  load_medications(conn = con, 
                   prescriptions = drugs$drug_rx,
                   administrations = drugs$drug_admins,
                   overwrite = overwrite)
}
