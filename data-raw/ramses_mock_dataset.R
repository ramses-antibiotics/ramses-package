## code to prepare `ramses_mock_dataset` dataset goes here


devtools::load_all(".")

# charlson_regex ----------------------------------------------------------

# Charlson Comorbidity regular expressions from the comorbidity package
charlson_regex <- lapply(
  comorbidity:::.maps$charlson_icd10_quan, 
  function(X) { 
    paste(paste0("^", X), collapse = "|")
  }
)

# Prescriptions -----------------------------------------------------------

drug_rx <- Ramses::drug_prescriptions
drug_admins <- Ramses::drug_administrations

drug_rx$antiinfective_type <- "antibacterial"
drug_admins$antiinfective_type <- "antibacterial"
drug_rx$drug_code <- gsub("Vancomycin protocol", "Vancomycin", drug_rx$tr_DESC)
drug_rx$drug_code <- as.character(AMR::as.ab(drug_rx$drug_code))
drug_rx$drug_name <- AMR::ab_name(drug_rx$drug_code)
drug_rx$drug_group <- AMR::ab_group(drug_rx$drug_code)
## recoding route of administration
drug_rx <- dplyr::mutate(
  drug_rx, 
  ATC_route = dplyr::case_when(
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
drug_rx$ATC_code <- AMR::ab_atc(drug_rx$drug_code, only_first = TRUE)

# prepare DDD extraction
compound_strength_lookup <- data.frame(list(
  drug_code = c("AMC", "AMC", "TZP", "SMX"),
  route = c("oral", "oral", "oral", "oral"),
  dose = c(625, 1.2, 4.5, 480),
  units = c("mg", "g", "g", "mg"),
  strength = c(500, 1, 4, 400),
  basis_of_strength = c("AMX", "AMX", "PIP", "SMX")
), stringsAsFactors = F)

drug_rx <- merge(drug_rx, compound_strength_lookup, all.x = T)
drug_rx <- drug_rx %>% 
  dplyr::mutate(strength = dplyr::if_else(is.na(strength), dose, strength),  
                basis_of_strength = dplyr::if_else(is.na(basis_of_strength),
                                                   as.character(drug_code), basis_of_strength))

drug_rx <- merge(drug_rx, reference_drug_frequency, by = "frequency", all.x = T)

# computing the prescription DDD the reference DDD from the ATC
drug_rx <- drug_rx %>% 
  dplyr::mutate(DDD = compute_DDDs(
    ATC_code = AMR::ab_atc(basis_of_strength, only_first = TRUE),
    ATC_administration = ATC_route,
    dose = strength * daily_frequency,
    unit = units, 
    silent = TRUE),
    duration_days = dplyr::if_else(
      daily_frequency == -1,
      "one-off", 
      dplyr::if_else(
        round(difftime(prescription_end, 
                       prescription_start, 
                       units = "days")) == 1,
        "1 day", 
        paste(round(difftime(prescription_end, 
                             prescription_start, 
                             units = "days")),
              "days")
      )
    )) %>% 
  dplyr::transmute(patient_id,
                   prescription_id,
                   prescription_text = paste0(
                     drug_name, " ", route, " ", dose, units,
                     " ",  duration_days),
                   drug_code,
                   drug_name = drug_name,
                   drug_display_name = drug_name,
                   drug_group,
                   ATC_code,
                   ATC_route,
                   antiinfective_type,
                   authoring_date,
                   prescription_start,
                   prescription_end,
                   prescription_status = "completed",
                   prescription_context,
                   dose,
                   unit = units,
                   route,
                   frequency,
                   daily_frequency,
                   DDD)


## prepare_drug_admin
drug_admins$drug_code <- gsub("Vancomycin protocol", "Vancomycin", drug_admins$tr_DESC)
drug_admins$drug_code <- as.character(AMR::as.ab(drug_admins$drug_code))
drug_admins$drug_name <- AMR::ab_name(drug_admins$drug_code)
drug_admins$drug_group <- AMR::ab_group(drug_admins$drug_code)

# recoding route of administration
drug_admins <- dplyr::mutate(
  drug_admins, 
  ATC_route = dplyr::case_when(
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
drug_admins$ATC_code <- AMR::ab_atc(drug_admins$drug_code, only_first = TRUE)

drug_admins <- merge(drug_admins, compound_strength_lookup, all.x = T)
drug_admins <- drug_admins %>% 
  dplyr::mutate(
    strength = dplyr::if_else(is.na(strength), dose, strength),
    basis_of_strength = dplyr::if_else(is.na(basis_of_strength),
                                       as.character(drug_code), basis_of_strength))

drug_admins <- drug_admins %>% 
  dplyr::mutate(
    DDD = compute_DDDs(
      ATC_code = AMR::ab_atc(basis_of_strength, only_first = TRUE),
      ATC_administration = ATC_route,
      dose = dose,
      unit = units,
      silent = TRUE
    ))

if ( utils::packageVersion("dplyr") >= "1.0.0" ) {
  drug_admins <- drug_admins %>% 
    dplyr::group_by(
      patient_id,
      drug_code,
      route,
      dose,
      units,
      administration_date) %>% 
    dplyr::mutate(administration_id = dplyr::cur_group_id()) %>% 
    dplyr::ungroup()
} else {
  drug_admins <- drug_admins %>% 
    dplyr::group_by(
      patient_id,
      drug_code,
      route,
      dose,
      units,
      administration_date) %>% 
    dplyr::mutate(administration_id = dplyr::group_indices()) %>% 
    dplyr::ungroup()
}

drug_admins <- dplyr::transmute(drug_admins,
                                patient_id,
                                administration_id = as.character(administration_id),
                                prescription_id,
                                administration_text = paste0(
                                  drug_name, " ", route, " ", dose, units),
                                drug_code,
                                drug_name,
                                drug_display_name = drug_name,
                                drug_group,
                                antiinfective_type,
                                ATC_code,
                                ATC_route,
                                dose,
                                unit = units,
                                route,
                                administration_date,
                                administration_status = "completed",
                                DDD
)


# Inpatient episodes & diagnoses ------------------------------------------

ip_patients <- Ramses::patients
ip_diagnoses <- Ramses::inpatient_diagnoses
ip_diagnoses <- dplyr::filter(ip_diagnoses, !is.na(icd_code))

ip_episodes <- Ramses::inpatient_episodes
ip_episodes <- ip_episodes %>% 
  dplyr::filter(!is.na(encounter_id)) %>% 
  dplyr::group_by(encounter_id) %>% 
  dplyr::mutate(last_episode_in_encounter = dplyr::if_else(
    episode_number == max(episode_number),
    1, 2)) %>% 
  dplyr::ungroup()

  

# Microbiology ------------------------------------------------------------

micro <- list()
micro$raw <- Ramses::inpatient_microbiology
micro$raw <- micro$raw %>% 
  dplyr::mutate(
    organism_code = AMR::as.mo(dplyr::if_else(
      organism_display_name == "No growth",
      NA_character_,
      organism_display_name)),
    agent_code = AMR::as.ab(agent_display_name)
  ) %>% 
  dplyr::mutate(organism_name = AMR::mo_name(organism_code),
                agent_name = AMR::ab_name(agent_code))
micro$raw <- micro$raw %>% 
  dplyr::mutate(specimen_type_code = dplyr::case_when(
    specimen_type_display == "Blood Culture" ~ 
      "446131002", # Blood specimen obtained for blood culture
    specimen_type_display == "Faeces" ~ 
      "119339001", # Stool specimen
    specimen_type_display == "MRSA Screen" ~ 
      "697989009", # Anterior nares swab 
    specimen_type_display == "Urine" ~ 
      "122575003", # Urine specimen
    TRUE ~ NA_character_
  )) %>% 
  dplyr::left_join(
    dplyr::transmute(reference_specimen_type,
                     specimen_type_code = conceptId,
                     specimen_type_name = pt_term),
    by = "specimen_type_code"
  )
micro$specimens <- micro$raw %>% 
  dplyr::transmute(specimen_id,
                   patient_id,
                   status = "available",
                   specimen_datetime,
                   specimen_type_code,
                   specimen_type_name,
                   specimen_type_display) %>% 
  dplyr::distinct() # Removing duplicates created by multiple isolates and susceptibility testing

micro$isolates <- micro$raw %>% 
  dplyr::transmute(isolate_id,
                   specimen_id,
                   patient_id,
                   organism_code,
                   organism_name,
                   organism_display_name,
                   isolation_datetime) %>% 
  dplyr::distinct() # Removing duplicates created by susceptibility testing

micro$susceptibilities <- micro$raw %>% 
  dplyr::filter(!is.na(organism_code)) %>%  # Remove no growth
  dplyr::transmute(isolate_id,
                   specimen_id,
                   patient_id,
                   organism_code,
                   organism_name,
                   organism_display_name,
                   agent_code,
                   agent_name,
                   agent_display_name,
                   rsi_code,
                   concept_code = NA_character_) %>% 
  dplyr::distinct()
micro$raw <- NULL
  

# Import ------------------------------------------------------------------

.ramses_mock_dataset <- list(
  patients = ip_patients,
  episodes = ip_episodes,
  diagnoses = ip_diagnoses,
  micro = micro,
  drug_rx = drug_rx,
  drug_admins = drug_admins,
  icd10cm_2020 = download_icd10cm()
)

usethis::use_data(.ramses_mock_dataset, 
                  charlson_regex,
                  overwrite = TRUE, 
                  internal = TRUE)

rm(list=ls())