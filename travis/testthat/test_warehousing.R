
library(DBI)
library(odbc)
 

# SQLite ------------------------------------------------------------------


test_that("Ramses on SQLite", {
  
  drug_data <- Ramses:::.prepare_example_drug_records()
  conSQLite <- suppressWarnings(connect_db_local("ramses-db.sqlite"))
  
  expect_null(validate_prescriptions(drug_data$drug_rx))
  expect_null(validate_administrations(drug_data$drug_admins))
  
  load_medications(conn = conSQLite, 
                   prescriptions = drug_data$drug_rx,
                   administrations = drug_data$drug_admins,
                   overwrite = TRUE)

  test_output <- tbl(conSQLite, "drug_prescriptions") %>% 
    filter(prescription_id %in% c("60caeda91f36308ba857100193d2f504", "fb7d0847a420d38a5ccc10c017ec91e6")) %>% 
    select(prescription_id, combination_id, therapy_id) %>% 
    collect()

  expect_equivalent(
    test_output, 
    tibble(prescription_id = c("60caeda91f36308ba857100193d2f504", "fb7d0847a420d38a5ccc10c017ec91e6"),
           combination_id = c(NA_character_, "717e21dcc485b7366ed825177e74c907"),
           therapy_id = c("15f7c729771ea989617e6ff97c3caa8a", "15f7c729771ea989617e6ff97c3caa8a")))
  
  DBI::dbDisconnect(conSQLite)
  file.remove("inst/ramses-db.sqlite")
  
})


test_that("SQLite does transitive closure", {
  
  test_edges <- tibble(
    id1 = as.character(c(1,1,2,5,6,7)),
    id2 = as.character(c(2,3,4,6,7,8))
  )
  
  test_solution <- tibble(
    id =  as.integer(c(1,2,3,4,5,6,7,8)),
    grp = as.integer(c(1,1,1,1,5,5,5,5))
  )
  
  conSQLite <- suppressWarnings(
    connect_db_local("test.sqlite"))
  
  dbplyr::db_copy_to(conSQLite,
                     "ramses_test_edges",
                     test_edges,
                     overwrite = T,
                     temporary = F)
  
  test_output <- Ramses:::.run_transitive_closure.SQLiteConnection(
    conSQLite,"ramses_test_edges") %>% 
    dplyr::select(id, grp) %>% 
    dplyr::arrange(id) %>% 
    dplyr::collect()
  
  expect_equal(test_output,
               test_solution)
  DBI::dbDisconnect(conSQLite)
  file.remove("test.sqlite")  
})

# PostgreSQL --------------------------------------------------------------



# conPostgreSQL <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
#                                 user = "user", password = "password", 
#                                 host = "db-postgres", dbname="RamsesDB")
# 
# conSQLServer <- dbConnect(
#   odbc::odbc(),
#   .connection_string = "Driver={ODBC Driver 17 for SQL Server}; Server=db-mssql,1433;Uid=Sa; Pwd=bidultrukCestLeurTruc!;", timeout = 10)



# test_that("Ramses on PosgreSQL", {
#   test_warehousing(conPostgreSQL, drug_data, overwrite = T)
#   
# })


# MS SQL Server -----------------------------------------------------------


# 
# test_that("Ramses on MS SQL Server", {
#   test_warehousing(conMS, drug_data, overwrite = T)
# 
# })



