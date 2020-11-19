

# SQLite ------------------------------------------------------------------


test_that("Ramses on SQLite", {
  
  if (!identical(Sys.getenv("TRAVISTESTS"), "true")) {
    skip("Test only on Travis")
  }
  

  # > validate functions --------------------------------------------------
  
  drug_data <- Ramses:::.prepare_example_drug_records()
  inpatient_data <- Ramses:::.prepare_example_inpatient_records()
  icd10cm <- download_icd10cm()
  conSQLite <- suppressWarnings(connect_db_local("test.sqlite"))
  
  expect_null(validate_prescriptions(drug_data$drug_rx))
  expect_null(validate_administrations(drug_data$drug_admins))
  
  expect_warning(
    validate_inpatient_diagnoses(
      diagnoses_data = inpatient_data$diagnoses,
      diagnoses_lookup = icd10cm))
  expect_true(validate_investigations(inpatient_data$investigations))
  expect_true(validate_microbiology(
    inpatient_data$micro$specimens,
    inpatient_data$micro$isolates,
    inpatient_data$micro$susceptibilities
    ))
  expect_true(validate_inpatient_episodes(inpatient_data$episodes))
  expect_true(validate_inpatient_episodes(episodes = inpatient_data$episodes,
                                          wards = inpatient_data$ward_movements))
  medication_loading <- load_medications(conn = conSQLite, 
                         prescriptions = drug_data$drug_rx,
                         administrations = drug_data$drug_admins,
                         overwrite = TRUE)
  
  expect_true(medication_loading$prescription_load_errors)
  expect_true(medication_loading$administration_load_errors)
  
  expect_equivalent(
    load_inpatient_episodes(conn = conSQLite,
                            episodes_data = inpatient_data$episodes,
                            wards_data = inpatient_data$ward_movements,
                            overwrite = TRUE),
    list(episodes_load_errors = TRUE,
         wards_load_errors = TRUE)
  )
  expect_true(
    expect_warning(
      load_inpatient_diagnoses(conn = conSQLite,
                           diagnoses_data = inpatient_data$diagnoses,
                           diagnoses_lookup = icd10cm,
                           overwrite = TRUE)))
  expect_true(
    load_inpatient_investigations(
      conn = conSQLite,
      investigations_data = inpatient_data$investigations,
      overwrite = TRUE
    ))
  expect_true(load_inpatient_microbiology(
    conn = conSQLite,
    inpatient_data$micro$specimens,
    inpatient_data$micro$isolates,
    inpatient_data$micro$susceptibilities,
    overwrite = TRUE
  ))
  
  test_output <- tbl(conSQLite, "drug_prescriptions") %>% 
    filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", "89ac870bc1c1e4b2a37cec79d188cb08")) %>% 
    select(prescription_id, combination_id, therapy_id) %>% 
    collect()

  expect_equivalent(
    test_output, 
    tibble(prescription_id = c("592a738e4c2afcae6f625c01856151e0", "89ac870bc1c1e4b2a37cec79d188cb08"),
           combination_id = c(NA_character_, "0bf9ea7732dd6e904ab670a407382d95"),
           therapy_id = c("592a738e4c2afcae6f625c01856151e0", "0bf9ea7732dd6e904ab670a407382d95")))
  
  
  # > date and datetime casting on SQLite -------------------------------------

  test_sqlite_date <- tbl(conSQLite, "inpatient_episodes") %>% 
    dplyr::filter(patient_id == "99999999999") %>% 
    Ramses:::.sqlite_date_collect( )
  
  expect_is(test_sqlite_date$spell_id, "character")
  expect_is(test_sqlite_date$admission_date, "POSIXt")
  expect_equal(test_sqlite_date$date_of_birth[1], as.Date("1926-08-02"))
  expect_equal(test_sqlite_date$date_of_death[1], as.Date(NA))
  
  # > therapy timeline -------------------------------------------------
  
  expect_error(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "I don't exist")
  )
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999"),
    "timevis")
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date1 = "2017-01-01",
                     date2 = "2017-03-01"), 
    "timevis")
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date1 = "2017-01-01"),
    "timevis")
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date2 = "2017-03-01"), 
    "timevis")
  

  # > close connection ----------------------------------------------------
  DBI::dbDisconnect(conSQLite)
  file.remove("test.sqlite")
  
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
    conSQLite,"ramses_test_edges", silent = TRUE) %>% 
    dplyr::select(id, grp) %>% 
    dplyr::arrange(id) %>% 
    dplyr::collect()
  
  expect_equal(test_output,
               test_solution)
  DBI::dbDisconnect(conSQLite)
  file.remove("test.sqlite")  
})

# PostgreSQL --------------------------------------------------------------

# test_that("Ramses on PosgreSQL", {
# conPostgreSQL <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
#                                 user = "user", password = "password", 
#                                 host = "db-postgres", dbname="RamsesDB")
#   test_warehousing(conPostgreSQL, drug_data, overwrite = T)
#   
# })


# MS SQL Server -----------------------------------------------------------


# 
# test_that("Ramses on MS SQL Server", {
#   test_warehousing(conMS, drug_data, overwrite = T)
    # requireNamespace("odbc", quietly = TRUE)
    # conSQLServer <- dbConnect(
    #   odbc::odbc(),
    #   .connection_string = "Driver={ODBC Driver 17 for SQL Server}; Server=db-mssql,1433;Uid=Sa; Pwd=bidultrukCestLeurTruc!;")

# 
# })



