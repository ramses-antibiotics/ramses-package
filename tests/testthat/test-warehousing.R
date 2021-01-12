

# SQLite ------------------------------------------------------------------


test_that("Ramses on SQLite 1", {
  
  # > create_mock_database ----------------------------------------------
  
  if (!identical(Sys.getenv("CI"), "true")) {
    skip("Test only on Travis")
  }

  conSQLite <- create_mock_database(file = "test1.sqlite", silent = TRUE)
  expect_true(is(conSQLite, "SQLiteConnection"))
  test_output <- tbl(conSQLite, "drug_prescriptions") %>% 
    filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", 
                                  "89ac870bc1c1e4b2a37cec79d188cb08")) %>% 
    select(prescription_id, combination_id, therapy_id) %>% 
    collect()
  expect_equivalent(
    test_output, 
    tibble(prescription_id = c("592a738e4c2afcae6f625c01856151e0",
                               "89ac870bc1c1e4b2a37cec79d188cb08"),
           combination_id = c(NA_character_, "0bf9ea7732dd6e904ab670a407382d95"),
           therapy_id = c("592a738e4c2afcae6f625c01856151e0", 
                          "0bf9ea7732dd6e904ab670a407382d95")))
  expect_equal(.nrow_sql_table(conSQLite, "ramses_tally"), 20000)
  dbDisconnect(conSQLite)
  file.remove("test1.sqlite")
  rm(conSQLite)
})


test_that("Ramses on SQLite 2", {
  
  if (!identical(Sys.getenv("CI"), "true")) {
    skip("Test only on Travis")
  }
  
  # > validate functions --------------------------------------------------
  
  drug_data <- Ramses:::.prepare_example_drug_records()
  inpatient_data <- Ramses:::.prepare_example_inpatient_records()
  icd10cm <- download_icd10cm()
  conSQLite <- suppressWarnings(connect_local_database("test.sqlite"))
  
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
  
  # > database loading functions ------------------------------------------
  
  expect_invisible(
    load_medications(conn = conSQLite, 
                     prescriptions = drug_data$drug_rx,
                     administrations = drug_data$drug_admins,
                     overwrite = TRUE)
  )

  expect_invisible(
    load_inpatient_episodes(conn = conSQLite,
                            episodes_data = inpatient_data$episodes,
                            wards_data = inpatient_data$ward_movements,
                            overwrite = TRUE)
  )
  expect_invisible(
    expect_warning(
      load_inpatient_diagnoses(conn = conSQLite,
                           diagnoses_data = inpatient_data$diagnoses,
                           diagnoses_lookup = icd10cm,
                           overwrite = TRUE)))
  expect_invisible(
    load_inpatient_investigations(
      conn = conSQLite,
      investigations_data = inpatient_data$investigations,
      overwrite = TRUE
    ))
  expect_invisible(
    load_inpatient_microbiology(
      conn = conSQLite,
      inpatient_data$micro$specimens,
      inpatient_data$micro$isolates,
      inpatient_data$micro$susceptibilities,
      overwrite = TRUE
    )
  )
  
  test_output <- tbl(conSQLite, "drug_prescriptions") %>% 
    filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", "89ac870bc1c1e4b2a37cec79d188cb08")) %>% 
    select(prescription_id, combination_id, therapy_id) %>% 
    collect()

  expect_equivalent(
    test_output, 
    tibble(prescription_id = c("592a738e4c2afcae6f625c01856151e0", "89ac870bc1c1e4b2a37cec79d188cb08"),
           combination_id = c(NA_character_, "0bf9ea7732dd6e904ab670a407382d95"),
           therapy_id = c("592a738e4c2afcae6f625c01856151e0", "0bf9ea7732dd6e904ab670a407382d95")))
  
  test_output <- tbl(conSQLite, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect() 
  
  expect_equivalent(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      therapy_start = "2016-08-01 11:15:19",
      therapy_end = "2016-08-03 11:15:19"
    )
  )

# > recreate therapy episodes and combinations --------------------------------

  
  DBI::dbRemoveTable(conSQLite, "drug_prescriptions_edges")
  DBI::dbRemoveTable(conSQLite, "drug_therapy_episodes")
  
  expect_silent(create_therapy_episodes(conSQLite))
  
  test_output <- tbl(conSQLite, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect()
  
  expect_equivalent(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      therapy_start = "2016-08-01 11:15:19",
      therapy_end = "2016-08-03 11:15:19"
    )
    )
  
  # > other database functions --------------------------------------------
  
     # bridge_episode_prescription_overlap
  expect_true(bridge_episode_prescription_overlap(conSQLite))
  expect_error(bridge_episode_prescription_overlap(conSQLite))
  expect_true(bridge_episode_prescription_overlap(conSQLite, overwrite = TRUE))
  test_bridge_overlap <- tbl(
    conSQLite,
    "bridge_episode_prescription_overlap") %>% 
    dplyr::filter(patient_id == "99999999999" & 
                    prescription_id == "89094c5dffaad0e56073adaddf286e73") %>% 
    dplyr::collect()
  expect_equal(round(sum(test_bridge_overlap$DOT), 1), 2.0)
  expect_equal(round(sum(test_bridge_overlap$DDD_prescribed), 1), 1.3)
  
    # bridge_episode_prescription_initiation
  expect_true(bridge_episode_prescription_initiation(conSQLite))
  expect_error(bridge_episode_prescription_initiation(conSQLite))
  expect_true(bridge_episode_prescription_initiation(conSQLite, overwrite = TRUE))
  test_bridge_init <- tbl(conSQLite, "bridge_episode_prescription_initiation") %>% 
    dplyr::filter(patient_id == "99999999999" & 
                    prescription_id == "89094c5dffaad0e56073adaddf286e73") %>% 
    dplyr::collect()
  expect_equal(round(test_bridge_init$DOT, 1), 2.0)
  expect_equal(round(test_bridge_init$DDD_prescribed, 1), 1.3)
  
  # bridge_spell_therapy_overlap
  expect_true(bridge_spell_therapy_overlap(conSQLite))
  expect_error(bridge_spell_therapy_overlap(conSQLite))
  expect_true(bridge_spell_therapy_overlap(conSQLite, overwrite = TRUE))
  test_bridge_th_overlap <- tbl(
    conSQLite,
    "bridge_spell_therapy_overlap") %>% 
    dplyr::filter(patient_id == "99999999999" &
                    therapy_id == "4d611fc8886c23ab047ad5f74e5080d7") %>% 
    dplyr::collect()
  expect_equal(round(sum(test_bridge_th_overlap$LOT), 1), 2.3)
  
  expect_true(bridge_tables(conn = conSQLite, overwrite = TRUE))
  
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


   # > transitive closure ----------------------------------------------------
test_that("SQLite does transitive closure", {
  
  test_edges <- tibble(
    id1 = as.integer(c(1,1,2,5,6,7)),
    id2 = as.integer(c(2,3,4,6,7,8))
  )
  test_solution <- tibble(
    id =  as.integer(c(1,2,3,4,5,6,7,8)),
    grp = as.integer(c(1,1,1,1,5,5,5,5))
  )
  
  conSQLite <- suppressWarnings(
    connect_local_database("test.sqlite"))
  
  dplyr::copy_to(
    dest = conSQLite,
    name = "ramses_test_edges",
    df = test_edges,
    temporary = FALSE,
    overwrite = TRUE)
  
  test_output <- Ramses:::.run_transitive_closure(
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


# > transitive closure ----------------------------------------------------

test_that("Postgres does transitive closure", {
  
  if (!identical(Sys.getenv("CI"), "true")) {
    skip("Test only on Travis")
  }
  
  test_edges <- tibble(
    id1 = as.integer(c(1,1,2,5,6,7)),
    id2 = as.integer(c(2,3,4,6,7,8))
  )
  test_solution <- tibble(
    id =  as.integer(c(1,2,3,4,5,6,7,8)),
    grp = as.integer(c(1,1,1,1,5,5,5,5))
  )
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB")
  
  dplyr::copy_to(
    dest = conPostgreSQL,
    name = "ramses_test_edges",
    df = test_edges,
    temporary = FALSE,
    overwrite = TRUE) 
  
  expect_true(
    is(tbl(conPostgreSQL,
           "ramses_test_edges"),
      "tbl_PqConnection")
  )
  expect_equivalent(
    dplyr::collect(dplyr::tbl(conPostgreSQL, "ramses_test_edges")),
    test_edges
  )
  
  test_output <- Ramses:::.run_transitive_closure(
    conPostgreSQL,"ramses_test_edges", silent = TRUE) %>% 
    dplyr::select(id, grp) %>% 
    dplyr::arrange(id) %>% 
    dplyr::collect()
  
  expect_equal(test_output,
               test_solution)
  DBI::dbRemoveTable(conPostgreSQL, "ramses_tc_group")
  DBI::dbDisconnect(conPostgreSQL)
})


test_that("Ramses on PosgreSQL", {
  
  if (!identical(Sys.getenv("CI"), "true")) {
    skip("Test only on Travis")
  }

  # > database loading functions ------------------------------------------
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB")

  drug_data <- Ramses:::.prepare_example_drug_records()
  expect_invisible(
    load_medications(conn = conPostgreSQL, 
                     prescriptions = drug_data$drug_rx,
                     administrations = drug_data$drug_admins,
                     overwrite = TRUE)
  )
  
  
  test_output <- tbl(conPostgreSQL, "drug_prescriptions") %>% 
    filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", "89ac870bc1c1e4b2a37cec79d188cb08")) %>% 
    select(prescription_id, combination_id, therapy_id) %>% 
    collect()
  
  expect_equivalent(
    test_output, 
    tibble(prescription_id = c("592a738e4c2afcae6f625c01856151e0", "89ac870bc1c1e4b2a37cec79d188cb08"),
           combination_id = c(NA_character_, "0bf9ea7732dd6e904ab670a407382d95"),
           therapy_id = c("592a738e4c2afcae6f625c01856151e0", "0bf9ea7732dd6e904ab670a407382d95")))
  
  test_output <- tbl(conPostgreSQL, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect() 
  
  expect_equivalent(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      therapy_start = "2016-08-01 11:15:19",
      therapy_end = "2016-08-03 11:15:19"
    )
  )
  
  DBI::dbDisconnect(conPostgreSQL)
})
