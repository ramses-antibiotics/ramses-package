

# SQLite ------------------------------------------------------------------


test_that("Ramses on SQLite 1", {
  
  # > create_mock_database ----------------------------------------------
  
  if (!identical(Sys.getenv("CI"), "true")) {
    skip("Test only on Travis")
  }

  conSQLite <- create_mock_database(file = "test1.sqlite", silent = TRUE)
  expect_true(is(conSQLite, "SQLiteConnection"))
  test_output <- tbl(conSQLite, "drug_prescriptions") %>% 
    dplyr::filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", 
                                         "89ac870bc1c1e4b2a37cec79d188cb08",
                                         "0bf9ea7732dd6e904ab670a407382d95")) %>% 
    dplyr::select(prescription_id, combination_id, therapy_id) %>% 
    dplyr::arrange(therapy_id, prescription_id) %>% 
    dplyr::collect()
  expect_equivalent(
    test_output, 
    dplyr::tibble(prescription_id = c("592a738e4c2afcae6f625c01856151e0",
                                      "0bf9ea7732dd6e904ab670a407382d95",
                                      "89ac870bc1c1e4b2a37cec79d188cb08"),
                  combination_id = c(NA_character_, 
                                     "0bf9ea7732dd6e904ab670a407382d95", 
                                     "0bf9ea7732dd6e904ab670a407382d95"),
                  therapy_id = c("592a738e4c2afcae6f625c01856151e0", 
                                 "89ac870bc1c1e4b2a37cec79d188cb08", 
                                 "89ac870bc1c1e4b2a37cec79d188cb08")))
  expect_equal(.nrow_sql_table(conSQLite, "ramses_tally"), 20001)
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
  expect_true(validate_inpatient_episodes(inpatient_data$patients,
                                          inpatient_data$episodes))
  expect_true(validate_inpatient_episodes(patients = inpatient_data$patients,
                                          episodes = inpatient_data$episodes,
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
                            patients_data = inpatient_data$patients,
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
    dplyr::filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", 
                                         "89ac870bc1c1e4b2a37cec79d188cb08",
                                         "0bf9ea7732dd6e904ab670a407382d95")) %>% 
    dplyr::select(prescription_id, combination_id, therapy_id) %>% 
    dplyr::arrange(therapy_id, prescription_id) %>% 
    dplyr::collect()
  expect_equivalent(
    test_output, 
    dplyr::tibble(prescription_id = c("592a738e4c2afcae6f625c01856151e0",
                                      "0bf9ea7732dd6e904ab670a407382d95",
                                      "89ac870bc1c1e4b2a37cec79d188cb08"),
                  combination_id = c(NA_character_, 
                                     "0bf9ea7732dd6e904ab670a407382d95", 
                                     "0bf9ea7732dd6e904ab670a407382d95"),
                  therapy_id = c("592a738e4c2afcae6f625c01856151e0", 
                                 "89ac870bc1c1e4b2a37cec79d188cb08", 
                                 "89ac870bc1c1e4b2a37cec79d188cb08")))

  test_output <- tbl(conSQLite, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect() 
  
  expect_equivalent(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      therapy_start = "2016-08-01 11:15:19+01:00",
      therapy_end = "2016-08-03 11:15:19+01:00"
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
      therapy_start = "2016-08-01 11:15:19+01:00",
      therapy_end = "2016-08-03 11:15:19+01:00"
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
  expect_equal(round(sum(test_bridge_th_overlap$LOT), 1), 7.4)
  
  expect_true(bridge_tables(conn = conSQLite, overwrite = TRUE))
  
  # > date and datetime casting on SQLite -------------------------------------

  test_sqlite_date <- tbl(conSQLite, "inpatient_episodes") %>% 
    dplyr::filter(patient_id == "99999999999") %>% 
    Ramses:::collect_ramses_tbl( )
  
  expect_is(test_sqlite_date$spell_id, "character")
  expect_is(test_sqlite_date$admission_date, "POSIXt")
  expect_equal(test_sqlite_date$date_of_birth[1], as.Date("1926-08-02"))
  expect_equal(test_sqlite_date$date_of_death[1], as.Date(NA))
  
  # > TherapyEpisode ------------------------------------------------------------
  
  # Single IVPO change pt 99999999999
  test_episode <- TherapyEpisode(conSQLite, "5528fc41106bb48eb4d48bc412e13e67")
  test_output <- get_therapy_table(test_episode, collect = T)
  test_expected_head <- dplyr::tibble(
    t = 0:5,
    patient_id = "99999999999",
    therapy_id = "5528fc41106bb48eb4d48bc412e13e67",
    therapy_start = as.POSIXct("2015-08-07 10:27:00", tz = "Europe/London"),
    therapy_end = as.POSIXct("2015-08-17 12:20:00", tz = "Europe/London"),
    t_start = as.POSIXct(
      c("2015-08-07 10:27:00", "2015-08-07 11:27:00", "2015-08-07 12:27:00",
        "2015-08-07 13:27:00", "2015-08-07 14:27:00", "2015-08-07 15:27:00"), tz = "Europe/London"),
    t_end = as.POSIXct(
      c("2015-08-07 11:27:00", "2015-08-07 12:27:00", "2015-08-07 13:27:00", 
        "2015-08-07 14:27:00", "2015-08-07 15:27:00", "2015-08-07 16:27:00"), tz = "Europe/London"),
    parenteral = 1L
  )
  test_expected_tail <- dplyr::tibble(
    t = 236:241,
    patient_id = "99999999999",
    therapy_id = "5528fc41106bb48eb4d48bc412e13e67",
    therapy_start = as.POSIXct("2015-08-07 10:27:00", tz = "Europe/London"),
    therapy_end = as.POSIXct("2015-08-17 12:20:00", tz = "Europe/London"),
    t_start = as.POSIXct(
      c("2015-08-17 06:27:00", "2015-08-17 07:27:00", "2015-08-17 08:27:00", 
        "2015-08-17 09:27:00", "2015-08-17 10:27:00", "2015-08-17 11:27:00"), tz = "Europe/London"),
    t_end = as.POSIXct(
      c("2015-08-17 07:27:00", "2015-08-17 08:27:00", "2015-08-17 09:27:00", "2015-08-17 10:27:00", 
        "2015-08-17 11:27:00", "2015-08-17 12:20:00"), tz = "Europe/London"),
    parenteral = 0L
  )
  
  expect_equivalent(head(test_output), test_expected_head)
  expect_equivalent(tail(test_output), test_expected_tail)
  expect_equal(
    sum(difftime(test_output$t_end, test_output$t_start,units =  "hours")),
    structure(241.883333333333, class = "difftime", units = "hours")
  )
  
  test_medication_request <- MedicationRequest(conSQLite, "5528fc41106bb48eb4d48bc412e13e67")
  expect_is(test_medication_request, "MedicationRequest")
  expect_is(TherapyEpisode(test_medication_request), "TherapyEpisode")
  expect_equivalent(head(get_therapy_table(TherapyEpisode(test_medication_request), collect = TRUE)), 
                    test_expected_head)
  expect_equivalent(tail(get_therapy_table(TherapyEpisode(test_medication_request), collect = TRUE)), 
                    test_expected_tail)
  
  
  expect_equal(parenteral_changes_get(TherapyEpisode(test_medication_request)), 
               list(c(1, 242)))
  
  # Three IVPO changes pt 5726385525
  
  single_therapy <- dplyr::collect(dplyr::filter(tbl(conSQLite, "drug_prescriptions"), 
                                                 patient_id == "5726385525"))
  expect_true(all(single_therapy$therapy_id == "a028cf950c29ca73c01803b54642d513"))
  expect_equal(
    parenteral_changes_get(TherapyEpisode(conSQLite, "a028cf950c29ca73c01803b54642d513")),
    list(
      c(1, 145),
      c(147, 317),
      c(319, 391)
    )
  )

  # > therapy timeline -------------------------------------------------
  
  expect_error(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "I don't exist")
  )
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999"),
    "timevis")
  expect_error(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date1 = "2017-01-01",
                     date2 = "2017-03-01")
  )
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date1 = as.Date("2017-01-01"),
                     date2 = as.Date("2017-03-01")), 
    "timevis")
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date1 = as.Date("2017-01-01")),
    "timevis")
  expect_is(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date2 = as.Date("2017-03-01")), 
    "timevis")
  
  # > other consistency checks ----------------------------------------------------
  
  # check that therapy id is the one of the first prescription
  invalid_therapy_ids <- tbl(conSQLite, "drug_prescriptions") %>% 
    dplyr::filter(therapy_rank == 1 & therapy_id != prescription_id) %>% 
    dplyr::collect()
  expect_true(nrow(invalid_therapy_ids) == 0)

  # > close connection ----------------------------------------------------
  DBI::dbDisconnect(conSQLite)
  file.remove("test.sqlite")
  
})


   # > transitive closure ----------------------------------------------------
test_that("SQLite does transitive closure", {
  
  test_edges <- dplyr::tibble(
    id1 = as.integer(c(1,1,2,5,6,7)),
    id2 = as.integer(c(2,3,4,6,7,8))
  )
  test_solution <- dplyr::tibble(
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


# > .format_str_time_sqlite ----------------------------------------------------

test_that(".format_str_time_sqlite", {
  
  conSQLite <- suppressWarnings(connect_local_database("test.sqlite"))
  test_posixct <- dplyr::tibble(t_start = as.POSIXct("2017-07-02 01:15:46", 
                                                     tz = "Europe/London"))
  test_posixct <- .format_str_time_sqlite(conSQLite, test_posixct)
  expect_equal(
    test_posixct,
    dplyr::tibble(t_start = "2017-07-02 01:15:46+01:00")
  )
  dplyr::copy_to(conSQLite, 
                 test_posixct,
                 overwrite = TRUE)
  expect_equal(
    collect_ramses_tbl(tbl(conSQLite, "test_posixct")),
    dplyr::tibble(t_start = as.POSIXct("2017-07-02 01:15:46", tz = "Europe/London"))
  )
  
  DBI::dbDisconnect(conSQLite)
  file.remove("test.sqlite")  
})

# > edge classification  --------------------------------------------------

test_that("SQLite drug_prescriptions_edges", {
  
  emptydatabase <- dbConnect(RSQLite::SQLite(), ":memory:")
  records_rx <- read.csv(system.file("test_cases", "prescription_linkage_prescriptions.csv", 
                                     package = "Ramses"),
                         colClasses = c("character", "character", "numeric", 
                                        "POSIXct", "POSIXct", "POSIXct", "character", "character", 
                                        "character", "character", "character", "character", "character"))
  load_medications(emptydatabase, records_rx, overwrite = T)
  
  output <- dplyr::distinct(tbl(emptydatabase, "drug_prescriptions_edges"), 
                          patient_id, edge_type, relation_type) %>%
    dplyr::arrange(patient_id) %>% 
    dplyr::collect()
  
  records_edges <- read.csv(system.file("test_cases", "prescription_linkage_edges_classes.csv", 
                                        package = "Ramses"),
                            colClasses = c("character", "character")) %>% 
    dplyr::filter(edge_type != "not an edge") %>% 
    dplyr::mutate(relation_type = substr(patient_id, 0, 1)) %>% 
    dplyr::tibble()

  expect_equal(output,  records_edges)
  dbDisconnect(emptydatabase)

})


# PostgreSQL --------------------------------------------------------------


# > transitive closure ----------------------------------------------------

test_that("Postgres does transitive closure", {
  
  if (!identical(Sys.getenv("CI"), "true")) {
    skip("Test only on Travis")
  }
  
  test_edges <- dplyr::tibble(
    id1 = as.integer(c(1,1,2,5,6,7)),
    id2 = as.integer(c(2,3,4,6,7,8))
  )
  test_solution <- dplyr::tibble(
    id =  as.integer(c(1,2,3,4,5,6,7,8)),
    grp = as.integer(c(1,1,1,1,5,5,5,5))
  )
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB")
  
  lapply(DBI::dbListTables(conPostgreSQL), 
         DBI::dbRemoveTable, 
         conn = conPostgreSQL)

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
  
  test_output <- Ramses:::.run_transitive_closure.PqConnection(
    conPostgreSQL,"ramses_test_edges", silent = TRUE) %>% 
    dplyr::select(id, grp) %>% 
    dplyr::arrange(id) %>% 
    dplyr::collect()
  
  expect_equal(test_output,
               test_solution)
  lapply(DBI::dbListTables(conPostgreSQL), 
         DBI::dbRemoveTable, 
         conn = conPostgreSQL)
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
  
  lapply(DBI::dbListTables(conPostgreSQL), 
         DBI::dbRemoveTable, 
         conn = conPostgreSQL)

  drug_data <- Ramses:::.prepare_example_drug_records()
  inpatient_data <- Ramses:::.prepare_example_inpatient_records()
  icd10cm <- download_icd10cm()
  
  expect_invisible(
    load_medications(conn = conPostgreSQL, 
                     prescriptions = drug_data$drug_rx,
                     administrations = drug_data$drug_admins,
                     overwrite = TRUE)
  )
  
  
  expect_invisible(
    load_inpatient_episodes(conn = conPostgreSQL,
                            patients_data = inpatient_data$patients,
                            episodes_data = inpatient_data$episodes,
                            wards_data = inpatient_data$ward_movements,
                            overwrite = TRUE)
  )
  expect_invisible(
    expect_warning(
      load_inpatient_diagnoses(conn = conPostgreSQL,
                               diagnoses_data = inpatient_data$diagnoses,
                               diagnoses_lookup = icd10cm,
                               overwrite = TRUE)))
  expect_invisible(
    load_inpatient_investigations(
      conn = conPostgreSQL,
      investigations_data = inpatient_data$investigations,
      overwrite = TRUE
    ))
  expect_invisible(
    load_inpatient_microbiology(
      conn = conPostgreSQL,
      inpatient_data$micro$specimens,
      inpatient_data$micro$isolates,
      inpatient_data$micro$susceptibilities,
      overwrite = TRUE
    )
  )
  
  test_output <- tbl(conPostgreSQL, "drug_prescriptions") %>% 
    dplyr::filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", 
                                         "89ac870bc1c1e4b2a37cec79d188cb08",
                                         "0bf9ea7732dd6e904ab670a407382d95")) %>% 
    dplyr::select(prescription_id, combination_id, therapy_id) %>% 
    dplyr::arrange(therapy_id, prescription_id) %>% 
    dplyr::collect()
  expect_equivalent(
    test_output, 
    dplyr::tibble(prescription_id = c("592a738e4c2afcae6f625c01856151e0",
                                      "0bf9ea7732dd6e904ab670a407382d95",
                                      "89ac870bc1c1e4b2a37cec79d188cb08"),
                  combination_id = c(NA_character_, 
                                     "0bf9ea7732dd6e904ab670a407382d95", 
                                     "0bf9ea7732dd6e904ab670a407382d95"),
                  therapy_id = c("592a738e4c2afcae6f625c01856151e0", 
                                 "89ac870bc1c1e4b2a37cec79d188cb08", 
                                 "89ac870bc1c1e4b2a37cec79d188cb08")))

  test_output <- tbl(conPostgreSQL, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect()
  
  expect_equivalent(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      therapy_start = as.POSIXct("2016-08-01 11:15:19", tz = "Europe/London"),
      therapy_end = as.POSIXct("2016-08-03 11:15:19", tz = "Europe/London")
    )
  )
  

  # > TherapyEpisode ------------------------------------------------------------

  # Single IVPO change pt 99999999999
  
  test_episode <- TherapyEpisode(conPostgreSQL, "5528fc41106bb48eb4d48bc412e13e67")
  test_output <- get_therapy_table(test_episode, collect = T)
  test_expected_head <- dplyr::tibble(
    t = 0:5,
    patient_id = "99999999999",
    therapy_id = "5528fc41106bb48eb4d48bc412e13e67",
    therapy_start = as.POSIXct("2015-08-07 10:27:00", tz = "Europe/London"),
    therapy_end = as.POSIXct("2015-08-12 12:12:00", tz = "Europe/London"),
    t_start = as.POSIXct(
      c("2015-08-07 10:27:00", "2015-08-07 11:27:00", "2015-08-07 12:27:00",
        "2015-08-07 13:27:00", "2015-08-07 14:27:00", "2015-08-07 15:27:00"), tz = "Europe/London"),
    t_end = as.POSIXct(
      c("2015-08-07 11:27:00", "2015-08-07 12:27:00", "2015-08-07 13:27:00", 
        "2015-08-07 14:27:00", "2015-08-07 15:27:00", "2015-08-07 16:27:00"), tz = "Europe/London"),
    parenteral = 1L
  )
  test_expected_tail <- dplyr::tibble(
    t = 116:121,
    patient_id = "99999999999",
    therapy_id = "5528fc41106bb48eb4d48bc412e13e67",
    therapy_start = as.POSIXct("2015-08-07 10:27:00", tz = "Europe/London"),
    therapy_end = as.POSIXct("2015-08-12 12:12:00", tz = "Europe/London"),
    t_start = as.POSIXct(
      c("2015-08-12 06:27:00", "2015-08-12 07:27:00", "2015-08-12 08:27:00", 
        "2015-08-12 09:27:00", "2015-08-12 10:27:00", "2015-08-12 11:27:00"), tz = "Europe/London"),
    t_end = as.POSIXct(
      c("2015-08-12 07:27:00", "2015-08-12 08:27:00", "2015-08-12 09:27:00", "2015-08-12 10:27:00", 
        "2015-08-12 11:27:00", "2015-08-12 12:12:00"), tz = "Europe/London"),
    parenteral = 0L
  )

  expect_equivalent(head(test_output), test_expected_head)
  expect_equivalent(tail(test_output), test_expected_tail)
  expect_equal(
    sum(difftime(test_output$t_end, test_output$t_start,units =  "hours")),
    structure(121.75, class = "difftime", units = "hours")
  )
  
  test_medication_request <- MedicationRequest(conPostgreSQL, "5528fc41106bb48eb4d48bc412e13e67")
  expect_is(test_medication_request, "MedicationRequest")
  expect_is(TherapyEpisode(test_medication_request), "TherapyEpisode")
  expect_equivalent(head(get_therapy_table(TherapyEpisode(test_medication_request), collect = TRUE)), 
                    test_expected_head)
  expect_equivalent(tail(get_therapy_table(TherapyEpisode(test_medication_request), collect = TRUE)), 
                    test_expected_tail)
  
  # Three IVPO changes pt 5726385525
  
  single_therapy <- dplyr::collect(dplyr::filter(tbl(conSQLite, "drug_prescriptions"), 
                                                 patient_id == "5726385525"))
  expect_true(all(single_therapy$therapy_id == "06bb1a8e680036089f889ec2cf2dc6ee"))
  expect_equal(
    parenteral_changes_get(TherapyEpisode(conSQLite, "06bb1a8e680036089f889ec2cf2dc6ee")),
    list(
      c(1, 145),
      c(147, 317),
      c(319, 391)
    )
  )
  
  # > recreate therapy episodes and combinations --------------------------------
  
  DBI::dbRemoveTable(conPostgreSQL, "drug_prescriptions_edges")
  DBI::dbRemoveTable(conPostgreSQL, "drug_therapy_episodes")
  
  expect_silent(create_therapy_episodes(conPostgreSQL))
  
  test_output <- tbl(conPostgreSQL, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect()

  expect_equivalent(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      therapy_start = as.POSIXct("2016-08-01 11:15:19", tz = "Europe/London"),
      therapy_end = as.POSIXct("2016-08-03 11:15:19", tz = "Europe/London")
    )
  )
  
  # > other database functions --------------------------------------------
  
  # bridge_episode_prescription_overlap
  expect_true(bridge_episode_prescription_overlap(conPostgreSQL))
  expect_error(bridge_episode_prescription_overlap(conPostgreSQL))
  expect_true(bridge_episode_prescription_overlap(conPostgreSQL, overwrite = TRUE))
  test_bridge_overlap <- tbl(
    conPostgreSQL,
    "bridge_episode_prescription_overlap") %>% 
    dplyr::filter(patient_id == "99999999999" & 
                    prescription_id == "89094c5dffaad0e56073adaddf286e73") %>% 
    dplyr::collect()
  expect_equal(round(sum(test_bridge_overlap$DOT), 1), 2.0)
  expect_equal(round(sum(test_bridge_overlap$DDD_prescribed), 1), 1.3)
  
  # bridge_episode_prescription_initiation
  expect_true(bridge_episode_prescription_initiation(conPostgreSQL))
  expect_error(bridge_episode_prescription_initiation(conPostgreSQL))
  expect_true(bridge_episode_prescription_initiation(conPostgreSQL, overwrite = TRUE))
  test_bridge_init <- tbl(conPostgreSQL, "bridge_episode_prescription_initiation") %>% 
    dplyr::filter(patient_id == "99999999999" & 
                    prescription_id == "89094c5dffaad0e56073adaddf286e73") %>% 
    dplyr::collect()
  expect_equal(round(test_bridge_init$DOT, 1), 2.0)
  expect_equal(round(test_bridge_init$DDD_prescribed, 1), 1.3)
  
  # bridge_spell_therapy_overlap
  expect_true(bridge_spell_therapy_overlap(conPostgreSQL))
  expect_error(bridge_spell_therapy_overlap(conPostgreSQL))
  expect_true(bridge_spell_therapy_overlap(conPostgreSQL, overwrite = TRUE))
  test_bridge_th_overlap <- tbl(
    conPostgreSQL,
    "bridge_spell_therapy_overlap") %>% 
    dplyr::filter(patient_id == "99999999999" &
                    therapy_id == "4d611fc8886c23ab047ad5f74e5080d7") %>% 
    dplyr::collect()
  expect_equal(round(sum(test_bridge_th_overlap$LOT), 1), 7.4)
  
  expect_true(bridge_tables(conn = conPostgreSQL, overwrite = TRUE))
  
  # > therapy timeline -------------------------------------------------
  
  expect_error(
    therapy_timeline(conn = conPostgreSQL, 
                     patient_identifier =  "I don't exist")
  )
  expect_is(
    therapy_timeline(conn = conPostgreSQL, 
                     patient_identifier =  "99999999999"),
    "timevis")
  expect_error(
    therapy_timeline(conn = conSQLite, 
                     patient_identifier =  "99999999999",
                     date1 = "2017-01-01",
                     date2 = "2017-03-01")
  )
  expect_is(
    therapy_timeline(conn = conPostgreSQL, 
                     patient_identifier =  "99999999999",
                     date1 = as.Date("2017-01-01"),
                     date2 = as.Date("2017-03-01")), 
    "timevis")
  expect_is(
    therapy_timeline(conn = conPostgreSQL, 
                     patient_identifier =  "99999999999",
                     date1 = as.Date("2017-01-01")),
    "timevis")
  expect_is(
    therapy_timeline(conn = conPostgreSQL, 
                     patient_identifier =  "99999999999",
                     date2 = as.Date("2017-03-01")), 
    "timevis")
  
  # > other consistency checks ----------------------------------------------------
  
  # check that therapy id is the one of the first prescription
  invalid_therapy_ids <- tbl(conPostgreSQL, "drug_prescriptions") %>% 
    dplyr::filter(therapy_rank == 1 & therapy_id != prescription_id) %>% 
    dplyr::collect()
  expect_true(nrow(invalid_therapy_ids) == 0)
  
  # > close connection ----------------------------------------------------
  
  Ramses:::.remove_db_tables(conPostgreSQL, DBI::dbListTables(conPostgreSQL))
  
  DBI::dbDisconnect(conPostgreSQL)
})

# > edge classification  --------------------------------------------------

test_that("Postgres drug_prescriptions_edges", {
  
  if (!identical(Sys.getenv("CI"), "true")) {
    skip("Test only on Travis")
  }
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB")
  
  lapply(DBI::dbListTables(conPostgreSQL), 
         DBI::dbRemoveTable, 
         conn = conPostgreSQL)
  
  records_rx <- read.csv(system.file("test_cases", "prescription_linkage_prescriptions.csv", 
                                     package = "Ramses"),
                         colClasses = c("character", "character", "numeric", 
                                        "POSIXct", "POSIXct", "POSIXct", "character", "character", 
                                        "character", "character", "character", "character", "character"))
  load_medications(conPostgreSQL, records_rx, overwrite = T)
  
  output <- dplyr::distinct(tbl(conPostgreSQL, "drug_prescriptions_edges"), 
                            patient_id, edge_type, relation_type) %>% 
    dplyr::arrange(patient_id) %>% 
    dplyr::collect()
  
  records_edges <- read.csv(system.file("test_cases", "prescription_linkage_edges_classes.csv", 
                                        package = "Ramses"),
                            colClasses = c("character", "character")) %>% 
    dplyr::filter(edge_type != "not an edge") %>% 
    dplyr::mutate(relation_type = substr(patient_id, 0, 1)) %>% 
    dplyr::tibble()
  
  expect_equal(output,  records_edges)
  
  lapply(DBI::dbListTables(conPostgreSQL), 
         DBI::dbRemoveTable, 
         conn = conPostgreSQL)
  
  DBI::dbDisconnect(conPostgreSQL)
})
