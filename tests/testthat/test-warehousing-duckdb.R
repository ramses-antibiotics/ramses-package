
# DuckDB ------------------------------------------------------------------

test_that(".create_sql_primary_key on DuckDB", {
  db_conn <- connect_local_database(file = "test.duckdb", timezone = "Europe/London")
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  DBI::dbWriteTable(conn = db_conn, name = "test_table", value = data.frame(key = 1:10) )
  DBI::dbListObjects(db_conn)
  .create_sql_primary_key(conn = db_conn, field = "key", table = "test_table")
  expect_error(
    DBI::dbWriteTable(
      conn = db_conn, name = "test_table", 
      value = data.frame(key = 1:10), append = TRUE
    )
  )
})

test_that(".create_sql_index on DuckDB", {
  db_conn <- connect_local_database(file = "test.duckdb")
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  DBI::dbWriteTable(conn = db_conn, name = "test_table", value = data.frame(key = 1:10) )
  .create_sql_index(conn = db_conn, field = "key", table = "test_table")
  expect_equal(
    DBI::dbGetQuery(db_conn, "SELECT * FROM pg_catalog.pg_indexes") %>% 
      dplyr::filter(.data$tablename == "test_table") %>% 
      dplyr::select("tablename", "indexname") %>% 
      dplyr::collect(),
    data.frame(
      tablename = "test_table",
      indexname = "idx_test_table_key",
      stringsAsFactors = FALSE
    )
  )
})

test_that(".build_tally_table on DuckDB", {
  db_conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = "test.duckdb")
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  .build_tally_table(db_conn)
  expect_equal(
    DBI::dbReadTable(db_conn, "ramses_tally"),
    data.frame(t = 0:50000)
  )
})

test_that(".update_date_dimension on DuckDB", {
  db_conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = "test.duckdb")
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  expected_df <- dplyr::tibble(
    date = structure(16242, class = "Date"), 
    date_string_iso = "2014-06-21", 
    date_string_dd_mm_yyyy = "21/06/2014", 
    date_string_dd_mm_yy = "21/06/14", 
    date_string_full = "21 June 2014", 
    calendar_year = 2014, 
    calendar_quarter = "Q2",
    calendar_month = 6, 
    calendar_month_name = "June", 
    calendar_month_short = "Jun", 
    day = 21, 
    day_name = "Saturday",
    day_name_short = "Sat", 
    week_day_numeric = 6, 
    week_starting = "2014-06-16", 
    week_ending = "2014-06-22",
    financial_year_uk = "2014/15",
    financial_quarter_uk = "Q1",
    financial_year_quarter_uk = "2014/15 Q1"
  )
  
  .update_date_dimension(db_conn, as.Date("2014-06-21"), as.Date("2014-06-21"))
  expect_true(DBI::dbExistsTable(db_conn, "reference_dimension_date"))
  expect_equal(
    dplyr::collect(dplyr::tbl(db_conn, "reference_dimension_date")),
    expected_df
  )
  
  # Test robustness against violating primary key constraint
  .update_date_dimension(db_conn, as.Date("2014-06-21"), as.Date("2014-06-21"))
  expect_equal(
    dplyr::collect(dplyr::tbl(db_conn, "reference_dimension_date")),
    expected_df
  )
})

test_that(".run_transitive_closure on DuckDB", {
  
  test_edges <- dplyr::tibble(
    from_id = as.integer(c(1,1,2,5,6,7)),
    to_id = as.integer(c(2,3,4,6,7,8))
  )
  test_solution <- dplyr::tibble(
    id =  as.integer(c(1,2,3,4,5,6,7,8)),
    grp = as.integer(c(1,1,1,1,2,2,2,2))
  )
  
  db_conn <- suppressWarnings(connect_local_database("test.duckdb"))
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  dplyr::copy_to(
    dest = db_conn,
    name = "ramses_test_edges",
    df = test_edges,
    temporary = FALSE,
    overwrite = TRUE)
  
  test_output <- tbl(db_conn,"ramses_test_edges") %>% 
    Ramses:::.run_transitive_closure() %>% 
    dplyr::collect()
  
  expect_equal(test_output,
               test_solution) 
})

test_that(".tbl_add_demographics on DuckDB", {
  
  db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
  })
  DBI::dbWriteTable(conn = db_conn,
                    name = "bad_table", 
                    value = data.frame(nokey = 1L))
  DBI::dbWriteTable(conn = db_conn,
                    name = "good_table", 
                    value = data.frame(patient_id = 1L,
                                       variable = "a"))
  
  expect_error(.tbl_add_demographics(data.frame(not_remote_tbl = 1)))
  expect_error(.tbl_add_demographics(dplyr::tbl(db_conn, "bad_table")))
  expect_equal(dplyr::collect(.tbl_add_demographics(dplyr::tbl(db_conn, "good_table"))),
               dplyr::collect(dplyr::tbl(db_conn, "good_table")))
  
  # Now add demographics to use the full function
  DBI::dbWriteTable(conn = db_conn,
                    name = "patients", 
                    value = data.frame(patient_id = 1L,
                                       sex = 1L))
  expect_equal(dplyr::collect(.tbl_add_demographics(dplyr::tbl(db_conn, "good_table"))),
               dplyr::tibble(patient_id = 1L,
                             variable = "a",
                             sex = 1L))
  DBI::dbWriteTable(conn = db_conn,
                    name = "patients", 
                    value = data.frame(patient_id = 1L,
                                       date_of_birth = as.Date("1957-03-25")), 
                    overwrite = TRUE)
  expect_equal(dplyr::collect(.tbl_add_demographics(dplyr::tbl(db_conn, "good_table"))),
               dplyr::tibble(patient_id = 1L,
                             variable = "a",
                             date_of_birth = as.Date("1957-03-25")))
  DBI::dbWriteTable(conn = db_conn,
                    name = "patients", 
                    value = data.frame(patient_id = 1L,
                                       sex = 1L,
                                       date_of_birth = as.Date("1957-03-25"),
                                       ethnic_category_UK = "A"), 
                    overwrite = TRUE)
  expect_equal(dplyr::collect(.tbl_add_demographics(dplyr::tbl(db_conn, "good_table"))),
               dplyr::tibble(patient_id = 1L,
                             variable = "a",
                             date_of_birth = as.Date("1957-03-25"),
                             sex = 1L,
                             ethnic_category_UK = "A"))
  
})

test_that("drug_prescriptions_edges on DuckDB", {
  
  db_conn <- suppressWarnings(connect_local_database("test.duckdb"))
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  records_rx <- read.csv(system.file("test_cases", "prescription_linkage_prescriptions.csv", 
                                     package = "Ramses"),
                         colClasses = c("character", "character", "numeric", 
                                        "POSIXct", "POSIXct", "POSIXct", "character", "character", 
                                        "character", "character", "character", "character", "character"))
  load_medications(db_conn, records_rx, overwrite = T)
  
  output <- dplyr::distinct(tbl(db_conn, "drug_prescriptions_edges"), 
                            .data$patient_id, .data$edge_type, .data$relation_type) %>%
    dplyr::arrange(.data$patient_id) %>% 
    dplyr::collect()
  
  records_edges <- read.csv(system.file("test_cases", "prescription_linkage_edges_classes.csv", 
                                        package = "Ramses"),
                            colClasses = c("character", "character")) %>% 
    dplyr::filter(edge_type != "not an edge") %>% 
    dplyr::mutate(relation_type = substr(patient_id, 0, 1)) %>% 
    dplyr::tibble()
  
  expect_equal(output,  records_edges)
})

test_that("create_mock_database on DuckDB", {

  db_conn <- create_mock_database(file = "test.duckdb", silent = TRUE)
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  expect_true(is(db_conn, "duckdb_connection"))

  test_output <- tbl(db_conn, "drug_prescriptions") %>% 
    dplyr::filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", 
                                         "89ac870bc1c1e4b2a37cec79d188cb08",
                                         "0bf9ea7732dd6e904ab670a407382d95")) %>% 
    dplyr::select("prescription_id", "combination_id", "therapy_id") %>% 
    dplyr::arrange(.data$therapy_id, .data$prescription_id) %>% 
    dplyr::collect()
  expect_equal(
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
  expect_equal(.nrow_sql_table(db_conn, "ramses_tally"), 50001)
})


test_that("Ramses on DuckDB (system test)", {
  
  db_conn <- suppressWarnings(connect_local_database("test.duckdb", timezone = "Europe/London"))
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb")
  })
  
  # database loading functions ------------------------------------------
  
  expect_invisible(
    load_medications(conn = db_conn, 
                     prescriptions = .ramses_mock_dataset$drug_rx,
                     administrations = .ramses_mock_dataset$drug_admins,
                     overwrite = TRUE)
  )

  test_tables <- DBI::dbGetQuery(db_conn, "SELECT * FROM information_schema.tables") %>% 
    dplyr::filter(table_type == "BASE TABLE" &
                    table_schema == "main") %>%
    dplyr::collect()
  expect_equal(
    sort(test_tables$table_name),
    c("drug_administrations", "drug_prescriptions", 
      "drug_prescriptions_edges", "drug_therapy_episodes", 
      "ramses_tally")
  )
  
  expect_invisible(
    load_inpatient_episodes(conn = db_conn,
                            patients_data = .ramses_mock_dataset$patients,
                            episodes_data = .ramses_mock_dataset$episodes,
                            wards_data = inpatient_wards,
                            overwrite = TRUE)
  )
  expect_invisible(
    expect_warning(
      load_inpatient_diagnoses(conn = db_conn,
                           diagnoses_data = .ramses_mock_dataset$diagnoses,
                           diagnoses_lookup = .ramses_mock_dataset$icd10cm_2020,
                           overwrite = TRUE)))
  expect_true(
    all(
      c("reference_icd",
        "reference_icd_comorbidity",
        "reference_icd_infections",
        "reference_icd_ccs",
        "reference_icd_ccsr") %in%
      DBI::dbListTables(db_conn)
    )
  )
  expect_invisible(
    load_inpatient_investigations(
      conn = db_conn,
      investigations_data = inpatient_investigations,
      overwrite = TRUE
    ))
  expect_invisible(
    load_inpatient_microbiology(
      conn = db_conn,
      .ramses_mock_dataset$micro$specimens,
      .ramses_mock_dataset$micro$isolates,
      .ramses_mock_dataset$micro$susceptibilities,
      overwrite = TRUE
    )
  )
  test_therapy_rank <- tbl(db_conn, "drug_prescriptions") %>% 
    dplyr::filter(!.data$prescription_status %in% c('unknown', 'cancelled', 'draft', 'entered-in-error')) %>% 
    dplyr::distinct(.data$therapy_rank) %>% 
    dplyr::collect()
  expect_true(
    all(test_therapy_rank$therapy_rank > 0)
  )
  test_output <- tbl(db_conn, "drug_prescriptions") %>% 
    dplyr::filter(prescription_id %in% c("592a738e4c2afcae6f625c01856151e0", 
                                         "89ac870bc1c1e4b2a37cec79d188cb08",
                                         "0bf9ea7732dd6e904ab670a407382d95")) %>% 
    dplyr::select("prescription_id", "combination_id", "therapy_id") %>% 
    dplyr::arrange(.data$therapy_id, .data$prescription_id) %>% 
    dplyr::collect()
  expect_equal(
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

  test_output <- tbl(db_conn, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect() 
  
  expect_equal(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      antiinfective_type = "antibacterial",
      therapy_start = as.POSIXct("2016-08-01 11:15:19", tz = "Europe/London", origin = "1960-01-01"),
      therapy_end = as.POSIXct("2016-08-03 11:15:19", tz = "Europe/London", origin = "1960-01-01")
    )
  )
  
  # recreate therapy episodes and combinations --------------------------------
  
  DBI::dbRemoveTable(db_conn, "drug_prescriptions_edges")
  DBI::dbRemoveTable(db_conn, "drug_therapy_episodes")
  
  expect_silent(create_therapy_episodes(db_conn, silent = TRUE))
  
  test_output <- tbl(db_conn, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == "592a738e4c2afcae6f625c01856151e0") %>% 
    dplyr::collect()
  
  expect_equal(
    test_output, 
    dplyr::tibble(
      patient_id = "1555756339",
      therapy_id = "592a738e4c2afcae6f625c01856151e0",
      antiinfective_type = "antibacterial",
      therapy_start = as.POSIXct("2016-08-01 11:15:19", tz = "Europe/London", origin = "1960-01-01"),
      therapy_end = as.POSIXct("2016-08-03 11:15:19", tz = "Europe/London", origin = "1960-01-01")
    )
  )
  
  # other database functions --------------------------------------------
  
  # bridge_episode_prescription_overlap
  expect_true(bridge_episode_prescription_overlap(db_conn))
  expect_error(bridge_episode_prescription_overlap(db_conn))
  expect_true(bridge_episode_prescription_overlap(db_conn, overwrite = TRUE))
  test_bridge_overlap <- tbl(
    db_conn,
    "bridge_episode_prescription_overlap") %>% 
    dplyr::filter(patient_id == "99999999999" & 
                    prescription_id == "89094c5dffaad0e56073adaddf286e73") %>% 
    dplyr::collect()
  expect_equal(round(sum(test_bridge_overlap$DOT), 1), 2.0)
  expect_equal(round(sum(test_bridge_overlap$DDD_prescribed), 1), 1.3)
  
  # bridge_episode_prescription_initiation
  expect_true(bridge_episode_prescription_initiation(db_conn))
  expect_error(bridge_episode_prescription_initiation(db_conn))
  expect_true(bridge_episode_prescription_initiation(db_conn, overwrite = TRUE))
  test_bridge_init <- tbl(db_conn, "bridge_episode_prescription_initiation") %>% 
    dplyr::filter(patient_id == "99999999999" & 
                    prescription_id == "89094c5dffaad0e56073adaddf286e73") %>% 
    dplyr::collect()
  expect_equal(round(test_bridge_init$DOT, 1), 2.0)
  expect_equal(round(test_bridge_init$DDD_prescribed, 1), 1.3)
  
  # bridge_encounter_therapy_overlap
  expect_true(bridge_encounter_therapy_overlap(db_conn))
  expect_error(bridge_encounter_therapy_overlap(db_conn))
  expect_true(bridge_encounter_therapy_overlap(db_conn, overwrite = TRUE))
  test_bridge_th_overlap <- tbl(
    db_conn,
    "bridge_encounter_therapy_overlap") %>% 
    dplyr::filter(patient_id == "99999999999" &
                    therapy_id == "4d611fc8886c23ab047ad5f74e5080d7") %>% 
    dplyr::collect()
  expect_equal(round(sum(test_bridge_th_overlap$LOT), 1), 7.4)
  
  expect_true(bridge_tables(conn = db_conn, overwrite = TRUE))
  
  # date and datetime casting -----------------------------------------------
  
  test_date <- tbl(db_conn, "inpatient_episodes") %>% 
    dplyr::filter(patient_id == "99999999999") %>% 
    dplyr::collect( )
  
  expect_is(test_date$encounter_id, "character")
  expect_is(test_date$admission_date, "POSIXt")
  expect_equal(test_date$date_of_birth[1], as.Date("1926-08-02"))
  expect_equal(test_date$date_of_death[1], as.Date(NA))
  
  # TherapyEpisode ------------------------------------------------------------
  
  # Single IVPO change pt 99999999999
  test_episode <- TherapyEpisode(db_conn, "5528fc41106bb48eb4d48bc412e13e67")
  test_output <- longitudinal_table(test_episode, collect = T)
  test_expected_head <- dplyr::tibble(
    t = 0:5,
    patient_id = "99999999999",
    therapy_id = "5528fc41106bb48eb4d48bc412e13e67",
    therapy_start = structure(1438939620, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    therapy_end = structure(1439810400, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_start = structure(1438939620 + 0:5 * 3600, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_end = structure(c(1438939620 + 1:6 * 3600), tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    parenteral = 1L
  )
  test_expected_tail <- dplyr::tibble(
    t = 236:241,
    patient_id = "99999999999",
    therapy_id = "5528fc41106bb48eb4d48bc412e13e67",
    therapy_start = structure(1438939620, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    therapy_end = structure(1439810400, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    t_start = structure(1438939620 + 236:241 * 3600, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    t_end = structure(c(1438939620 + 237:241 * 3600, 1439810400), tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    parenteral = 0L
  )
  
  expect_equal(head(test_output), test_expected_head)
  expect_equal(tail(test_output), test_expected_tail)
  expect_equal(
    sum(difftime(test_output$t_end, test_output$t_start,units = "hours")),
    structure(241.883333333333, class = "difftime", units = "hours")
  )
  
  test_episode_extended <- TherapyEpisode(db_conn, "5528fc41106bb48eb4d48bc412e13e67",
                                 extend_table_start = 2)
  test_output_extended <- longitudinal_table(test_episode_extended, collect = T)
  
  test_expected_head_extended <- dplyr::tibble(
    t = -2:3,
    patient_id = "99999999999",
    therapy_id = "5528fc41106bb48eb4d48bc412e13e67",
    therapy_start = structure(1438939620, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    therapy_end = structure(1439810400, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_start = structure(1438939620 + -2:3 * 3600, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_end = structure(c(1438939620 + -1:4 * 3600), tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    parenteral = c(NA, 1L, 1L, 1L, 1L, 1L)
  )
  expect_equal(head(test_output_extended), test_expected_head_extended)
  expect_equal(tail(test_output_extended), test_expected_tail)
  expect_equal(
    sum(difftime(test_output_extended$t_end, test_output_extended$t_start, units = "hours")),
    structure(241.883333333333 + 2, class = "difftime", units = "hours") 
  )
  
  # TherapyEpisode() method for MedicationRequest object
  
  test_medication_request <- MedicationRequest(db_conn, "5528fc41106bb48eb4d48bc412e13e67")
  expect_is(test_medication_request, "MedicationRequest")
  expect_is(TherapyEpisode(test_medication_request), "TherapyEpisode")
  expect_equal(head(longitudinal_table(TherapyEpisode(test_medication_request), collect = TRUE)), 
                    test_expected_head)
  expect_equal(tail(longitudinal_table(TherapyEpisode(test_medication_request), collect = TRUE)), 
                    test_expected_tail)
  expect_equal(head(longitudinal_table(test_medication_request, collect = TRUE)), 
                    test_expected_head)
  expect_equal(tail(longitudinal_table(test_medication_request, collect = TRUE)), 
                    test_expected_tail)
  
  # 2+ TherapyEpisode -------------------------------------------------------
  
  test_episode <- TherapyEpisode(conn = db_conn, 
                                 id = c("f770855cf9d424c76fdfbc9786d508ac", 
                                        "5528fc41106bb48eb4d48bc412e13e67"))
  expect_is(test_episode, "TherapyEpisode")
  
  test_expected_tail_second_therapy_episode <- dplyr::tibble(
    t = 117:122, 
    patient_id = "8258333156", 
    therapy_id = "f770855cf9d424c76fdfbc9786d508ac", 
    therapy_start = structure(1444239793, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    therapy_end = structure(1444681333, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_start = structure(1444239793 + 117:122 * 3600, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_end = structure(c(1444239793 + 118:122 * 3600, 1444681333), tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    parenteral = 0L
  )
  expect_equal(head(longitudinal_table(test_episode, collect = TRUE)), 
                    test_expected_head)
  
  expect_equal(tail(longitudinal_table(test_episode, collect = TRUE)), 
                    test_expected_tail_second_therapy_episode)
  
  
  test_episode_extended <- TherapyEpisode(
    conn = db_conn, 
    id = c("f770855cf9d424c76fdfbc9786d508ac", 
           "5528fc41106bb48eb4d48bc412e13e67"),
    extend_table_start = 2
  )
  
  test_expected_head_second_episode <- dplyr::tibble(
    t = -2:3, 
    patient_id = "8258333156", 
    therapy_id = "f770855cf9d424c76fdfbc9786d508ac", 
    therapy_start = structure(1444239793, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    therapy_end = structure(1444681333, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_start = structure(1444239793 + -2:3 * 3600, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    t_end = structure(1444239793 + -1:4 * 3600, tzone = "Europe/London", class = c("POSIXct", "POSIXt")), 
    parenteral = c(NA, 1L, 1L, 1L, 1L, 1L)
  )
  
  expect_equal(
    head(longitudinal_table(test_episode_extended, collect = TRUE)),
    test_expected_head_extended
  )
  expect_equal(
    tail(longitudinal_table(test_episode_extended, collect = TRUE)), 
    test_expected_tail_second_therapy_episode
  )
  expect_equal(
    head(dplyr::filter(longitudinal_table(test_episode_extended, collect = TRUE),
                       .data$therapy_id == "f770855cf9d424c76fdfbc9786d508ac")), 
    test_expected_head_second_episode
  )
  
  # TherapyEpisode .longitudinal_table_completeness_check -------------------------------------
  
  expect_true(
    .longitudinal_table_completeness_check(
      x = TherapyEpisode(db_conn, "592a738e4c2afcae6f625c01856151e0"),
      tbl_object = TherapyEpisode(db_conn, "592a738e4c2afcae6f625c01856151e0")@longitudinal_table,
      silent = F
    )
  )
  
  expect_false(
    expect_warning(
      .longitudinal_table_completeness_check(
        x = TherapyEpisode(db_conn, "592a738e4c2afcae6f625c01856151e0"),
        tbl_object = TherapyEpisode(db_conn, "89ac870bc1c1e4b2a37cec79d188cb08")@longitudinal_table,
        silent = F
      )
    )
  )
  
  # IVPO ------------------------------------------------------------------
  
  expect_equal(parenteral_changes(TherapyEpisode(db_conn, "5528fc41106bb48eb4d48bc412e13e67")), 
               list("5528fc41106bb48eb4d48bc412e13e67" = list(c(0, 241, 6))))
  expect_equal(parenteral_changes(TherapyEpisode(db_conn, 
                                                 c("f770855cf9d424c76fdfbc9786d508ac",
                                                   "74e3f378b91c6d7121a0d637bd56c2fa"))), 
               list("74e3f378b91c6d7121a0d637bd56c2fa" = list(c(0, 97, 49)),
                    "f770855cf9d424c76fdfbc9786d508ac" = list(c(0, 122, 74))))
  
  # Three IVPO changes in pt 5726385525 with only one therapy episode
  single_therapy <- dplyr::collect(dplyr::filter(tbl(db_conn, "drug_prescriptions"), 
                                                 patient_id == "5726385525"))
  expect_true(all(single_therapy$therapy_id == "a028cf950c29ca73c01803b54642d513"))
  expect_equal(
    parenteral_changes(TherapyEpisode(db_conn, "a028cf950c29ca73c01803b54642d513")),
    list(
      "a028cf950c29ca73c01803b54642d513" = list(c(0, 144, 97),
                                                c(146, 316, 219),
                                                c(318, 390,  NA))
    )
  )
  
  # therapy timeline -------------------------------------------------
  
  expect_error(
    therapy_timeline(Patient(conn = db_conn, 
                             id =  "I don't exist"))
  )
  expect_is(
    therapy_timeline(Patient(conn = db_conn, 
                             id =  "99999999999")),
    "timevis")
  expect_error(
    therapy_timeline(Patient(conn = db_conn, 
                             id =  "99999999999"),
                     date1 = "2017-01-01",
                     date2 = "2017-03-01")
  )
  expect_is(
    therapy_timeline(Patient(conn = db_conn, 
                             id =  "99999999999"),
                     date1 = as.Date("2017-01-01"),
                     date2 = as.Date("2017-03-01")), 
    "timevis")
  expect_is(
    therapy_timeline(Patient(conn = db_conn, 
                             id =  "99999999999"),
                     date1 = as.Date("2017-01-01")),
    "timevis")
  expect_is(
    therapy_timeline(Patient(conn = db_conn, 
                             id =  "99999999999"),
                     date2 = as.Date("2017-03-01")), 
    "timevis")
  expect_is(
    therapy_timeline(TherapyEpisode(conn = db_conn,
                                    id = "4d611fc8886c23ab047ad5f74e5080d7")), 
    "timevis")
  
  expect_is(
    expect_warning(
      therapy_timeline(TherapyEpisode(conn = db_conn,
                                      id = c("4d611fc8886c23ab047ad5f74e5080d7",
                                             "a028cf950c29ca73c01803b54642d513")))
    ), 
    "timevis")
  
  expect_is(
    therapy_timeline(MedicationRequest(conn = db_conn,
                                       id = "4d611fc8886c23ab047ad5f74e5080d7")), 
    "timevis")
  
  expect_is(
    therapy_timeline(Encounter(conn = db_conn,
                               id = "3968305736")),
    "timevis"
  )
  
  # clinical features --------------------------------------------------------
  
  # > last -------------------------------------------------------------------
  
  expect_warning(
    expect_s4_class(
      clinical_feature_last(
        TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
        observation_code = "8310-5",
        hours = 24,
        observation_code_system = "doesnotexist"
      ),
      "TherapyEpisode"
    )
  )
  last_temp <- clinical_feature_last(
    TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
    observation_code = "8310-5",
    hours = 24
  ) %>% 
    longitudinal_table(collect = T)

  expect_equal(
    last_temp$last_temperature_24h[1:5],
    c(36.9, 36.9, 36.8, 36.8, 36.8)
  )
  expect_equal(
    last_temp$last_temperature_24h[174:178],
    c(35.8, 35.8, 36.0, 36.0, 36.0)
  )
  rm(last_temp)
  
  last_temp_2therapies <- clinical_feature_last(
    TherapyEpisode(db_conn, c("4d611fc8886c23ab047ad5f74e5080d7",
                                "a028cf950c29ca73c01803b54642d513")),
    observation_code = "8310-5",
    hours = 24
  ) %>% 
    longitudinal_table(collect = T)
  
  expect_equal(
    dplyr::filter(last_temp_2therapies, 
                  therapy_id == "4d611fc8886c23ab047ad5f74e5080d7")$last_temperature_24h[1:5],
    c(36.9, 36.9, 36.8, 36.8, 36.8)
  )
  expect_equal(
    dplyr::filter(last_temp_2therapies, 
                  therapy_id == "4d611fc8886c23ab047ad5f74e5080d7" & 
                    t %in% 173:177)$last_temperature_24h,
    c(35.8, 35.8, 36.0, 36.0, 36.0)
  )
  rm(last_temp_2therapies)
  
  last_temp <- clinical_feature_last(
    TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
    observation_code = c("8310-5", "2160-0"),
    hours = 32
  ) %>% 
    longitudinal_table(collect = T)
    
  expect_equal(
    last_temp$last_temperature_32h[1:5],
    c(36.9, 36.9, 36.8, 36.8, 36.8)
  )
  expect_equal(
    last_temp$last_temperature_32h[174:178],
    c(35.8, 35.8, 36.0, 36.0, 36.0)
  )
  expect_equal(
    last_temp$last_creatinine_32h[1:5],
    c(116, 116, 116, 135, 135)
  )
  expect_equal(
    last_temp$last_creatinine_32h[174:178],
    c(109, 109, 109, NA, NA)
  )
  rm(last_temp)
  
  # > OLS -------------------------------------------------------------------

  example_therapy <-  TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7")
  example_therapy_record <- collect(example_therapy)
  
  expect_warning(
    expect_s4_class(
      clinical_feature_ols_trend(
        example_therapy,
        observation_code = "8310-5",
        hours = 24, 
        observation_code_system = "doesnotexist"
      ),
      "TherapyEpisode"
    )
  )
  
  ols_temp <- longitudinal_table(clinical_feature_ols_trend(
    example_therapy,
    observation_code = "8310-5",
    hours = 24
  ), collect = T)
  
  expect_equal(
    ols_temp$ols_temperature_24h_intercept[1:10],
    c(37.0910412742301, 37.5548549668735, 37.2275037202613, 37.0472923111899, 
      37.1741173337574, 37.075394573336, 36.9776345962514, 37.0335604592393, 
      36.6369479184972, 36.6346852329661)
  )
  expect_equal(
    ols_temp$ols_temperature_24h_intercept[169:178],
    c(35.9561283858803, 35.2034714186531, 35.6407220017662, 35.6068191357815, 
      35.5729162697968, 35.5390134038121, 35.5051105378274, 35.9753406349546, 
      35.9970238977037, 36.0187071604529)
  )
  expect_equal(
    ols_temp$ols_temperature_24h_slope[1:10],
    c(0.463813692637874, 0.463813692637874, 0.229966638232132, 0.126825022566752, 
      0.126825022566752, 0.0859028479069017, 0.0559258629877923, 0.0559258629877923, 
      -0.00226268553081795, -0.00226268553081795)
  )
  expect_equal(
    ols_temp$ols_temperature_24h_slope[169:178],
    c(0.00775828995026235, -0.0647998148577262, -0.0339028659846864, 
      -0.0339028659846864, -0.0339028659846864, -0.0339028659846864, 
      -0.0339028659846864, 0.0216832627491193, 0.0216832627491193, 
      0.0216832627491193)
  )
  
  # > interval ------------------------------------------------------------------
  
  expect_warning(
    expect_s4_class(
      clinical_feature_interval(
        TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
        observation_intervals = list("8310-5" = c(36, 38)),
        hours = 24,
        observation_code_system = "doesnotexist"
      ),
      "TherapyEpisode"
    )
  )
  
  temperature_check <- longitudinal_table(clinical_feature_interval(
    TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
    observation_intervals = list("8310-5" = c(36, 38)),
    hours = 24), collect = TRUE)
  
  expect_equal(temperature_check$range_temperature36_38_24h_in_range[1:5],
               c(3, 3, 4, 5, 5))
  expect_equal(temperature_check$range_temperature36_38_24h_in_range[174:178],
               c(1, 1, 1, 1, 1))
  expect_equal(temperature_check$range_temperature36_38_24h_strictly_under[1:5],
               c(0, 0, 0, 0, 0))
  expect_equal(temperature_check$range_temperature36_38_24h_strictly_under[174:178],
               c(2, 2, 2, 2, 2))
  expect_equal(temperature_check$range_temperature36_38_24h_strictly_over[1:5],
               c(0, 0, 0, 0, 0))
  expect_equal(temperature_check$range_temperature36_38_24h_strictly_over[174:178],
               c(0, 0, 0, 0, 0))
  
  expect_error(
    longitudinal_table(clinical_feature_interval(
      TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
      observation_intervals = list("8310-5" = c(NA, 38)),
      hours = 24), collect = TRUE)
  )

  temperature_check <- longitudinal_table(clinical_feature_interval(
    TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
    observation_intervals = list("8310-5" = c(38)),
    hours = 24), collect = TRUE)
  expect_equal(temperature_check$threshold_temperature38_24h_under[1:5],
               c(3, 3, 4, 5, 5))
  expect_equal(temperature_check$threshold_temperature38_24h_strictly_over[1:5],
               c(0, 0, 0, 0, 0))
  expect_equal(temperature_check$threshold_temperature38_24h_under[174:178],
               c(3, 3, 3, 3, 3))
  expect_equal(temperature_check$threshold_temperature38_24h_strictly_over[174:178],
               c(0, 0, 0, 0, 0))
  
  temperature_check <- longitudinal_table(clinical_feature_interval(
    TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
    observation_intervals = list("8310-5" = c(36)),
    hours = 24), collect = TRUE)
  expect_equal(temperature_check$threshold_temperature36_24h_under[1:5],
               c(1, 1, 1, 1, 1))
  expect_equal(temperature_check$threshold_temperature36_24h_strictly_over[1:5],
               c(2, 2, 3, 4, 4))
  expect_equal(temperature_check$threshold_temperature36_24h_under[174:178],
               c(2, 2, 3, 3, 3))
  expect_equal(temperature_check$threshold_temperature36_24h_strictly_over[174:178],
               c(1, 1, 0, 0, 0))
  
  # > mean ------------------------------------------------------------------
  
  expect_warning(
    expect_s4_class(
      clinical_feature_mean(
        TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
        observation_code = "8310-5",
        hours = 2, 
        observation_code_system = "doesnotexist"),
      "TherapyEpisode"
    )
  )
  
  temperature_check <- longitudinal_table(
    clinical_feature_mean(
      TherapyEpisode(db_conn, "4d611fc8886c23ab047ad5f74e5080d7"),
      observation_code = "8310-5",
      hours = 2),
    collect = TRUE
  )
  expect_equal(temperature_check$mean_temperature_2h[1:4],
               c(36.9, 36.9, 36.8, 36.8))
  expect_equal(temperature_check$mean_temperature_2h[174:178],
               c(NA, NA, 36.0, 36.0, NA))
  
  # show methods ----------------------------------------------------------
  
  # TherapyEpisode
  expect_equal(
    utils::capture.output(TherapyEpisode(db_conn, "89ac870bc1c1e4b2a37cec79d188cb08"))[1:8],
    c("TherapyEpisode 89ac870bc1c1e4b2a37cec79d188cb08 ", "Patient:   1555756339 ", 
      paste0("Start:     ",
             format(as.POSIXct("2017-07-02 01:15:46", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "), 
      paste0("End:       ",
             format(as.POSIXct("2017-07-06 01:35:46", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "), 
      "", "Medications:", "  > Amoxicillin/clavulanic acid IV 1.2g 2 days", 
      "  > Clarithromycin ORAL 500mg 4 days"))
  expect_equal(
    utils::capture.output(TherapyEpisode(db_conn, "fa179f4bcf3efa1e21225ab207ab40c4"))[1:11],
    c("TherapyEpisode fa179f4bcf3efa1e21225ab207ab40c4 ", "Patient:   3422481921 ", 
      paste0("Start:     ", 
             format(as.POSIXct("2017-11-15 15:33:36", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "),
      paste0("End:       ",
             format(as.POSIXct("2017-12-01 21:11:36", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "),
      "", "Medications:", "  > Amoxicillin/clavulanic acid IV 1.2g 2 days", 
      "  > Amoxicillin/clavulanic acid IV 1.2g 2 days", "  > Piperacillin/tazobactam IV 4.5g 4 days", 
      "  > Amoxicillin/clavulanic acid IV 1.2g 2 days", "  ... (2 additional medication requests)"))
  expect_equal(
    utils::capture.output(TherapyEpisode(db_conn, "biduletruc"))[1:5],
    c("TherapyEpisode biduletruc ", "Record is not available.", "Please check object id is valid", 
      "", "Database connection:")
  )
  expect_equal(
    utils::capture.output(
      TherapyEpisode(conn = db_conn, 
                     id = c("f770855cf9d424c76fdfbc9786d508ac", 
                            "5528fc41106bb48eb4d48bc412e13e67")))[1:3],
    c("TherapyEpisode 5528fc41106bb48eb4d48bc412e13e67, f770855cf9d424c76fdfbc9786d508ac ", 
      "[total of 2 therapy episodes]",
      "Patient(s):   99999999999, 8258333156 ")
  )
  
  # MedicationRequest
  
  expect_equal(
    utils::capture.output(MedicationRequest(db_conn, "5528fc41106bb48eb4d48bc412e13e67"))[1:8],
    c("MedicationRequest 5528fc41106bb48eb4d48bc412e13e67 ", "Clarithromycin IV 500mg 0 days ", 
      "Patient:     99999999999 ", 
      paste0("Start:        ", 
             format(as.POSIXct("2015-08-07 10:27:00", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "),
      paste0("End:          ", 
             format(as.POSIXct("2015-08-07 15:59:00", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "),
      "Therapy:      5528fc41106bb48eb4d48bc412e13e67 ", 
      "", "Database connection:"))
  expect_equal(
    utils::capture.output(MedicationRequest(db_conn, "1ab55e515af6b86dde76abbe0bffbd3f"))[1:9],
    c("MedicationRequest 1ab55e515af6b86dde76abbe0bffbd3f ", "Clarithromycin ORAL 500mg 4 days ", 
      "Patient:     3894468747 ", 
      paste0("Start:        ", 
             format(as.POSIXct("2015-10-01 21:38:55", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "), 
      paste0("End:          ", 
             format(as.POSIXct("2015-10-05 21:38:55", tz = "Europe/London"), tz = "Europe/London", format = "%Y-%m-%d %H:%M:%S %Z"),
             " "),
      "Combination:  1ab55e515af6b86dde76abbe0bffbd3f ", 
      "Therapy:      1ab55e515af6b86dde76abbe0bffbd3f ", "", "Database connection:"
    ))
  expect_equal(
    utils::capture.output(MedicationRequest(db_conn, "biduletruc"))[1:5],
    c("MedicationRequest biduletruc ", "Record is not available.", "Please check object id is valid", 
      "", "Database connection:")
  )
  
  # other consistency checks ----------------------------------------------
  
  # check that therapy id is the one of the first prescription
  invalid_therapy_ids <- tbl(db_conn, "drug_prescriptions") %>% 
    dplyr::filter(therapy_rank == 1 & therapy_id != prescription_id) %>% 
    dplyr::collect()
  expect_true(nrow(invalid_therapy_ids) == 0)
  
})


test_that("Encounter class on DuckDB", {
  
  db_conn <- create_mock_database(file = "test.duckdb", timezone = "UTC", silent = TRUE)
  on.exit({
    DBI::dbDisconnect(db_conn, shutdown = TRUE)
    file.remove("test.duckdb") 
  })
  
  # Encounter ------------------------------------------------------------------
  
  test_encounter <- Encounter(db_conn, "3968305736")
  test_output <- longitudinal_table(test_encounter, collect = T)
  test_expected_head <- dplyr::tibble(
    t = 0:5,
    patient_id = "99999999999",
    encounter_id = "3968305736",
    admission_date = structure(1486982520, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    discharge_date = structure(1487932800, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_start = structure(1486982520 + 0:5*3600, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_end = structure(1486982520 + 1:6*3600, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  )
  test_expected_tail <- dplyr::tibble(
    t = 258:263,
    patient_id = "99999999999",
    encounter_id = "3968305736",
    admission_date = structure(1486982520, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    discharge_date = structure(1487932800, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    t_start = structure(1486982520 + 258:263*3600, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_end = structure(c(1486982520 + 259:263*3600, 1487932800), tzone = "UTC", class = c("POSIXct", "POSIXt"))
  )
  
  expect_equal(head(test_output), test_expected_head)
  expect_equal(tail(test_output), test_expected_tail)
  expect_equal(
    as.numeric(sum(difftime(test_output$t_end, test_output$t_start,units =  "days"))),
    sum(collect(test_encounter)[["ramses_bed_days"]])
  )
  
  test_encounter_extended <- Encounter(db_conn, "3968305736", extend_table_start = 2)
  test_output_extended <- longitudinal_table(test_encounter_extended, collect = T)
  
  test_expected_head_extended <- dplyr::tibble(
    t = -2:3,
    patient_id = "99999999999",
    encounter_id = "3968305736",
    admission_date = structure(1486982520, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    discharge_date = structure(1487932800, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_start = structure(1486982520 + -2:3*3600, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_end = structure(1486982520 + -1:4*3600, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  )
  
  expect_equal(head(test_output_extended), test_expected_head_extended)
  expect_equal(tail(test_output_extended), test_expected_tail)
  expect_equal(
    as.numeric(sum(difftime(test_output_extended$t_end, test_output_extended$t_start, units =  "days"))),
    sum(collect(test_encounter)[["ramses_bed_days"]]) + 2/24
  )
  
  # 2+ Encounters --------------------------------------------------------------
  
  test_encounter <- Encounter(conn = db_conn, 
                              id = c("3968305736", "9278078393"))
  expect_is(test_encounter, "Encounter")
  
  test_expected_tail_second_encounter <- dplyr::tibble(
    t = 20:25,
    patient_id = "99999999999",
    encounter_id = "9278078393",
    admission_date = structure(1459332000, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    discharge_date = structure(1459425600, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    t_start = structure(1459332000 + 20:25*3600, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_end = structure(c(1459332000 + 21:25*3600, 1459425600), tzone = "UTC", class = c("POSIXct", "POSIXt"))
  )
  
  expect_equal(head(longitudinal_table(test_encounter, collect = TRUE)), 
               test_expected_head)
  
  expect_equal(tail(longitudinal_table(test_encounter, collect = TRUE)), 
               test_expected_tail_second_encounter)
  
  test_encounter_extended <- Encounter(conn = db_conn, 
                                       id = c("3968305736", "9278078393"),
                                       extend_table_start = 2)
  
  test_expected_head_second_encounter <- dplyr::tibble(
    t = -2:3,
    patient_id = "99999999999",
    encounter_id = "9278078393",
    admission_date = structure(1459332000, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    discharge_date = structure(1459425600, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    t_start = structure(1459332000 + -2:3*3600, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_end = structure(c(1459332000 + -1:4*3600), tzone = "UTC", class = c("POSIXct", "POSIXt"))
  )
  
  test_expected_tail_second_encounter <- dplyr::tibble(
    t = 20:25,
    patient_id = "99999999999",
    encounter_id = "9278078393",
    admission_date = structure(1459332000, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    discharge_date = structure(1459425600, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
    t_start = structure(1459332000 + 20:25*3600, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    t_end = structure(c(1459332000 + 21:25*3600, 1459425600), tzone = "UTC", class = c("POSIXct", "POSIXt"))
  )
  
  expect_equal(
    head(longitudinal_table(test_encounter_extended, collect = TRUE)),
    test_expected_head_extended
  )
  expect_equal(
    tail(longitudinal_table(test_encounter_extended, collect = TRUE)), 
    test_expected_tail_second_encounter
  )
  expect_equal(
    head(dplyr::filter(longitudinal_table(test_encounter_extended, collect = TRUE),
                       .data$encounter_id == 9278078393)), 
    test_expected_head_second_encounter
  )
  
  # Encounter .longitudinal_table_completeness_check ---------------------------
  
  expect_true(
    .longitudinal_table_completeness_check(
      x = Encounter(db_conn, "3968305736"),
      tbl_object = Encounter(db_conn, "3968305736")@longitudinal_table,
      silent = F
    )
  )
  
  expect_false(
    expect_warning(
      .longitudinal_table_completeness_check(
        x = Encounter(db_conn, "3968305736"),
        tbl_object = Encounter(db_conn, "4956274655")@longitudinal_table,
        silent = F
      )
    )
  )
  
  # clinical features --------------------------------------------------------
  
  # > last -------------------------------------------------------------------
  
  expect_warning(
    expect_s4_class(
      clinical_feature_last(
        Encounter(db_conn, "9278078393"),
        observation_code = "8310-5",
        hours = 24,
        observation_code_system = "doesnotexist"
      ),
      "Encounter"
    )
  )
  last_temp <- clinical_feature_last(
    Encounter(db_conn, "9278078393"),
    observation_code = "8310-5",
    hours = 24
  ) %>% 
    longitudinal_table(collect = T)
  
  last_temp_extended_1h <- clinical_feature_last(
    Encounter(db_conn, "9278078393", extend_table_start = 1),
    observation_code = "8310-5",
    hours = 24
  ) %>% 
    longitudinal_table(collect = T)
  
  expect_equal(
    last_temp$last_temperature_24h[1:5],
    c(NA, NA, NA, 35.7, 35.7)
  )
  expect_equal(
    last_temp_extended_1h$last_temperature_24h[1:6],
    c(NA, NA, NA, NA, 35.7, 35.7)
  )
  expect_equal(
    last_temp$last_temperature_24h[21:25],
    c(37.1, 37.1, 37.1, 37.1, 37.1)
  )
  expect_equal(
    last_temp_extended_1h$last_temperature_24h[22:26],
    c(37.1, 37.1, 37.1, 37.1, 37.1)
  )
  rm(last_temp)
  
  last_temp_2encounters <- clinical_feature_last(
    Encounter(db_conn, c("3968305736", "9278078393")),
    observation_code = "8310-5",
    hours = 24
  ) %>% 
    longitudinal_table(collect = T)
  
  expect_equal(
    dplyr::filter(last_temp_2encounters, 
                  encounter_id == "9278078393")$last_temperature_24h[1:5],
    c(NA, NA, NA, 35.7, 35.7)
  )
  expect_equal(
    dplyr::filter(last_temp_2encounters, 
                  encounter_id == "9278078393" & 
                    t %in% 20:25)$last_temperature_24h,
    c(37.1, 37.1, 37.1, 37.1, 37.1, 37.1)
  )
  rm(last_temp_2encounters)
  
  last_temp <- clinical_feature_last(
    Encounter(db_conn, "3968305736"),
    observation_code = c("8310-5", "2160-0"),
    hours = 32
  ) %>% 
    longitudinal_table(collect = T)
  
  expect_equal(
    last_temp$last_temperature_32h[1:5],
    c(NA, 36, 36.9, 36.9, 36.8)
  )
  expect_equal(
    last_temp$last_temperature_32h[174:178],
    c(35.8, 35.8, 35.8, 35.8, 35.8)
  )
  expect_equal(
    last_temp$last_creatinine_32h[1:5],
    c(116, 116, 116, 116, 116)
  )
  expect_equal(
    last_temp$last_creatinine_32h[174:178],
    c(109, 109, 109, 109, 109)
  )
  rm(last_temp)
  
  # > OLS -------------------------------------------------------------------
  
  example_encounter <-  Encounter(db_conn, "9278078393")
  example_encounter_record <- collect(example_encounter)
  
  expect_warning(
    expect_s4_class(
      clinical_feature_ols_trend(
        example_encounter,
        observation_code = "8310-5",
        hours = 24, 
        observation_code_system = "doesnotexist"
      ),
      "Encounter"
    )
  )
  
  ols_temp <- longitudinal_table(clinical_feature_ols_trend(
    example_encounter,
    observation_code = "8310-5",
    hours = 24
  ), collect = T)
  
  expect_equal(
    ols_temp$ols_temperature_24h_intercept[1:10],
    c(NA, NA, NA, NA, NA, 
      36.6131724683544, 37.0403876582278, 36.4450313376705, 
      36.5775984454222, 36.719050516548)
  )
  expect_equal(
    ols_temp$ols_temperature_24h_slope[1:10],
    c(NA, NA, NA, NA, NA, 0.427215189873417, 0.427215189873417, 0.144431722745784, 
      0.141452071125883, 0.141452071125883)
  )

  # > interval ------------------------------------------------------------------
  
  expect_warning(
    expect_s4_class(
      clinical_feature_interval(
        Encounter(db_conn, "9278078393"),
        observation_intervals = list("8310-5" = c(36, 38)),
        hours = 24,
        observation_code_system = "doesnotexist"
      ),
      "Encounter"
    )
  )
  
  temperature_check <- longitudinal_table(clinical_feature_interval(
    Encounter(db_conn, "9278078393"),
    observation_intervals = list("8310-5" = c(36, 38)),
    hours = 24), collect = TRUE)
  
  expect_equal(temperature_check$range_temperature36_38_24h_in_range[1:5],
               c(NA, NA, NA, 0, 0))
  expect_equal(temperature_check$range_temperature36_38_24h_strictly_under[1:5],
               c(NA, NA, NA, 1, 1))
  expect_equal(temperature_check$range_temperature36_38_24h_strictly_over[1:5],
               c(NA, NA, NA, 0, 0))
  
  expect_error(
    longitudinal_table(clinical_feature_interval(
      Encounter(db_conn, "9278078393"),
      observation_intervals = list("8310-5" = c(NA, 38)),
      hours = 24), collect = TRUE)
  )
  
  temperature_check <- longitudinal_table(clinical_feature_interval(
    Encounter(db_conn, "9278078393"),
    observation_intervals = list("8310-5" = c(38)),
    hours = 24), collect = TRUE)
  expect_equal(temperature_check$threshold_temperature38_24h_under[1:5],
               c(NA, NA, NA, 1, 1))
  expect_equal(temperature_check$threshold_temperature38_24h_strictly_over[1:5],
               c(NA, NA, NA, 0, 0))
  
  temperature_check <- longitudinal_table(clinical_feature_interval(
    Encounter(db_conn, "9278078393"),
    observation_intervals = list("8310-5" = c(36)),
    hours = 24), collect = TRUE)
  expect_equal(temperature_check$threshold_temperature36_24h_under[1:6],
               c(NA, NA, NA, 1, 1, 1))
  expect_equal(temperature_check$threshold_temperature36_24h_strictly_over[1:6],
               c(NA, NA, NA, 0, 0, 1))
  
  # > mean ------------------------------------------------------------------
  
  expect_warning(
    expect_s4_class(
      clinical_feature_mean(
        Encounter(db_conn, "9278078393"),
        observation_code = "8310-5",
        hours = 2, 
        observation_code_system = "doesnotexist"),
      "Encounter"
    )
  )
  
  temperature_check <- longitudinal_table(
    clinical_feature_mean(
      Encounter(db_conn, "9278078393"),
      observation_code = "8310-5",
      hours = 24),
    collect = TRUE
  )
  expect_equal(temperature_check$mean_temperature_24h[1:6],
               c(NA, NA, NA, 35.7, 35.7, 36.15))
})
