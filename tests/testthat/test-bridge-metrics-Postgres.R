
test_that(".tbl_add_demographics on Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  db_conn <- DBI::dbConnect(RPostgres::Postgres(),
                            user = "user", 
                            password = "password",
                            host = "localhost", 
                            dbname = "RamsesDB_testing",
                            timezone = "UTC")
  on.exit({
    .remove_db_tables(conn = db_conn,
                      DBI::dbListTables(db_conn))
    DBI::dbDisconnect(db_conn)
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

test_that("bridge_episode_prescription_overlap on Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  db_conn <- DBI::dbConnect(RPostgres::Postgres(),
                            user = "user", 
                            password = "password",
                            host = "localhost", 
                            dbname="RamsesDB_testing",
                            timezone = "UTC")
  on.exit({
    .remove_db_tables(conn = db_conn,
                      DBI::dbListTables(db_conn))
    DBI::dbDisconnect(db_conn)
  })
  
  
  test_rx <- dplyr::tibble(
    patient_id = 1, 
    prescription_id = 1:4, 
    authoring_date = as.POSIXct(c("2017-11-25 12:23:07","2017-11-25 12:23:07", 
                                  "2015-03-01 09:20:37", "2015-03-01 09:20:37")), 
    prescription_start = as.POSIXct(c("2017-11-26 14:04:07","2017-11-26 14:04:07",
                                      "2015-03-01 10:37:37", "2015-03-01 10:37:37")), 
    prescription_end = as.POSIXct(c("2017-12-01 14:04:07","2017-12-01 14:04:07",
                                    "2015-03-04 10:37:37", "2015-03-04 10:37:37")), 
    prescription_status = c("completed", "stopped", "cancelled", NA), 
    prescription_context = c("inpatient", "outpatient", "inpatient", "inpatient"), 
    dose = c(500, 400, 2, 2), unit = c("mg", "mg", "g", "g"), 
    route = c("ORAL", "ORAL", "IV", "IV"), 
    frequency = c("6H", "OD", "6H", "6H"), 
    daily_frequency = c(4, 1, 4, 4), 
    DDD = c(1, 1, 4, 4)
  ) 
  
  test_ip <- dplyr::tibble(
    patient_id = 1, encounter_id = 1,
    admission_method = "2",
    admission_date = as.POSIXct("2017-11-23 10:47:07"),
    discharge_date = as.POSIXct("2017-11-30 14:04:07"),
    episode_start = as.POSIXct("2017-11-23 10:47:07"),
    episode_end = as.POSIXct("2017-11-30 14:04:07"),
    episode_number = 1,
    last_episode_in_encounter = 1,
    consultant_code = 1,
    main_specialty_code = 100
  )
  
  expect_error(bridge_episode_prescription_overlap(db_conn))
  
  dplyr::copy_to(db_conn, 
                 df = test_rx,
                 name = "drug_prescriptions")
  
  expect_error(bridge_episode_prescription_overlap(db_conn))
  
  dplyr::copy_to(db_conn, 
                 df = test_ip,
                 name = "inpatient_episodes")
  
  expect_true(bridge_episode_prescription_overlap(db_conn))
  
  expect_true(DBI::dbExistsTable(db_conn, "bridge_episode_prescription_overlap"))
  
  expect_equal(
    dplyr::arrange(
      dplyr::collect(tbl(db_conn, "bridge_episode_prescription_overlap")),
      prescription_id
    ),
    dplyr::tibble(
      patient_id = 1, 
      encounter_id = 1,
      episode_number = 1,
      prescription_id = 1:2,
      t_start = as.POSIXct(c("2017-11-26 14:04:07","2017-11-26 14:04:07"), tz = "UTC"),
      t_end = as.POSIXct(c("2017-11-30 14:04:07", "2017-11-30 14:04:07"), tz = "UTC")
    )
  )
})

test_that("bridge_episode_prescription_initiation on Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  db_conn <- DBI::dbConnect(RPostgres::Postgres(),
                            user = "user", 
                            password = "password",
                            host = "localhost", 
                            dbname="RamsesDB_testing",
                            timezone = "UTC")
  on.exit({
    .remove_db_tables(conn = db_conn,
                      DBI::dbListTables(db_conn))
    DBI::dbDisconnect(db_conn)
  })
  
  test_rx <- dplyr::tibble(
    patient_id = 1, 
    prescription_id = 1:4, 
    authoring_date = as.POSIXct(c("2017-11-25 12:23:07","2017-11-25 12:23:07", 
                                  "2015-03-01 09:20:37", "2015-03-01 09:20:37")), 
    prescription_start = as.POSIXct(c("2017-11-26 14:04:07","2017-11-26 14:04:07",
                                      "2015-03-01 10:37:37", "2015-03-01 10:37:37")), 
    prescription_end = as.POSIXct(c("2017-12-01 14:04:07","2017-12-01 14:04:07",
                                    "2015-03-04 10:37:37", "2015-03-04 10:37:37")), 
    prescription_status = c("completed", "stopped", "cancelled", NA), 
    prescription_context = c("inpatient", "outpatient", "inpatient", "inpatient"), 
    dose = c(500, 400, 2, 2), unit = c("mg", "mg", "g", "g"), 
    route = c("ORAL", "ORAL", "IV", "IV"), 
    frequency = c("6H", "OD", "6H", "6H"), 
    daily_frequency = c(4, 1, 4, 4), 
    DDD = c(1, 1, 4, 4)
  ) 
  
  test_ip <- dplyr::tibble(
    patient_id = 1, encounter_id = 1,
    admission_method = "2",
    admission_date = as.POSIXct("2017-11-23 10:47:07"),
    discharge_date = as.POSIXct("2017-11-30 14:04:07"),
    episode_start = as.POSIXct("2017-11-23 10:47:07"),
    episode_end = as.POSIXct("2017-11-30 14:04:07"),
    episode_number = 1,
    last_episode_in_encounter = 1,
    consultant_code = 1,
    main_specialty_code = 100
  )
  
  expect_error(bridge_episode_prescription_initiation(db_conn))
  
  dplyr::copy_to(db_conn, 
                 df = test_rx,
                 name = "drug_prescriptions")
  
  expect_error(bridge_episode_prescription_initiation(db_conn))
  
  dplyr::copy_to(db_conn, 
                 df = test_ip,
                 name = "inpatient_episodes")
  
  expect_true(bridge_episode_prescription_initiation(db_conn))
  
  expect_true(DBI::dbExistsTable(db_conn, "bridge_episode_prescription_initiation"))
  
  expect_equal(
    tbl(db_conn, "bridge_episode_prescription_initiation") %>% 
      dplyr::arrange(.data$episode_number, .data$prescription_id) %>% 
      dplyr::collect(),
    dplyr::tibble(
      patient_id = 1, 
      encounter_id = 1,
      episode_number = 1,
      prescription_id = 1:2
    )
  )
})

test_that("bridge_episode_therapy_overlap on Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  db_conn <- DBI::dbConnect(RPostgres::Postgres(),
                            user = "user", 
                            password = "password",
                            host = "localhost", 
                            dbname="RamsesDB_testing",
                            timezone = "UTC")
  on.exit({
    .remove_db_tables(conn = db_conn,
                      DBI::dbListTables(db_conn))
    DBI::dbDisconnect(db_conn)
  })
  
  test_th_span_out <- dplyr::tibble(
    patient_id = 1, 
    therapy_id = 1, 
    therapy_start = as.POSIXct("2017-11-26 14:04:07"), 
    therapy_end = as.POSIXct("2017-12-01 14:04:07")
  )
  
  test_th_within <- dplyr::tibble(
    patient_id = 1, 
    therapy_id = 1, 
    therapy_start = as.POSIXct("2017-11-26 14:04:07"), 
    therapy_end = as.POSIXct("2017-11-28 14:04:07")
  )
  
  test_th_beyond <- dplyr::tibble(
    patient_id = 1, 
    therapy_id = 1, 
    therapy_start = as.POSIXct("2017-11-20 14:04:07"), 
    therapy_end = as.POSIXct("2017-12-01 14:04:07")
  )
  
  test_ip <- dplyr::tibble(
    patient_id = 1, encounter_id = 1,
    admission_method = "2",
    admission_date = as.POSIXct("2017-11-23 10:47:07"),
    discharge_date = as.POSIXct("2017-11-30 14:04:07"),
    episode_start = as.POSIXct("2017-11-23 10:47:07"),
    episode_end = as.POSIXct("2017-11-30 14:04:07"),
    episode_number = 1,
    last_episode_in_encounter = 1,
    consultant_code = 1,
    main_specialty_code = 100
  )
  
  expect_error(bridge_episode_therapy_overlap(db_conn))
  
  dplyr::copy_to(db_conn, 
                 df = test_th_span_out,
                 name = "drug_therapy_episodes")
  
  expect_error(bridge_episode_therapy_overlap(db_conn))
  
  dplyr::copy_to(db_conn, 
                 df = test_ip,
                 name = "inpatient_episodes")
  
  expect_true(bridge_episode_therapy_overlap(db_conn))
  
  expect_true(DBI::dbExistsTable(db_conn, "bridge_episode_therapy_overlap"))
  
  expect_equal(
    tbl(db_conn, "bridge_episode_therapy_overlap") %>% 
      dplyr::arrange(.data$episode_number) %>% 
      dplyr::collect(),
    dplyr::tibble(
      patient_id = 1, 
      encounter_id = 1,
      episode_number = 1,
      therapy_id = 1,
      t_start = as.POSIXct("2017-11-26 14:04:07", tz = "UTC"),
      t_end = as.POSIXct("2017-11-30 14:04:07", tz = "UTC")
    )
  )
  
  dplyr::copy_to(db_conn, 
                 df = test_th_within,
                 name = "drug_therapy_episodes",
                 overwrite = TRUE)
  
  expect_true(bridge_episode_therapy_overlap(db_conn, overwrite = TRUE))
  expect_equal(
    tbl(db_conn, "bridge_episode_therapy_overlap") %>% 
      dplyr::arrange(.data$episode_number) %>% 
      dplyr::collect(),
    dplyr::tibble(
      patient_id = 1, 
      encounter_id = 1,
      episode_number = 1,
      therapy_id = 1,
      t_start = as.POSIXct("2017-11-26 14:04:07", tz = "UTC"),
      t_end = as.POSIXct("2017-11-28 14:04:07", tz = "UTC")
    )
  )
  
  dplyr::copy_to(db_conn, 
                 df = test_th_beyond,
                 name = "drug_therapy_episodes",
                 overwrite = TRUE)
  
  expect_true(bridge_episode_therapy_overlap(db_conn, overwrite = TRUE))
  expect_equal(
    tbl(db_conn, "bridge_episode_therapy_overlap") %>% 
      dplyr::arrange(.data$episode_number) %>% 
      dplyr::collect(),
    dplyr::tibble(
      patient_id = 1, 
      encounter_id = 1,
      episode_number = 1,
      therapy_id = 1,
      t_start = as.POSIXct("2017-11-23 10:47:07", tz = "UTC"),
      t_end = as.POSIXct("2017-11-30 14:04:07", tz = "UTC")
    )
  )
})

test_that("bridge_drug_prescriptions_date on Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  db_conn <- DBI::dbConnect(RPostgres::Postgres(),
                            user = "user", 
                            password = "password",
                            host = "localhost", 
                            dbname="RamsesDB_testing",
                            timezone = "UTC")
  on.exit({
    .remove_db_tables(conn = db_conn,
                      DBI::dbListTables(db_conn))
    DBI::dbDisconnect(db_conn)
  })
  
  expect_error(bridge_drug_prescriptions_date(db_conn))
  
  test_ip <- dplyr::tibble(
    patient_id = 1, encounter_id = 1,
    admission_method = "2",
    admission_date = as.POSIXct("2017-11-23 10:47:07"),
    discharge_date = as.POSIXct("2017-11-30 14:04:07"),
    episode_start = as.POSIXct("2017-11-23 10:47:07"),
    episode_end = as.POSIXct("2017-11-30 11:04:07"),
    episode_number = 1,
    last_episode_in_encounter = 1,
    consultant_code = 1,
    main_specialty_code = 100
  )
  
  test_rx <- dplyr::tibble(
    patient_id = 1, 
    prescription_id = 1:3, 
    authoring_date = as.POSIXct(c("2017-11-25 12:23:07","2017-11-25 12:23:07", "2015-03-01 09:20:37")), 
    prescription_start = as.POSIXct(c("2017-11-26 14:04:07","2017-11-26 14:04:07", "2015-03-01 10:37:37")), 
    prescription_end = as.POSIXct(c("2017-12-01 14:04:07","2017-12-01 14:04:07", "2015-03-04 10:37:37")), 
    prescription_status = c("completed", "stopped", "cancelled"), 
    prescription_context = c("inpatient", "outpatient", "inpatient"), 
    dose = c(500, 400, 2), unit = c("mg", "mg", "g"), 
    route = c("ORAL", "ORAL", "IV"), 
    frequency = c("6H", "12H", "6H"), 
    daily_frequency = c(4, 2, 4), 
    DDD = c(1, 2, 4)
  ) 
  
  dplyr::copy_to(db_conn, 
                 df = test_rx,
                 name = "drug_prescriptions")
  
  dplyr::copy_to(db_conn, 
                 df = test_ip,
                 name = "inpatient_episodes")
  
  expect_error(bridge_drug_prescriptions_date(db_conn))
  expect_true(bridge_episode_prescription_overlap(db_conn))
  expect_true(bridge_drug_prescriptions_date(db_conn))
  expect_true(DBI::dbExistsTable(db_conn, "bridge_drug_prescriptions_date"))
  
  test_output <- dplyr::collect(dplyr::tbl(db_conn, "bridge_drug_prescriptions_date"))
  
  expect_equal(
    dplyr::arrange(test_output, prescription_id, date),
    dplyr::tibble(
      "patient_id" = 1,
      "prescription_id" = c(rep(1, 6), rep(2, 6)), 
      "date" = c(seq(as.Date("2017-11-26"), as.Date("2017-12-01"), 1),
                 seq(as.Date("2017-11-26"), as.Date("2017-12-01"), 1)), 
      "DOT_prescribed_all" = c(
        0.41380787037037, 1, 1, 1, 1, 0.58619212962963, 
        0.41380787037037, 1, 1, 1, 1, 0.58619212962963
      ),
      "DDD_prescribed_all" = c(
        0.41380787037037, 1, 1, 1, 1, 0.58619212962963, 
        0.827615740740741, 2, 2, 2, 2, 1.17238425925926
      ),
      "DOT_prescribed_IP_only" = c(
        0.41380787037037, 1, 1, 1, 0.46119212962963, NA, 
        0.41380787037037, 1, 1, 1, 0.46119212962963, NA
      ),
      "DDD_prescribed_IP_only" = c(
        0.41380787037037, 1, 1, 1, 0.46119212962963, NA, 
        0.827615740740741, 2, 2, 2, 0.922384259259259, NA
      )
    )
  )
})

test_that("bridge_inpatient_episodes_date on Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  db_conn <- DBI::dbConnect(RPostgres::Postgres(),
                            user = "user", 
                            password = "password",
                            host = "localhost", 
                            dbname="RamsesDB_testing",
                            timezone = "UTC")
  on.exit({
    .remove_db_tables(conn = db_conn,
                      DBI::dbListTables(db_conn))
    DBI::dbDisconnect(db_conn)
  })
  
  expect_error(bridge_inpatient_episodes_date(db_conn))
  
  test_th_span_out <- dplyr::tibble(
    patient_id = 1, 
    therapy_id = 1, 
    therapy_start = as.POSIXct("2017-11-26 14:04:07"), 
    therapy_end = as.POSIXct("2017-12-01 14:04:07")
  )
  
  test_th_within <- dplyr::tibble(
    patient_id = 1, 
    therapy_id = 1, 
    therapy_start = as.POSIXct("2017-11-26 14:04:07"), 
    therapy_end = as.POSIXct("2017-11-28 14:04:07")
  )
  
  test_ip <- dplyr::tibble(
    patient_id = 1, encounter_id = 1,
    admission_method = "2",
    admission_date = as.POSIXct("2017-11-23 10:47:07"),
    discharge_date = as.POSIXct("2017-11-30 14:04:07"),
    episode_start = as.POSIXct("2017-11-23 10:47:07"),
    episode_end = as.POSIXct("2017-11-30 14:04:07"),
    episode_number = 1,
    last_episode_in_encounter = 1,
    consultant_code = 1,
    main_specialty_code = 100
  )
  
  dplyr::copy_to(db_conn, 
                 df = test_ip,
                 name = "inpatient_episodes")
  
  expect_error(bridge_inpatient_episodes_date(db_conn))
  
  dplyr::copy_to(db_conn,
                 df = test_th_span_out,
                 name = "drug_therapy_episodes")
  
  expect_true(bridge_inpatient_episodes_date(db_conn))
  
  expect_true(DBI::dbExistsTable(db_conn, "bridge_inpatient_episodes_date"))
  
  test_output <- dplyr::collect(dplyr::tbl(db_conn, "bridge_inpatient_episodes_date"))
  
  expect_equal(
    dplyr::arrange(test_output, encounter_id, episode_number, date),
    dplyr::tibble(
      "patient_id" = 1, 
      "encounter_id" = 1, 
      "episode_number" = 1, 
      "date" = seq(as.Date("2017-11-23"), as.Date("2017-11-30"), 1) ,
      "bed_days" = c(0.550613425925926, 1, 1, 1, 1, 1, 1, 0.58619212962963)
      # "DNOT_antibacterial" = c(0.550613425925926, 1, 1, 0.58619212962963, 0, 0, 0, 0),
      # "DNOT_antifungal" = c(0.550613425925926, 1, 1, 1, 1, 1, 1, 0.58619212962963)
    )
  )
  
  dplyr::copy_to(db_conn,
                 df = test_th_within,
                 name = "drug_therapy_episodes",
                 overwrite = TRUE)
  expect_true(bridge_inpatient_episodes_date(db_conn, overwrite = TRUE))
  test_output <- dplyr::collect(dplyr::tbl(db_conn, "bridge_inpatient_episodes_date"))
  expect_equal(
    dplyr::arrange(test_output, encounter_id, episode_number, date),
    dplyr::tibble(
      "patient_id" = 1, 
      "encounter_id" = 1, 
      "episode_number" = 1, 
      "date" = seq(as.Date("2017-11-23"), as.Date("2017-11-30"), 1),
      "bed_days" = c(0.550613425925926, 1, 1, 1, 1, 1, 1, 0.58619212962963)
      # "DNOT_antibacterial" = c(0.550613425925926, 1, 1, 0.58619212962963, 0, 0.4138079, 1, 1),
      # "DNOT_antifungal" = c(0.550613425925926, 1, 1, 1, 1, 1, 1, 0.58619212962963)
    )
  )
})
