
test_that(".therapy_timeline_get_diagnoses (merge diagnoses)", {
  
  db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(db_conn, shutdown = TRUE)})
  patients <- dplyr::tibble(patient_id = 99)
  encounters <- dplyr::tibble(
    patient_id = 99, encounter_id = 99,
    admission_method = "2",
    admission_date = structure(1438684036, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    discharge_date = structure(1439380488, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    episode_number = 1:5, last_episode_in_encounter = c("2", "2", "2", "2", "1"),
    episode_start = structure(c(1438684036, 1438858149, 1439032262, 1439206375, 1439380488), tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    episode_end = structure(c(1438858149, 1439032262, 1439206375, 1439380488, 1439380488), tzone = "Europe/London", class = c("POSIXct", "POSIXt"))
  )
  
  dplyr::copy_to(db_conn, patients, temporary = FALSE)
  dplyr::copy_to(db_conn, encounters, name = "inpatient_episodes", temporary = FALSE)
  
  expect_warning(
    expect_equal(
      .therapy_timeline_get_diagnoses(Patient(db_conn, 99)),
      data.frame()
    ),
    "^The following tables are missing from the Ramses database:"
  )
  
  load_inpatient_diagnoses(
    conn = db_conn,
    diagnoses_data = dplyr::tibble(
      patient_id = 99, 
      encounter_id = 99, 
      episode_number = 1L, 
      icd_code = "N390", 
      diagnosis_position = 2L
    ), 
    diagnoses_lookup = dplyr::tibble(
      icd_code = "N390", icd_display = "N39.0",
      icd_description = "Urinary tract infection, site not specified", 
      category_code = "N39", 
      category_description = "Other disorders of urinary system"
    )
  )
  
  expect_equal(
    .therapy_timeline_get_diagnoses(Patient(db_conn, 99)),
    data.frame(
      id = NA, 
      content = "N39.0 &ndash; Urinary tract infection, site not specified", 
      title = "Urinary tract infection, site not specified", 
      start = structure(1438684036, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      end = structure(1438858149, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      group = 2, subgroup = "u03", type = "range", 
      style = "background-color: #97e2f8", className = "",
      stringsAsFactors = FALSE
    )
  )
  
  load_inpatient_diagnoses(
    conn = db_conn,
    diagnoses_data = dplyr::tibble(
      patient_id = 99, 
      encounter_id = 99, 
      episode_number = c(1L, 3L, 4L), 
      icd_code = "N390", 
      diagnosis_position = 2L
    ), 
    diagnoses_lookup = dplyr::tibble(
      icd_code = "N390", icd_display = "N39.0",
      icd_description = "Urinary tract infection, site not specified", 
      category_code = "N39", 
      category_description = "Other disorders of urinary system"
    ),
    overwrite = TRUE
  )
  
  expect_equal(
    .therapy_timeline_get_diagnoses(Patient(db_conn, 99)),
    data.frame(
      id = NA, 
      content = "N39.0 &ndash; Urinary tract infection, site not specified", 
      title = "Urinary tract infection, site not specified", 
      start = structure(c(1438684036, 1439032262), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      end = structure(c(1438858149, 1439380488), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      group = 2, subgroup = "u03", type = "range", 
      style = "background-color: #97e2f8", className = "",
      stringsAsFactors = FALSE
    )
  )
})

test_that(".therapy_timeline_get_diagnoses (use additional dates)", {
  
  db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(db_conn, shutdown = TRUE)})
  patients <- dplyr::tibble(patient_id = 99)
  encounters <- dplyr::tibble(
    patient_id = 99, encounter_id = 99,
    admission_method = "2",
    admission_date = structure(1438684036, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    discharge_date = structure(1439380488, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    episode_number = 1:5, last_episode_in_encounter = c("2", "2", "2", "2", "1"),
    episode_start = structure(c(1438684036, 1438858149, 1439032262, 1439206375, 1439380488), tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
    episode_end = structure(c(1438858149, 1439032262, 1439206375, 1439380488, 1439380488), tzone = "Europe/London", class = c("POSIXct", "POSIXt"))
  )
  
  dplyr::copy_to(db_conn, patients, temporary = FALSE)
  dplyr::copy_to(db_conn, encounters, name = "inpatient_episodes", temporary = FALSE)
  
  expect_warning(
    expect_equal(
      .therapy_timeline_get_diagnoses(Patient(db_conn, 99)),
      data.frame()
    ),
    "^The following tables are missing from the Ramses database:"
  )
  
  load_inpatient_diagnoses(
    conn = db_conn,
    diagnoses_data = dplyr::tibble(
      patient_id = 99, 
      encounter_id = 99, 
      episode_number = 1L, 
      icd_code = "N390", 
      diagnosis_position = 2L,
      diagnosis_start = structure(1438682036, tzone = "Europe/London", class = c("POSIXct", "POSIXt")),
      diagnosis_end = structure(1438683036, tzone = "Europe/London", class = c("POSIXct", "POSIXt"))
    ), 
    diagnoses_lookup = dplyr::tibble(
      icd_code = "N390", icd_display = "N39.0",
      icd_description = "Urinary tract infection, site not specified", 
      category_code = "N39", 
      category_description = "Other disorders of urinary system"
    )
  )
  
  expect_equal(
    .therapy_timeline_get_diagnoses(Patient(db_conn, 99)),
    data.frame(
      id = NA, 
      content = "N39.0 &ndash; Urinary tract infection, site not specified", 
      title = "Urinary tract infection, site not specified", 
      start = structure(c(1438682036), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      end = structure(c(1438683036), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      group = 2, subgroup = "u03", type = "range", 
      style = "background-color: #97e2f8", className = "",
      stringsAsFactors = FALSE
    )
  )
  
  # missing diagnosis_end
  load_inpatient_diagnoses(
    conn = db_conn,
    diagnoses_data = dplyr::tibble(
      patient_id = 99, 
      encounter_id = 99, 
      episode_number = 1L, 
      icd_code = "N390", 
      diagnosis_position = 2L,
      diagnosis_start = structure(1438682036, tzone = "Europe/London", class = c("POSIXct", "POSIXt"))
    ), 
    diagnoses_lookup = dplyr::tibble(
      icd_code = "N390", icd_display = "N39.0",
      icd_description = "Urinary tract infection, site not specified", 
      category_code = "N39", 
      category_description = "Other disorders of urinary system"
    ),
    overwrite = TRUE
  )
  
  expect_equal(
    .therapy_timeline_get_diagnoses(Patient(db_conn, 99)),
    data.frame(
      id = NA, 
      content = "N39.0 &ndash; Urinary tract infection, site not specified", 
      title = "Urinary tract infection, site not specified", 
      start = structure(c(1438684036), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      end = structure(c(1438858149), class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      group = 2, subgroup = "u03", type = "range", 
      style = "background-color: #97e2f8", className = "",
      stringsAsFactors = FALSE
    )
  )
})


test_that(".therapy_timeline_get_admissions", {
  
  db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(db_conn, shutdown = TRUE)})
  patients <- dplyr::tibble(patient_id = 99,
                            date_of_birth = structure(-1248, class = "Date"), 
                            date_of_death = structure(NA_real_, class = "Date"))
  dplyr::copy_to(db_conn, patients, temporary = FALSE)
  
  expect_warning(
    expect_equal(
      .therapy_timeline_get_admissions(Patient(db_conn, 99)),
      data.frame()
    ),
    "^The following tables are missing from the Ramses database:"
  )
  
  load_inpatient_episodes(
    conn = db_conn,
    patients_data = patients,
    episodes_data = dplyr::tibble(
      patient_id = 99, 
      encounter_id = 99,
      admission_method = "2", 
      admission_date = structure(1438684036, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
      discharge_date = structure(1439380488, tzone = "UTC", class = c("POSIXct", "POSIXt")),
      episode_number = 1L, 
      last_episode_in_encounter = 2, 
      episode_start = structure(1438684036, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
      episode_end = structure(1438858149, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
      consultant_code = "C3400039", 
      main_specialty_code = "340"
    ), 
    overwrite = TRUE
  )
  
  expect_equal(
    .therapy_timeline_get_admissions(Patient(db_conn, 99)),
    data.frame(
      id = NA_character_, 
      content = "Emergency admission", 
      start = structure(1438684036, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      end = structure(1439380488, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      type = "background", className = "admission-emergency",
      stringsAsFactors = FALSE
    )
  )
})

test_that("therapy_timeline No drugs data", {
  
  db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(db_conn, shutdown = TRUE)})
  patients <- dplyr::tibble(patient_id = 99,
                            date_of_birth = structure(-1248, class = "Date"), 
                            date_of_death = structure(NA_real_, class = "Date"))
  dplyr::copy_to(db_conn, patients, temporary = FALSE)
  
  expect_error(
    therapy_timeline(Patient(db_conn, 99)),
    "^The Ramses database must contain a valid `drug_prescriptions` table."
  )
})

test_that(".therapy_timeline_get_micro", {
  
  db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(db_conn, shutdown = TRUE)})
  patients <- dplyr::tibble(patient_id = 99,
                            date_of_birth = structure(-1248, class = "Date"), 
                            date_of_death = structure(NA_real_, class = "Date"))
  dplyr::copy_to(db_conn, patients, temporary = FALSE)
  
  expect_warning(
    expect_equal(
      .therapy_timeline_get_micro(Patient(db_conn, 99)),
      data.frame()
    ),
    "^The following tables are missing from the Ramses database:"
  )
  
  load_inpatient_microbiology(
    conn = db_conn,
    specimens = dplyr::tibble(
      specimen_id = "CCC2", 
      patient_id = 99, 
      status = "available", 
      specimen_datetime = structure(1487030400, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      specimen_type_code = "697989009", 
      specimen_type_name = "Anterior nares swab", 
      specimen_type_display = "MRSA Screen"
    ),
    isolates = dplyr::tibble(
      isolate_id = "2300967", 
      specimen_id = "CCC2", 
      patient_id = 99,
      organism_code = NA_character_, 
      organism_name = NA_character_, 
      organism_display_name = "No growth", 
      isolation_datetime = structure(1487289600, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    ),
    susceptibilities = dplyr::tibble(
      isolate_id = character(0), specimen_id = character(0), 
      patient_id = numeric(0), organism_code = character(0), 
      organism_name = character(0), organism_display_name = character(0), 
      agent_code = character(0), agent_name = character(0), 
      agent_display_name = character(0), 
      rsi_code = character(0), concept_code = character(0)
    )
  )
  
  expect_equal(
    .therapy_timeline_get_micro(Patient(db_conn, 99)),
    dplyr::tibble(
      specimen_id = "CCC2", 
      className = "micro-report-no-growth", 
      id = "Specimen ID:CCC2:report", 
      start = structure(1487289600, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
      title = "MRSA Screen\nOrganisms: No growth\nSample received: 2017-02-14", 
      type = "point", group = 1, subgroup = "micro"
    )
  )
  
})

