
test_that("Patient..constructor", {
  patients <- dplyr::tibble(patient_id = "99999999999")
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)})
  
  expect_error(Patient(fake_db_conn, 99),
               "^The Ramses database must contain a valid `patients` table")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_error(Patient(fake_db_conn, NA),
               "`id` must not be NA")
  expect_error(Patient(fake_db_conn, c()),
               "`id` must have length 1")
  expect_error(Patient(fake_db_conn, c("a", "b")),
               "`id` must have length 1")
  patient_object <- Patient(fake_db_conn, "99999999999")
  expect_s4_class(patient_object, "Patient")
  expect_s4_class(compute(patient_object), "Patient")
  expect_is(collect(patient_object), "tbl_df")
  expect_error(Patient(fake_db_conn, 99999999999),
               "`id` must be character")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  # works with integer/numeric
  patients <- dplyr::tibble(patient_id = 999)
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_error(Patient(fake_db_conn, "999"),
               "`id` must be numeric")
  expect_s4_class(Patient(fake_db_conn, 999), "Patient")
  expect_s4_class(Patient(fake_db_conn, 999L), "Patient")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  patients <- dplyr::tibble(patient_id = 999L)
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_error(Patient(fake_db_conn, "999"),
               "`id` must be integer")
  expect_s4_class(Patient(fake_db_conn, 999), "Patient")
  expect_s4_class(Patient(fake_db_conn, 999L), "Patient")
})

test_that("Patient..interface_methods DuckDB", {
  patients <- dplyr::tibble(patient_id = "99999999999")

  conDuckDB <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(conDuckDB, shutdown = TRUE)})
  
  dplyr::copy_to(conDuckDB, patients, temporary = FALSE)
  
  # SHOW
  expect_equal(capture.output(Patient(conDuckDB, "3422481921"))[1],
               "Patient 3422481921 ")
  DBI::dbDisconnect(conDuckDB, shutdown = TRUE)
  
  patients <- dplyr::tibble(patient_id = 99999999999)
  conDuckDB <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(conDuckDB, patients, temporary = FALSE)
  
  patient_object <- Patient(conDuckDB, 99999999999)
  # CLASS
  expect_equal(
    class(patient_object),
    structure("Patient", package = "Ramses")
  )
  
  # SHOW
  expect_equal(capture.output(patient_object)[1],
               "Patient 99999999999 ")
  # COMPUTE 
  expect_equal(
    patient_object@record$lazy_query$x$x,
    structure("patients", class = c("ident", "character"))
  )
  patient_object_computed <- compute(patient_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            patient_object_computed@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(patient_object),
    patients
  )
})


test_that("Patient..interface_methods Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB_testing",
                                  timezone = "UTC")
  on.exit({
    .remove_db_tables(conPostgreSQL, DBI::dbListTables(conPostgreSQL))
    DBI::dbDisconnect(conPostgreSQL)
  })
  
  patients <- dplyr::tibble(patient_id = 99999999999)
  dplyr::copy_to(conPostgreSQL, patients, temporary = FALSE)
  
  patient_object <- Patient(conPostgreSQL, 99999999999)
  # CLASS
  expect_equal(
    class(patient_object),
    structure("Patient", package = "Ramses")
  )
  
  # SHOW
  expect_equal(capture.output(patient_object)[1],
               "Patient 99999999999 ")
  # COMPUTE 
  expect_equal(
    patient_object@record$lazy_query$x$x,
    structure("patients", class = c("ident", "character"))
  )
  patient_object_computed <- compute(patient_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            patient_object_computed@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(patient_object),
    patients
  )
})

test_that(".process_io_parenteral_vector", {
  
  # Perfect IV sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111111111111111111", "")), 5), list(c(0, 19, NA)))
  # Perfect IV sequence before gobbledigook
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111111111111111110011001011", "")), 5), list(c(0, 29, NA)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111111111111111110011001010", "")), 5), list(c(0, 29, NA)))
  # Perfect IV sequence after gobbledigook
  expect_equal(.parenteral_vector_process(unlist(strsplit("001100101011111111111111111111", "")), 5), list(c(10, 29, NA)))
  
  # Perfect IVPO sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111111110000000000", "")), 5), list(c(0, 19, 10)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("00000000011111100000", "")), 4), list(c(9, 19, 15)))
  # Perfect IVPO sequence before gobbledigook
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111111110000000000110011001010", "")), 5), list(c(0, 19, 10)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("00000000011111100000110011001010", "")), 4), list(c(9, 19, 15)))
  # Perfect IVPO sequence after gobbledigook
  expect_equal(.parenteral_vector_process(unlist(strsplit("001100101011111111110000000000", "")), 5), list(c(10, 29, 20)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("001100101000000000011111100000", "")), 4), list(c(19, 29, 25)))
  
  # No sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("000000000000", "")), 4), list())
  # No sequence (with a shot of gentamicin in between)
  expect_equal(.parenteral_vector_process(unlist(strsplit("001000000000", "")), 4), list())
  
  # One needs to start IV for at least 6 hours uninterrupted.
  expect_equal(.parenteral_vector_process(unlist(strsplit("111110001111011", "")), 4), list())
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111001111011", "")), 4), list(c(0, 14, NA)))
  
  # Impact of increasing tolerance from 4 to 5
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111100000111111011", "")), 4), list(c(0, 10, 6), 
                                                                                                 c(11, 19, NA)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111100000111111011", "")), 5), list(c(0, 19, NA)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111#####111111011", "")), 4), list(c(0, 5, NA), 
                                                                                                 c(11, 19, NA)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111#####111111011", "")), 5), list(c(0, 19, NA)))
})


test_that("MedicationRequest..constructor", {
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  
  expect_error(MedicationRequest(fake_db_conn, 99),
               "^The Ramses database must contain a valid `drug_prescriptions` table")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(prescription_id = "999999"),
                 "drug_prescriptions", 
                 temporary = FALSE)
  on.exit({DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)})
  expect_error(MedicationRequest(fake_db_conn, NA),
               "`id` must not be NA")
  expect_error(MedicationRequest(fake_db_conn, c()),
               "`id` must have length 1")
  expect_warning(MedicationRequest(fake_db_conn, c("a", "b")),
                 "`id` must have length 1")
  object <- MedicationRequest(fake_db_conn, "999999")
  expect_s4_class(object, "MedicationRequest")
  expect_error(MedicationRequest(fake_db_conn, 999999),
               "`id` must be character")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  # works with integer/numeric
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(prescription_id = 999L),
                 "drug_prescriptions", 
                 temporary = FALSE)
  expect_error(MedicationRequest(fake_db_conn, "999"),
               "`id` must be integer")
  expect_s4_class(MedicationRequest(fake_db_conn, 999), "MedicationRequest")
  expect_s4_class(MedicationRequest(fake_db_conn, 999L), "MedicationRequest")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(prescription_id = 999),
                 "drug_prescriptions", 
                 temporary = FALSE)
  expect_error(MedicationRequest(fake_db_conn, "999"),
               "`id` must be numeric")
  expect_s4_class(MedicationRequest(fake_db_conn, 999), "MedicationRequest")
  expect_s4_class(MedicationRequest(fake_db_conn, 999L), "MedicationRequest")
})


test_that("MedicationRequest..interface_methods DuckDB", {
  conDuckDB <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(conDuckDB, shutdown = TRUE)})
  
  fake_prescription <- data.frame(
    patient_id = "5124578766",
    prescription_id = 111,
    prescription_text = "Piperacillin / Tazobactam IVI 4.5 g TDS",
    authoring_date = structure(1438690036,
                               class = c("POSIXct", "POSIXt"),
                               tzone = "Europe/London"),
    prescription_start = structure(1438695916,
                                   class = c("POSIXct", "POSIXt"),
                                   tzone = "Europe/London"),
    prescription_end = structure(1438955116,
                                 class = c("POSIXct", "POSIXt"),
                                 tzone = "Europe/London"),
    drug_code = "TZP",
    drug_name = "Piperacillin + Tazobactam",
    drug_display_name = "Piperacillin + Tazobactam",
    drug_group = "Beta-lactams/penicillins",
    prescription_status = "completed",
    route = "IV",
    ATC_route = "P",
    ATC_code = "J01CR05",
    prescription_context = "inpatient",
    dose = 4.5,
    unit = "g",
    frequency = "TDS",
    daily_frequency = 3,
    duration = 3,
    antiinfective_type = "antibacterial",
    stringsAsFactors = FALSE
  )
  
  dplyr::copy_to(conDuckDB, df = dplyr::tibble(patient_id="5124578766"), 
                 name = "patients", temporary = FALSE)
  load_medications(conn = conDuckDB,
                   prescriptions = fake_prescription,
                   overwrite = TRUE)
  med_req_object <- MedicationRequest(conDuckDB, 111)
  
  # CLASS
  expect_equal(
    class(med_req_object),
    structure("MedicationRequest", package = "Ramses")
  )
  
  # SHOW
  expect_equal(
    capture.output(med_req_object)[1:2],
    c("MedicationRequest 111 ", "Piperacillin / Tazobactam IVI 4.5 g TDS ")
  )
  
  # COMPUTE 
  expect_equal(
    med_req_object@record$lazy_query$x$x,
    structure("drug_prescriptions", class = c("ident", "character"))
  )
  med_req_object_computed <- compute(med_req_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            med_req_object_computed@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(med_req_object),
    dplyr::tibble(id = 1L, patient_id = "5124578766", prescription_id = 111,
               combination_id = NA_real_, therapy_id = 111, therapy_rank = 1L,
               prescription_text = "Piperacillin / Tazobactam IVI 4.5 g TDS",
               drug_code = "TZP", drug_name = "Piperacillin + Tazobactam",
               drug_display_name = "Piperacillin + Tazobactam", drug_group = "Beta-lactams/penicillins",
               antiinfective_type = "antibacterial", ATC_code = "J01CR05",
               ATC_route = "P", dose = 4.5, unit = "g", route = "IV", frequency = "TDS",
               authoring_date = structure(1438690036, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
               prescription_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
               prescription_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
               prescription_context = "inpatient",
               prescription_status = "completed", daily_frequency = 3, duration = 3)
  )
  
  # PATIENT
  expect_s4_class(
    Patient(med_req_object),
    "Patient"
  )
})

test_that("MedicationRequest..interface_methods Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB_testing",
                                  timezone = "UTC")
  on.exit({
    .remove_db_tables(conPostgreSQL, DBI::dbListTables(conPostgreSQL))
    DBI::dbDisconnect(conPostgreSQL)
  })
  
  fake_prescription <- data.frame(
    patient_id = "5124578766",
    prescription_id = 111,
    prescription_text = "Piperacillin / Tazobactam IVI 4.5 g TDS",
    authoring_date = structure(1438690036,
                               class = c("POSIXct", "POSIXt"),
                               tzone = "Europe/London"),
    prescription_start = structure(1438695916,
                                   class = c("POSIXct", "POSIXt"),
                                   tzone = "Europe/London"),
    prescription_end = structure(1438955116,
                                 class = c("POSIXct", "POSIXt"),
                                 tzone = "Europe/London"),
    drug_code = "TZP",
    drug_name = "Piperacillin + Tazobactam",
    drug_display_name = "Piperacillin + Tazobactam",
    drug_group = "Beta-lactams/penicillins",
    prescription_status = "completed",
    route = "IV",
    ATC_route = "P",
    ATC_code = "J01CR05",
    prescription_context = "inpatient",
    dose = 4.5,
    unit = "g",
    frequency = "TDS",
    daily_frequency = 3,
    duration = 3,
    antiinfective_type = "antibacterial",
    stringsAsFactors = FALSE
  )
  dplyr::copy_to(conPostgreSQL, df = dplyr::tibble(patient_id="5124578766"), 
                 name = "patients", temporary = FALSE)
  load_medications(conn = conPostgreSQL,
                   prescriptions = fake_prescription,
                   overwrite = TRUE)
  med_req_object <- MedicationRequest(conPostgreSQL, 111)
  
  # CLASS
  expect_equal(
    class(med_req_object),
    structure("MedicationRequest", package = "Ramses")
  )
  
  # SHOW
  expect_equal(
    capture.output(med_req_object)[1:2],
    c("MedicationRequest 111 ", "Piperacillin / Tazobactam IVI 4.5 g TDS ")
  )
  
  # COMPUTE 
  expect_equal(
    med_req_object@record$lazy_query$x$x,
    structure("drug_prescriptions", class = c("ident", "character"))
  )
  med_req_object_computed <- compute(med_req_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            med_req_object_computed@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(med_req_object),
    dplyr::tibble(id = 1L, patient_id = "5124578766", prescription_id = 111,
                  combination_id = NA_real_, therapy_id = 111, therapy_rank = 1L,
                  prescription_text = "Piperacillin / Tazobactam IVI 4.5 g TDS",
                  drug_code = "TZP", drug_name = "Piperacillin + Tazobactam",
                  drug_display_name = "Piperacillin + Tazobactam", drug_group = "Beta-lactams/penicillins",
                  antiinfective_type = "antibacterial", ATC_code = "J01CR05",
                  ATC_route = "P", dose = 4.5, unit = "g", route = "IV", frequency = "TDS",
                  authoring_date = structure(1438690036, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
                  prescription_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
                  prescription_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                  prescription_context = "inpatient",
                  prescription_status = "completed", daily_frequency = 3, duration = 3)
  )
  
  # PATIENT
  expect_s4_class(
    Patient(med_req_object),
    "Patient"
  )
})

test_that("TherapyEpisode..constructor", {
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)})
  
  expect_error(TherapyEpisode(fake_db_conn, 99),
               "^The Ramses database must contain a valid `drug_therapy_episodes` table")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(
                   patient_id = "9",
                   therapy_id = "999999"
                 ),
                 "drug_therapy_episodes", 
                 temporary = FALSE)
  expect_error(TherapyEpisode(fake_db_conn, NA),
               "`id` must contain at least one identifier")
  expect_error(TherapyEpisode(fake_db_conn, c()),
               "`id` must contain at least one identifier")
  expect_error(TherapyEpisode(fake_db_conn, 999999),
               "`id` must be character")
  expect_error(TherapyEpisode(fake_db_conn, 999999L),
               "`id` must be character")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  # works with integer/numeric
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(patient_id = 9L,
                               therapy_id = 999999L),
                 "drug_therapy_episodes", 
                 temporary = FALSE)
  expect_error(TherapyEpisode(fake_db_conn, "999999"),
               "`id` must be integer")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(patient_id = 9,
                               therapy_id = 999999),
                 "drug_therapy_episodes", 
                 temporary = FALSE)
  expect_error(TherapyEpisode(fake_db_conn, "999999"))
})


test_that("TherapyEpisode..interface_methods DuckDB", {
  conDuckDB <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(conDuckDB, shutdown = TRUE)})
  
  fake_prescription <- data.frame(
    patient_id = c("5124578766", "5124578767", "5124578768", "5124578769"),
    prescription_id = 111:114,
    prescription_text = "Piperacillin / Tazobactam IVI 4.5 g TDS",
    authoring_date = structure(1438690036,
                               class = c("POSIXct", "POSIXt"),
                               tzone = "Europe/London"),
    prescription_start = structure(1438695916,
                                   class = c("POSIXct", "POSIXt"),
                                   tzone = "Europe/London"),
    prescription_end = structure(1438955116,
                                 class = c("POSIXct", "POSIXt"),
                                 tzone = "Europe/London"),
    drug_code = "TZP",
    drug_name = "Piperacillin + Tazobactam",
    drug_display_name = "Piperacillin + Tazobactam",
    drug_group = "Beta-lactams/penicillins",
    prescription_status = "completed",
    route = "IV",
    ATC_route = "P",
    ATC_code = "J01CR05",
    prescription_context = "inpatient",
    dose = 4.5,
    unit = "g",
    frequency = "TDS",
    daily_frequency = 3,
    duration = 3,
    antiinfective_type = "antibacterial",
    stringsAsFactors = FALSE
  )
  dplyr::copy_to(conDuckDB, df = dplyr::tibble(patient_id="5124578766"), 
                 name = "patients", temporary = FALSE)
  load_medications(conn = conDuckDB,
                   prescriptions = fake_prescription,
                   overwrite = TRUE)
  therapy_object <- TherapyEpisode(conDuckDB, 111)
  therapy_object_multi <- TherapyEpisode(conDuckDB, 111:113)
  therapy_object_multi4 <- TherapyEpisode(conDuckDB, 111:114)
  
  # CLASS
  expect_equal(
    class(therapy_object),
    structure("TherapyEpisode", package = "Ramses")
  )
  expect_equal(
    class(therapy_object_multi),
    structure("TherapyEpisode", package = "Ramses")
  )
  expect_equal(
    class(therapy_object_multi4),
    structure("TherapyEpisode", package = "Ramses")
  )
  
  # SHOW
  expect_equal(
    capture.output(therapy_object)[1:2],
    c("TherapyEpisode 111 ", "Patient:   5124578766 ")
  )
  expect_equal(
    capture.output(therapy_object_multi)[1:2],
    c("TherapyEpisode 111, 112, 113 ", "[total of 3 therapy episodes]")
  )
  expect_equal(
    capture.output(therapy_object_multi4)[1:2],
    c("TherapyEpisode 111, 112, 113 ...", "[total of 4 therapy episodes]")
  )
  
  # COMPUTE 
  expect_equal(
    therapy_object@record$lazy_query$x$x,
    structure("drug_therapy_episodes", class = c("ident", "character"))
  )
  therapy_object_computed <- compute(therapy_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            therapy_object_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    therapy_object_multi@record$lazy_query$x$x,
    structure("drug_therapy_episodes", class = c("ident", "character"))
  )
  therapy_object_multi_computed <- compute(therapy_object_multi)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            therapy_object_multi_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    therapy_object_multi4@record$lazy_query$x$x,
    structure("drug_therapy_episodes", class = c("ident", "character"))
  )
  therapy_object_multi_computed4 <- compute(therapy_object_multi4)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            therapy_object_multi_computed4@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(therapy_object),
    dplyr::tibble(
      patient_id = "5124578766", 
      therapy_id = 111, 
      antiinfective_type = "antibacterial", 
      therapy_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      therapy_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
  )
  expect_equal(
    collect(therapy_object_multi),
    dplyr::tibble(
      patient_id = c("5124578766", "5124578767", "5124578768"), 
      therapy_id = 111:113, 
      antiinfective_type = "antibacterial", 
      therapy_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      therapy_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
  )
  expect_equal(
    collect(therapy_object_multi4),
    dplyr::tibble(
      patient_id = c("5124578766", "5124578767", "5124578768", "5124578769"), 
      therapy_id = 111:114, 
      antiinfective_type = "antibacterial", 
      therapy_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      therapy_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
  )
  
  # PATIENT
  expect_s4_class(
    Patient(therapy_object),
    "Patient"
  )
  expect_error(
    Patient(therapy_object_multi),
    "`id` must have length 1"
  )
  expect_error(
    Patient(therapy_object_multi4),
    "`id` must have length 1"
  )
})

test_that("TherapyEpisode..interface_methods Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB_testing",
                                  timezone = "UTC")
  on.exit({
    .remove_db_tables(conPostgreSQL, DBI::dbListTables(conPostgreSQL))
    DBI::dbDisconnect(conPostgreSQL)
  })
  
  fake_prescription <- data.frame(
    patient_id = c("5124578766", "5124578767", "5124578768", "5124578769"),
    prescription_id = 111:114,
    prescription_text = "Piperacillin / Tazobactam IVI 4.5 g TDS",
    authoring_date = structure(1438690036,
                               class = c("POSIXct", "POSIXt"),
                               tzone = "Europe/London"),
    prescription_start = structure(1438695916,
                                   class = c("POSIXct", "POSIXt"),
                                   tzone = "Europe/London"),
    prescription_end = structure(1438955116,
                                 class = c("POSIXct", "POSIXt"),
                                 tzone = "Europe/London"),
    drug_code = "TZP",
    drug_name = "Piperacillin + Tazobactam",
    drug_display_name = "Piperacillin + Tazobactam",
    drug_group = "Beta-lactams/penicillins",
    prescription_status = "completed",
    route = "IV",
    ATC_route = "P",
    ATC_code = "J01CR05",
    prescription_context = "inpatient",
    dose = 4.5,
    unit = "g",
    frequency = "TDS",
    daily_frequency = 3,
    duration = 3,
    antiinfective_type = "antibacterial",
    stringsAsFactors = FALSE
  )
  dplyr::copy_to(conPostgreSQL, df = dplyr::tibble(patient_id="5124578766"), 
                 name = "patients", temporary = FALSE)
  load_medications(conn = conPostgreSQL,
                   prescriptions = fake_prescription,
                   overwrite = TRUE)
  therapy_object <- TherapyEpisode(conPostgreSQL, 111)
  therapy_object_multi <- TherapyEpisode(conPostgreSQL, 111:113)
  therapy_object_multi4 <- TherapyEpisode(conPostgreSQL, 111:114)
  
  # CLASS
  expect_equal(
    class(therapy_object),
    structure("TherapyEpisode", package = "Ramses")
  )
  expect_equal(
    class(therapy_object_multi),
    structure("TherapyEpisode", package = "Ramses")
  )
  expect_equal(
    class(therapy_object_multi4),
    structure("TherapyEpisode", package = "Ramses")
  )
  
  # SHOW
  expect_equal(
    capture.output(therapy_object)[1:2],
    c("TherapyEpisode 111 ", "Patient:   5124578766 ")
  )
  expect_equal(
    capture.output(therapy_object_multi)[1:2],
    c("TherapyEpisode 111, 112, 113 ", "[total of 3 therapy episodes]")
  )
  expect_equal(
    capture.output(therapy_object_multi4)[1:2],
    c("TherapyEpisode 111, 112, 113 ...", "[total of 4 therapy episodes]")
  )
  
  # COMPUTE 
  expect_equal(
    therapy_object@record$lazy_query$x$x,
    structure("drug_therapy_episodes", class = c("ident", "character"))
  )
  therapy_object_computed <- compute(therapy_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            therapy_object_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    therapy_object_multi@record$lazy_query$x$x,
    structure("drug_therapy_episodes", class = c("ident", "character"))
  )
  therapy_object_multi_computed <- compute(therapy_object_multi)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            therapy_object_multi_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    therapy_object_multi4@record$lazy_query$x$x,
    structure("drug_therapy_episodes", class = c("ident", "character"))
  )
  therapy_object_multi_computed4 <- compute(therapy_object_multi4)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            therapy_object_multi_computed4@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(therapy_object),
    dplyr::tibble(
      patient_id = "5124578766", 
      therapy_id = 111, 
      antiinfective_type = "antibacterial", 
      therapy_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      therapy_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
  )
  expect_equal(
    collect(therapy_object_multi),
    dplyr::tibble(
      patient_id = c("5124578766", "5124578767", "5124578768"), 
      therapy_id = 111:113, 
      antiinfective_type = "antibacterial", 
      therapy_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      therapy_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
  )
  expect_equal(
    collect(therapy_object_multi4),
    dplyr::tibble(
      patient_id = c("5124578766", "5124578767", "5124578768", "5124578769"), 
      therapy_id = 111:114, 
      antiinfective_type = "antibacterial", 
      therapy_start = structure(1438695916, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
      therapy_end = structure(1438955116, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
  )
  
  # PATIENT
  expect_s4_class(
    Patient(therapy_object),
    "Patient"
  )
  expect_error(
    Patient(therapy_object_multi),
    "`id` must have length 1"
  )
  expect_error(
    Patient(therapy_object_multi4),
    "`id` must have length 1"
  )
})

test_that("Encounter..constructor", {
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)})
  
  expect_error(Encounter(fake_db_conn, 99),
               "^The Ramses database must contain a valid `inpatient_episodes` table")
  dplyr::copy_to(
    fake_db_conn, 
    dplyr::tibble(
      patient_id = "9",
      encounter_id = "999999", 
      admission_date = Sys.Date(), 
      discharge_date = Sys.Date()
    ),
    "inpatient_episodes", 
    temporary = FALSE
  )
  expect_error(Encounter(fake_db_conn, NA),
               "`id` must contain at least one identifier")
  expect_error(Encounter(fake_db_conn, c()),
               "`id` must contain at least one identifier")
  expect_error(Encounter(fake_db_conn, 999999),
               "`id` must be character")
  expect_error(Encounter(fake_db_conn, 999999L),
               "`id` must be character")
  expect_s4_class(Encounter(fake_db_conn, "999999"), "Encounter")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  # works with integer/numeric
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(
    fake_db_conn, 
    dplyr::tibble(
      patient_id = 9,
      encounter_id = 999999,
      admission_date = Sys.Date(), 
      discharge_date = Sys.Date()
    ),
    "inpatient_episodes", 
    temporary = FALSE
  )
  expect_error(Encounter(fake_db_conn, "999999"), 
               "`id` must be numeric")
  expect_s4_class(Encounter(fake_db_conn, 999999), "Encounter")
  expect_s4_class(Encounter(fake_db_conn, 999999L), "Encounter")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  dplyr::copy_to(
    fake_db_conn, 
    dplyr::tibble(
      patient_id = 9L,
      encounter_id = 999999L,
      admission_date = Sys.Date(), 
      discharge_date = Sys.Date()
    ),
    "inpatient_episodes", 
    temporary = FALSE
  )
  expect_error(Encounter(fake_db_conn, "999999"),
               "`id` must be integer")
  expect_s4_class(Encounter(fake_db_conn, 999999), "Encounter")
  expect_s4_class(Encounter(fake_db_conn, 999999L), "Encounter")
})


test_that("Encounter..interface_methods DuckDB", {
  conDuckDB <- DBI::dbConnect(duckdb::duckdb(), ":memory:", timezone_out = "UTC")
  on.exit({DBI::dbDisconnect(conDuckDB, shutdown = TRUE)})
  
  fake_encounters <- dplyr::tibble(
    patient_id = 6145252493, 
    encounter_id = 5458286195:5458286199, 
    admission_method = "1", 
    admission_date = structure(1443082402, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    discharge_date = structure(1443118601, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    episode_number = 1L, 
    last_episode_in_encounter = 1, 
    episode_start = structure(1443082402, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    episode_end = structure(1443118601, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    consultant_code = "C1000003", 
    main_specialty_code = "100"
  )
  dplyr::copy_to(conDuckDB, df = dplyr::tibble(patient_id=6145252493), 
                 name = "patients", temporary = FALSE)
  load_inpatient_episodes(
    conn = conDuckDB,
    patients_data = dplyr::tibble(patient_id = 6145252493),
    episodes_data = fake_encounters,
    overwrite = TRUE
  )
  encounter_object <- Encounter(conDuckDB, 5458286195)
  encounter_object_multi <- Encounter(conDuckDB, 5458286195:5458286196)
  encounter_object_multi5 <- Encounter(conDuckDB, 5458286195:5458286199)
  
  # CLASS
  expect_equal(
    class(encounter_object),
    structure("Encounter", package = "Ramses")
  )
  
  # SHOW
  expect_equal(
    capture.output(encounter_object)[1:2],
    c("Encounter 5458286195 ", "Patient:   6145252493 ")
  )
  expect_equal(
    capture.output(encounter_object_multi)[1:3],
    c("Encounters 5458286195, 5458286196 ", "[total of 2 encounters]", "Patient(s):   6145252493 ")
  )
  expect_equal(
    capture.output(encounter_object_multi5)[1:3],
    c("Encounters 5458286195, 5458286196, 5458286197 ...", 
      "[total of 5 encounters]", 
      "Patient(s):   6145252493 ")
  )
  
  # COMPUTE 
  expect_equal(
    encounter_object@record$lazy_query$x$x,
    structure("inpatient_episodes", class = c("ident", "character"))
  )
  encounter_object_computed <- compute(encounter_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            encounter_object_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    encounter_object_multi@record$lazy_query$x$x,
    structure("inpatient_episodes", class = c("ident", "character"))
  )
  encounter_object_multi_computed <- compute(encounter_object_multi)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            encounter_object_multi_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    encounter_object_multi5@record$lazy_query$x$x,
    structure("inpatient_episodes", class = c("ident", "character"))
  )
  encounter_object_multi5_computed <- compute(encounter_object_multi5)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            encounter_object_multi5_computed@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(encounter_object),
    dplyr::tibble(
      patient_id = 6145252493, 
      encounter_id = 5458286195, 
      admission_method = "1", 
      admission_date = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      discharge_date = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_number = 1L, 
      last_episode_in_encounter = 1, 
      episode_start = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_end = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      consultant_code = "C1000003", 
      main_specialty_code = "100", 
      ramses_bed_days = 0.418969907407407
    )
  )
  expect_equal(
    collect(encounter_object_multi),
    dplyr::tibble(
      patient_id = 6145252493, 
      encounter_id = 5458286195:5458286196, 
      admission_method = "1", 
      admission_date = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      discharge_date = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_number = 1L, 
      last_episode_in_encounter = 1, 
      episode_start = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_end = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      consultant_code = "C1000003", 
      main_specialty_code = "100", 
      ramses_bed_days = 0.418969907407407
    )
  )
  expect_equal(
    collect(encounter_object_multi5),
    dplyr::tibble(
      patient_id = 6145252493, 
      encounter_id = 5458286195:5458286199, 
      admission_method = "1", 
      admission_date = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      discharge_date = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_number = 1L, 
      last_episode_in_encounter = 1, 
      episode_start = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_end = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      consultant_code = "C1000003", 
      main_specialty_code = "100", 
      ramses_bed_days = 0.418969907407407
    )
  )
  # PATIENT
  expect_s4_class(
    Patient(encounter_object),
    "Patient"
  )
  expect_s4_class(
    Patient(encounter_object_multi),
    "Patient"
  )
  expect_s4_class(
    Patient(encounter_object_multi5),
    "Patient"
  )
  
  # Deprecation of therapy_table method
  expect_warning(
    forget <- therapy_table(encounter_object),
    "^'therapy_table' is deprecated[.]"
  )
})


test_that("Encounter..interface_methods Postgres", {
  
  if (!identical(Sys.getenv("CI_Postgres"), "true")) {
    skip("CI_Postgres is set to false")
  }
  
  conPostgreSQL <- DBI::dbConnect(RPostgres::Postgres(),
                                  user = "user", 
                                  password = "password",
                                  host = "localhost", 
                                  dbname="RamsesDB_testing",
                                  timezone = "UTC")
  on.exit({
    .remove_db_tables(conPostgreSQL, DBI::dbListTables(conPostgreSQL))
    DBI::dbDisconnect(conPostgreSQL)
  })
  
  fake_encounters <- dplyr::tibble(
    patient_id = 6145252493, 
    encounter_id = 5458286195:5458286199, 
    admission_method = "1", 
    admission_date = structure(1443082402, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    discharge_date = structure(1443118601, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    episode_number = 1L, 
    last_episode_in_encounter = 1, 
    episode_start = structure(1443082402, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    episode_end = structure(1443118601, tzone = "UTC", class = c("POSIXct", "POSIXt")), 
    consultant_code = "C1000003", 
    main_specialty_code = "100"
  )
  dplyr::copy_to(conPostgreSQL, df = dplyr::tibble(patient_id=6145252493), 
                 name = "patients", temporary = FALSE)
  load_inpatient_episodes(
    conn = conPostgreSQL,
    patients_data = dplyr::tibble(patient_id = 6145252493),
    episodes_data = fake_encounters,
    overwrite = TRUE
  )
  encounter_object <- Encounter(conPostgreSQL, 5458286195)
  encounter_object_multi <- Encounter(conPostgreSQL, 5458286195:5458286196)
  encounter_object_multi5 <- Encounter(conPostgreSQL, 5458286195:5458286199)
  
  # CLASS
  expect_equal(
    class(encounter_object),
    structure("Encounter", package = "Ramses")
  )
  
  # SHOW
  expect_equal(
    capture.output(encounter_object)[1:2],
    c("Encounter 5458286195 ", "Patient:   6145252493 ")
  )
  expect_equal(
    capture.output(encounter_object_multi)[1:3],
    c("Encounters 5458286195, 5458286196 ", "[total of 2 encounters]", "Patient(s):   6145252493 ")
  )
  expect_equal(
    capture.output(encounter_object_multi5)[1:3],
    c("Encounters 5458286195, 5458286196, 5458286197 ...", 
      "[total of 5 encounters]", 
      "Patient(s):   6145252493 ")
  )
  
  # COMPUTE 
  expect_equal(
    encounter_object@record$lazy_query$x$x,
    structure("inpatient_episodes", class = c("ident", "character"))
  )
  encounter_object_computed <- compute(encounter_object)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            encounter_object_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    encounter_object_multi@record$lazy_query$x$x,
    structure("inpatient_episodes", class = c("ident", "character"))
  )
  encounter_object_multi_computed <- compute(encounter_object_multi)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            encounter_object_multi_computed@record$lazy_query$x
          ))
  )
  expect_equal(
    encounter_object_multi5@record$lazy_query$x$x,
    structure("inpatient_episodes", class = c("ident", "character"))
  )
  encounter_object_multi5_computed <- compute(encounter_object_multi5)
  expect_true(
    grepl("^dbplyr_", 
          as.character(
            encounter_object_multi5_computed@record$lazy_query$x
          ))
  )
  
  # COLLECT
  expect_equal(
    collect(encounter_object),
    dplyr::tibble(
      patient_id = 6145252493, 
      encounter_id = 5458286195, 
      admission_method = "1", 
      admission_date = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      discharge_date = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_number = 1L, 
      last_episode_in_encounter = 1, 
      episode_start = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_end = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      consultant_code = "C1000003", 
      main_specialty_code = "100", 
      ramses_bed_days = 0.418969907407407
    )
  )
  expect_equal(
    collect(encounter_object_multi),
    dplyr::tibble(
      patient_id = 6145252493, 
      encounter_id = 5458286195:5458286196, 
      admission_method = "1", 
      admission_date = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      discharge_date = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_number = 1L, 
      last_episode_in_encounter = 1, 
      episode_start = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_end = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      consultant_code = "C1000003", 
      main_specialty_code = "100", 
      ramses_bed_days = 0.418969907407407
    )
  )
  expect_equal(
    collect(encounter_object_multi5),
    dplyr::tibble(
      patient_id = 6145252493, 
      encounter_id = 5458286195:5458286199, 
      admission_method = "1", 
      admission_date = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      discharge_date = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_number = 1L, 
      last_episode_in_encounter = 1, 
      episode_start = structure(1443082402, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      episode_end = structure(1443118601, class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
      consultant_code = "C1000003", 
      main_specialty_code = "100", 
      ramses_bed_days = 0.418969907407407
    )
  )
  # PATIENT
  expect_s4_class(
    Patient(encounter_object),
    "Patient"
  )
  expect_s4_class(
    Patient(encounter_object_multi),
    "Patient"
  )
  expect_s4_class(
    Patient(encounter_object_multi5),
    "Patient"
  )
  
  # Deprecation of therapy_table method
  expect_warning(
    forget <- therapy_table(encounter_object),
    "^'therapy_table' is deprecated[.]"
  )
})

test_that(".validate_extended_table_input", {
  invalid_input <- 1:2
  expect_error(
    .validate_extended_table_input(invalid_input),
    "`invalid_input` must be a numeric or integer of length 1"
  )
  expect_error(
    .validate_extended_table_input(-1)
  )
  expect_error(
    .validate_extended_table_input("1")
  )
  expect_equal(
    .validate_extended_table_input(NULL),
    0
  )
  expect_equal(
    .validate_extended_table_input(NA),
    0
  )
  expect_equal(
    .validate_extended_table_input(1),
    1
  )
  expect_equal(
    .validate_extended_table_input(1L),
    1
  )
  expect_equal(
    .validate_extended_table_input(1.1),
    2
  )
  expect_equal(
    .validate_extended_table_input(1.9),
    2
  )
  expect_equal(
    .validate_extended_table_input(0.1),
    1
  )
  expect_equal(
    .validate_extended_table_input(0),
    0
  )
})
