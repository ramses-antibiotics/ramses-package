
test_that("Patient..constructor", {
  patients <- dplyr::tibble(patient_id = "99999999999")
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_error(Patient(fake_db_conn, NA))
  expect_error(Patient(fake_db_conn, c()))
  expect_error(Patient(fake_db_conn, c("a", "b")))
  patient_object <- Patient(fake_db_conn, "99999999999")
  expect_s4_class(patient_object, "Patient")
  expect_s4_class(compute(patient_object), "Patient")
  expect_is(collect(patient_object), "tbl_df")
  expect_error(Patient(fake_db_conn, 99999999999))
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  # works with integer/numeric
  patients <- dplyr::tibble(patient_id = 999)
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_error(Patient(fake_db_conn, "999"))
  expect_s4_class(Patient(fake_db_conn, 999), "Patient")
  expect_s4_class(Patient(fake_db_conn, 999L), "Patient")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  patients <- dplyr::tibble(patient_id = 999L)
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_error(Patient(fake_db_conn, "999"))
  expect_s4_class(Patient(fake_db_conn, 999), "Patient")
  expect_s4_class(Patient(fake_db_conn, 999L), "Patient")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
})

test_that("Patient..show", {
  patients <- dplyr::tibble(patient_id = "99999999999")
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_equal(capture.output(Patient(fake_db_conn, "3422481921"))[1],
               "Patient 3422481921 ")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  patients <- dplyr::tibble(patient_id = 99999999999)
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, patients, temporary = FALSE)
  expect_equal(capture.output(Patient(fake_db_conn, 99999999999))[1],
               "Patient 99999999999 ")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
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
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(prescription_id = "999999"),
                 "drug_prescriptions", 
                 temporary = FALSE)
  expect_error(MedicationRequest(fake_db_conn, NA))
  expect_error(MedicationRequest(fake_db_conn, c()))
  expect_warning(MedicationRequest(fake_db_conn, c("a", "b")))
  object <- MedicationRequest(fake_db_conn, "999999")
  expect_s4_class(object, "MedicationRequest")
  expect_error(MedicationRequest(fake_db_conn, 999999))
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  # works with integer/numeric
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(prescription_id = 999L),
                 "drug_prescriptions", 
                 temporary = FALSE)
  expect_error(MedicationRequest(fake_db_conn, "999"))
  expect_s4_class(MedicationRequest(fake_db_conn, 999), "MedicationRequest")
  expect_s4_class(MedicationRequest(fake_db_conn, 999L), "MedicationRequest")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(prescription_id = 999),
                 "drug_prescriptions", 
                 temporary = FALSE)
  expect_error(MedicationRequest(fake_db_conn, "999"))
  expect_s4_class(MedicationRequest(fake_db_conn, 999), "MedicationRequest")
  expect_s4_class(MedicationRequest(fake_db_conn, 999L), "MedicationRequest")
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
})



test_that("TherapyEpisode..constructor", {
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(therapy_id = "999999"),
                 "drug_therapy_episodes", 
                 temporary = FALSE)
  expect_error(TherapyEpisode(fake_db_conn, NA))
  expect_error(TherapyEpisode(fake_db_conn, c()))
  expect_error(TherapyEpisode(fake_db_conn, 999999))
  expect_error(TherapyEpisode(fake_db_conn, 999999L))
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  # works with integer/numeric
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(therapy_id = 999999L),
                 "drug_therapy_episodes", 
                 temporary = FALSE)
  expect_error(TherapyEpisode(fake_db_conn, "999999"))
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
  
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dplyr::copy_to(fake_db_conn, 
                 dplyr::tibble(therapy_id = 999999),
                 "drug_therapy_episodes", 
                 temporary = FALSE)
  expect_error(TherapyEpisode(fake_db_conn, "999999"))
  DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)
})