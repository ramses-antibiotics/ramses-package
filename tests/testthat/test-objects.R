
# ramses_db <- connect_local_database("vignettes/ramses-db_perm.sqlite")
# pts <- dplyr::collect(dplyr::distinct(tbl(conSQLite, "drug_prescriptions"), patient_id))
# onept <- sample(pts$patient_id, 1)
# therapy_timeline(conSQLite, onept)
# dplyr::filter(tbl(conSQLite, "drug_therapy_episodes"), patient_id == onept)

test_that("create patient", {
  patients <- dplyr::tibble(patient_id = "99999999999")
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  dplyr::copy_to(con, patients, temporary = FALSE)
  expect_error(Patient(con, ""))
  patient_object <- Patient(con, "99999999999")
  expect_s4_class(patient_object, "Patient")
  expect_s4_class(compute(patient_object), "Patient")
  expect_is(collect(patient_object), "tbl_df")
})

test_that(".process_io_parenteral_vector", {
  
  # Perfect IV sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111111111111111111", "")), 5), list(c(1, 20)))
  # Perfect IVPO sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111111110000000000", "")), 5), list(c(1, 20)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("00000000011111100000", "")), 4), list(c(10, 20)))
  
  # No sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("000000000000", "")), 4), list())
  # No sequence (with a shot of gentamicin in between)
  expect_equal(.parenteral_vector_process(unlist(strsplit("001000000000", "")), 4), list())
  
  # One needs to start IV for at least 6 hours uninterrupted.
  expect_equal(.parenteral_vector_process(unlist(strsplit("111110001111011", "")), 4), list())
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111001111011", "")), 4), list(c(1, 15)))
  
  # Impact of increasing tolerance from 4 to 5
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111100000111111011", "")), 4), list(c(1, 11), c(12, 20)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111100000111111011", "")), 5), list(c(1, 20)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111#####111111011", "")), 4), list(c(1, 11), c(12, 20)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111#####111111011", "")), 5), list(c(1, 20)))
  
})

