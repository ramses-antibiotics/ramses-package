
# TODO
# con <- dbConnect(RSQLite::SQLite(), "vignettes/ramses-db_perm.sqlite")
# Patient("99999999999", con)
# new("Patient")
# expect_error(Patient(id = "", conn = NULL))

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


 
# ramses_db <- connect_local_database("vignettes/ramses-db_perm.sqlite")
 