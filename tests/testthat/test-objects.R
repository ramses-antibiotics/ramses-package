
# TODO
# con <- dbConnect(RSQLite::SQLite(), "vignettes/ramses-db_perm.sqlite")
# Patient("99999999999", con)
# new("Patient")
# expect_error(Patient(id = "", conn = NULL))

test_that("create patient", {
  patients <- dplyr::tibble(patient_id = "99999999999")
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  dplyr::copy_to(con, patients, temporary = FALSE)
  expect_s4_class(Patient("99999999999", con), "Patient")
})