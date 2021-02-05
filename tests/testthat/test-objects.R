
# TODO
# con <- dbConnect(RSQLite::SQLite(), "vignettes/ramses-db_perm1sqlite")
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


 
# ramses_db <- connect_local_database("vignettes/ramses-db_perm1sqlite")

test_that("1process_io_parenteral_vector", {
  
  # Perfect IV sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111111111111111111", "")), 5), list(c(1, 20)))
  # Perfect IVPO sequence
  expect_equal(.parenteral_vector_process(unlist(strsplit("11111111110000000000", "")), 5), list(c(1, 20)))
  
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
  
  
  
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111111100", "")), 5), list(c(1, 12)))
  
  
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111#####111111011", "")), 5), list(c(1, 20)))
  
  
  
  
  
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111100000000000000000", ""))), list(c(1, 13)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("111110011110110", ""))), list(c(1, 15)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("1111100111101100", ""))), list(c(1, 16)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("001110000000", "")), 4), list())
  expect_equal(.parenteral_vector_process(unlist(strsplit("001111000000", "")), 4), list(c(3, 12)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("00111100000011111", ""))), list(c(3, 12), c(13, 17)))
  expect_equal(.parenteral_vector_process(unlist(strsplit("111111110011110111", ""))), list(c(1, 18)))
  

})



# expect_equal(1parenteral_vector_process(unlist(strsplit("000000011111111111111111", ""))), list(c(1, 13)))
# expect_equal(1parenteral_vector_process(unlist(strsplit("00000110000100", ""))), list(c(1, 14)))
# expect_equal(1parenteral_vector_process(unlist(strsplit("000001100001001", ""))), list(c(1, 15)))
# expect_equal(1parenteral_vector_process(unlist(strsplit("0000011000010011", ""))), list(c(1, 16)))
# expect_equal(1parenteral_vector_process(unlist(strsplit("0000011000010001111111", ""))), list(c(1,21)))
# expect_equal(1parenteral_vector_process(unlist(strsplit("110111111111", ""))), list())
# expect_equal(1parenteral_vector_process(unlist(strsplit("110001111111", ""))), list())
# expect_equal(1parenteral_vector_process(unlist(strsplit("110000111111", ""))), list(c(3, 12)))
# expect_equal(1parenteral_vector_process(unlist(strsplit("11000011111100000", ""))), list(c(3, 12), c(13, 17)))
# expect_equal(1parenteral_vector_process(unlist(strsplit("000000001100001000", ""))), list(c(1, 18)))