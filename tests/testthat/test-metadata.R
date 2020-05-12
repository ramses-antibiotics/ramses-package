
test_that("ICD-10-CM downloads and maps to Charlson", {
  icd10cm <- download_icd10cm()
  expect_equal(icd10cm$icd_code[1], "A000")
  icd10_charlson <- map_charlson_comorbidities(df = icd10cm, icd_column = "icd_code")
  expect_true(icd10_charlson[icd10_charlson$icd_code=="J440", "comorb"] == "copd")
  expect_true(icd10_charlson[icd10_charlson$icd_code=="J440", "comorb_group"] == "copd")
  expect_equal(icd10_charlson[icd10_charlson$icd_code=="J440", "charlson_weights"], 1)
})

test_that("Infection mapping works", {
  
  mock_icd10 <- data.frame(list(
    icd_code = c("A38X", "N390"),
    icd_label = c("Scarlet fever", "Urinary tract infection, site not specified")
  ))
  mock_icd10 <- map_infections_abx_indications(mock_icd10, "icd_code")
  expect_equal(mock_icd10$infection_group2_label[1], "Scarlet fever")
  expect_equal(mock_icd10$infection_group2_label[2], "Urinary tract infections, unspecified")
})

test_that("dosage and DDDs work", {
  expect_equivalent(compute_DDDs("J01CA04", "O", 1500, "mg"), 1)
  expect_true(is.na(compute_DDDs("J01CA04", "O", 1500, NA)))
  expect_error(compute_DDDs("J01CA04", "O", 1500, "fake_unit"))
  expect_true(is.na(compute_DDDs("J01CA04", "O", NA, "mg")))
  expect_true(is.na(compute_DDDs("J01CA04", NA, 1500, "mg")))
  expect_error(compute_DDDs("J01CA04", "fake_route", 1500, "mg"))
  expect_error(compute_DDDs(NA, "O", 1500, "mg"))
  expect_equivalent(compute_DDDs("J01CA04", c("O", "P", "O", NA), c(1500, 1500, 1500, 1500), "mg"), c(1, .5, 1, NA))
  expect_error(expect_warning(compute_DDDs("Fake_abx", "P", 650, "mg")))
  x <- expect_warning(compute_DDDs(c("J01CR02", "Fake_abx"), "P", 650, "mg"))
  expect_true(is.na(x[2]))
})


test_that("CCS mapping works", {
  mock_icd_data <- data.frame(
    list(icd10_code = c("J44", "J44X", "J440")), 
    stringsAsFactors = FALSE)
  
  mock_icd_data <- map_ICD10_CCS(mock_icd_data, "icd10_code")
  mock_icd_data <- map_ICD10_CCSR(mock_icd_data, "icd10_code")
  expect_equal(mock_icd_data$ccsr_cat_code, c("RSP008", "RSP008", "RSP008"))
  expect_equal(mock_icd_data$ccs_L2_code, c("8.2", "8.2", "8.2"))
})