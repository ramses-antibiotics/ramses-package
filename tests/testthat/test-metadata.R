
test_that("ICD-10-CM downloads and maps to Charlson", {
  icd10cm <- download_icd10cm()
  expect_equal(icd10cm$icd_code[1], "A000")
  icd10_charlson <- map_charlson_comorbidities(df = icd10cm, icd_column = "icd_code")
  expect_true(icd10_charlson[icd10_charlson$icd_code=="J440", "comorb"] == "cpd")
  expect_true(icd10_charlson[icd10_charlson$icd_code=="J440", "comorb_group"] == "cpd")
  expect_equal(icd10_charlson[icd10_charlson$icd_code=="J440", "charlson_weights"], 1)
  
  # Bug 84 fix verification
  icd10cm_tbl <- dplyr::as_tibble(icd10cm)
  icd10_charlson_tbl <- map_charlson_comorbidities(df = icd10cm_tbl, icd_column = "icd_code")
  expect_true(icd10_charlson_tbl[icd10_charlson_tbl$icd_code=="J440", "comorb"] == "cpd")
  expect_true(icd10_charlson_tbl[icd10_charlson_tbl$icd_code=="J440", "comorb_group"] == "cpd")
  expect_equal(icd10_charlson_tbl[icd10_charlson_tbl$icd_code=="J440", "charlson_weights"], 1)
})

test_that("Infection mapping", {
  
  mock_icd10 <- data.frame(list(
    icd_code = c("A38X", "N390"),
    icd_label = c("Scarlet fever", "Urinary tract infection, site not specified")
  ))
  mock_icd10 <- map_infections_abx_indications(mock_icd10, "icd_code")
  expect_equal(mock_icd10$infection_group2_label[1], "Scarlet fever")
  expect_equal(mock_icd10$infection_group2_label[2], "Urinary tract infections, unspecified")
})

test_that("dosage and DDDs", {
  expect_equivalent(compute_DDDs("J02AC01", "O", 200, "mg"), 1)
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

test_that("ATC name look up", {
  expect_equal(get_ATC_name(c("J01CA04", "J01XA01")), c("amoxicillin", "vancomycin"))
  expect_equal(expect_warning(get_ATC_name(c(NA, "", "fake_code"))), rep(NA_character_, 3))
})

test_that("CCS mapping", {
  mock_icd_data <- data.frame(
    list(icd10_code = c("J44", "J44X", "J440")), 
    stringsAsFactors = FALSE)
  
  mock_icd_data <- map_ICD10_CCS(mock_icd_data, "icd10_code")
  mock_icd_data <- map_ICD10_CCSR(mock_icd_data, "icd10_code")
  expect_equal(mock_icd_data$ccsr_cat_code, c("RSP008", "RSP008", "RSP008"))
  expect_equal(mock_icd_data$ccs_L2_code, c("8.2", "8.2", "8.2"))
})

test_that("import_icd", {
  mock_icd_file <- system.file(package = "Ramses", "ICD10_mock_file.zip")
  imported_mock_icd <- import_icd(mock_icd_file, version = "blurg")
  expect_equal(imported_mock_icd$icd_display, c("A00.0", "A00.1"))
  expect_equal(imported_mock_icd$category_description, c("Cholera", "Cholera"))
})