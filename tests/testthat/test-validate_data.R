date1 <- structure(1585220000, class = c("POSIXct", "POSIXt"))

test_that("inpatient episode records are validated", {
  expect_false(expect_warning(validate_variable_no_missing(data.frame(foo = c("1", "")), "foo")))
  expect_false(expect_warning(validate_variable_no_missing(data.frame(foo = c("1", NA)), "foo")))
  expect_false(expect_warning(validate_variable_no_missing(data.frame(bar = c("1", NA)), "foo")))
  
  faulty_spells <- data.frame(list(
    patient_id = c(1, 2, 2),
    spell_id = c(1, 2, 3),
    admission_date = c(date1, date1, date1 + 3*3600),
    discharge_date = c(date1 - 12*3600, 
                       date1 + 6*3600,
                       date1 + 6*3600)
  ))
  
  expect_false(expect_warning(validate_inpatient_spells(faulty_spells[1, ])))
  expect_false(expect_warning(validate_inpatient_spells(faulty_spells[-1, ])))
  
  healthy_episodes <- data.frame(list(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 3*3600,
    episode_number = 1:3,
    last_episode_in_spell_indicator = 2,
    episode_start = c(date1, date1 + 3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  ))
  
  expect_equal(validate_inpatient_episodes(healthy_episodes), TRUE)
  
  overlap_episode <- data.frame(list(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 3*3600,
    episode_number = 1:3,
    last_episode_in_spell_indicator = 2,
    episode_start = c(date1, date1 + 0.5*3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  ))
  
  missing_first_episode <- data.frame(list(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 4*3600,
    episode_number = 2:4,
    last_episode_in_spell_indicator = 2,
    episode_start = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    episode_end = c(date1 + 2*3600, date1 + 3*3600, date1 + 4*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  ))
  
  missing_intermediate_episode <- data.frame(list(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 4*3600,
    episode_number = c(1, 3, 4),
    last_episode_in_spell_indicator = 2,
    episode_start = c(date1, date1 + 2*3600, date1 + 3*3600),
    episode_end = c(date1 + 3600, date1 + 3*3600, date1 + 4*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  ))
  
  missing_final_episode <- data.frame(list( 
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 4*3600,
    episode_number = 1:3,
    last_episode_in_spell_indicator = 2,
    episode_start = c(date1, date1 + 3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  ))
  
  expect_true(expect_warning(validate_inpatient_episodes(missing_first_episode)))
  expect_true(expect_warning(validate_inpatient_episodes(missing_intermediate_episode)))
  expect_true(expect_warning(validate_inpatient_episodes(missing_final_episode)))
  expect_false(expect_warning(validate_inpatient_episodes(overlap_episode)))
 
})

test_that("inpatient diagnosis records are validated", {
  test_diagnoses <- data.frame(list(
    patient_id = 1,
    spell_id = 1,
    episode_number = 1,
    icd_code = c("J440", "N39", "N81", NA),
    diagnosis_position = 1
  ))
  test_lookup <- data.frame(list(
    icd_code = c("J44", "N39"),
    icd_label = c("Other chronic obstructive pulmonary disease",
                  "Other disorders of urinary system")
  ))
  
  expect_true(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[1,], 
                                 diagnoses_lookup = test_lookup)))
  expect_true(validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[2,], 
                                           diagnoses_lookup = test_lookup))
  expect_true(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[3,], 
                                 diagnoses_lookup = test_lookup)))
  expect_false(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[3,], 
                                 diagnoses_lookup = dplyr::select(test_lookup, icd_code))))
  expect_false(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[4,], 
                                 diagnoses_lookup = test_lookup)
  ))
  
})

test_that("Prescriptions are validated", {
  
  expect_error(validate_prescriptions(
    data.frame(list(
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_id = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      ATC_code = "J01CF05",
      ATC_group = "Beta-lactam antibacterials, penicillins", 
      ATC_route = "P",
      authoring_date = structure(1421048831, 
                                 class = c("POSIXct", "POSIXt"), 
                                 tzone = ""), 
      prescription_start = structure(1421051891, 
                                     class = c("POSIXct", "POSIXt"), 
                                     tzone = ""), 
      prescription_end = structure(1421311091, 
                                   class = c("POSIXct", "POSIXt"), 
                                   tzone = ""), 
      prescription_status = "completed", 
      prescription_context = "inpatient", 
      dose = 2, unit = "g", route = "IV", frequency = "6H", 
      daily_frequency = 4, DDD = 4), stringsAsFactors = F)
  ))
  
  expect_error(validate_prescriptions(
    data.frame(list(
      patient_id = NA, 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_id = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      ATC_code = "J01CF05",
      ATC_group = "Beta-lactam antibacterials, penicillins",
      ATC_route = "P", 
      authoring_date = structure(1421048831, 
                                 class = c("POSIXct", "POSIXt"), 
                                 tzone = ""), 
      prescription_start = structure(1421051891, 
                                     class = c("POSIXct", "POSIXt"), 
                                     tzone = ""), 
      prescription_end = structure(1421311091, 
                                   class = c("POSIXct", "POSIXt"), 
                                   tzone = ""), 
      prescription_status = "completed", 
      prescription_context = "inpatient", 
      dose = 2, unit = "g", route = "IV", frequency = "6H", 
      daily_frequency = 4, DDD = 4), stringsAsFactors = F)
  ))
  
  expect_null(validate_prescriptions(
    data.frame(list(
      patient_id = "5593245762", 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_id = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      ATC_code = "J01CF05",
      ATC_group = "Beta-lactam antibacterials, penicillins", 
      ATC_route = "P",
      authoring_date = structure(1421048831, 
                                 class = c("POSIXct", "POSIXt"), 
                                 tzone = ""), 
      prescription_start = structure(1421051891, 
                                     class = c("POSIXct", "POSIXt"), 
                                     tzone = ""), 
      prescription_end = structure(1421311091, 
                                   class = c("POSIXct", "POSIXt"), 
                                   tzone = ""), 
      prescription_status = "completed", 
      prescription_context = "inpatient", 
      dose = 2, unit = "g", route = "IV", frequency = "6H", 
      daily_frequency = 4, DDD = 4), stringsAsFactors = F)
  ))
  
  expect_warning(validate_prescriptions(
    data.frame(list(
      patient_id = c("5593245762", "5593245762"),
      prescription_id = c("6025e96e1cc750dc6ec7fb9aadca0dbd", "2"),
      prescription_text = c("Flucloxacillin IV 2g 3 days", "Flucloxacillin IV 2g 4 days"), 
      drug_id = c("FLC","FLC"), drug_name = c("Flucloxacillin", "Flucloxacillin"), 
      drug_display_name = c("Flucloxacillin", "Flucloxacillin"), 
      ATC_code = c("J01CF05","J01CF05"),
      ATC_group = c("Beta-lactam antibacterials, penicillins",
                    "Beta-lactam antibacterials, penicillins"), 
      ATC_route = c("P", "P"),
      authoring_date = rep(structure(1421048831, 
                                     class = c("POSIXct", "POSIXt"), 
                                     tzone = ""), 2), 
      prescription_start = rep(structure(1421051891, 
                                         class = c("POSIXct", "POSIXt"), 
                                         tzone = ""), 2), 
      prescription_end = rep(structure(1421311091, 
                                       class = c("POSIXct", "POSIXt"), 
                                       tzone = ""),2), 
      prescription_status = c("completed", "completed"), 
      prescription_context = c("inpatient", "inpatient"), 
      dose = c(2, 2), unit = c("g", "g"), route = c("IV", "IV"), 
      frequency = c("6H", "6H"), 
      daily_frequency = c(4, 4), DDD = c( 4,  4)), stringsAsFactors = F)
  ))
  
})

test_that("variables are rearranged", {
  testdf <- data.frame(list(
    misc = 3,
    spell_id = 2,
    patient_id = 1
  ))

  expect_error(arrange_variables(testdf, "missing variable"))
  expect_equal(
    colnames(arrange_variables(testdf, c("patient_id", "spell_id"))), 
    c("patient_id", "spell_id", "misc"))
})
