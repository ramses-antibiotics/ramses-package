
date1 <- structure(1585220000, class = c("POSIXct", "POSIXt"))


# .validate_variable_exist ------------------------------------------------

test_that(".validate_variable_exist", {
  testdata <- dplyr::tibble(
    bidule = 1,
    truc = 2
  )
  
  testdatatable <- data.table::data.table(
    bidule = 1,
    truc = 2
  )
  
  expect_true(.validate_variable_exist(testdata, c("bidule", "truc")))
  expect_error(.validate_variable_exist(testdata, c("bidule", "truc", "bidul"), "error"))
  expect_warning(.validate_variable_exist(testdata, c("bidule", "truc", "TRUC")))
  
  expect_true(.validate_variable_exist(testdatatable, c("bidule", "truc")))
  expect_error(.validate_variable_exist(testdatatable, c("bidule", "truc", "bidul"), "error"))
  expect_warning(.validate_variable_exist(testdatatable, c("bidule", "truc", "TRUC")))
})


# .validate_variable_no_missing -------------------------------------------

test_that(".validate_variable_no_missing() on data.frame", {
  expect_error(.validate_variable_no_missing(
    data = data.frame(foo = c("1", "")), 
    vectorname = "foo", 
    action = "bidule"))
  expect_error(.validate_variable_no_missing(
    data = data.frame(foo = c("1", "")), 
    vectorname = "foo", 
    action = NULL))
  expect_false(expect_warning(.validate_variable_no_missing(
    data = data.frame(foo = c("1", "")), 
    vectorname = "foo", 
    action = "warning")))
  expect_error(.validate_variable_no_missing(
    data = data.frame(foo = c("1", "")), 
    vectorname = "foo", 
    action = "error"))
  
  expect_false(expect_warning(.validate_variable_no_missing(
    data = data.frame(foo = c("1", NA)), 
    vectorname = "foo", 
    action = "warning")))
  expect_error(.validate_variable_no_missing(
    data = data.frame(foo = c("1", NA)), 
    vectorname = "foo", 
    action = "error"))
  expect_true(.validate_variable_no_missing(
    data = data.frame(foo = c("1", NA)), 
    vectorname = character(0),
    action = "error"))
  expect_true(.validate_variable_no_missing(
    data.frame(bar = c("1", NA)), 
    vectorname = "foo",
    action = "error"))
})


test_that(".validate_variable_no_missing() on data.table", {
  expect_error(.validate_variable_no_missing(
    data = data.table::data.table(foo = c("1", "")), 
    vectorname = "foo", 
    action = "bidule"))
  expect_error(.validate_variable_no_missing(
    data = data.table::data.table(foo = c("1", "")), 
    vectorname = "foo", 
    action = NULL))
  expect_false(expect_warning(.validate_variable_no_missing(
    data = data.table::data.table(foo = c("1", "")), 
    vectorname = "foo", 
    action = "warning")))
  expect_error(.validate_variable_no_missing(
    data = data.table::data.table(foo = c("1", "")), 
    vectorname = "foo", 
    action = "error"))
  
  expect_false(expect_warning(.validate_variable_no_missing(
    data = data.table::data.table(foo = c("1", NA)), 
    vectorname = "foo", 
    action = "warning")))
  expect_error(.validate_variable_no_missing(
    data = data.table::data.table(foo = c("1", NA)), 
    vectorname = "foo", 
    action = "error"))
  expect_true(.validate_variable_no_missing(
    data = data.table::data.table(foo = c("1", NA)), 
    vectorname = character(0),
    action = "error"))
  expect_true(.validate_variable_no_missing(
    data.table::data.table(bar = c("1", NA)), 
    vectorname = "foo",
    action = "error"))
})



# validate_inpatient_spells/validate_inpatient_episodes -------------------

test_that("validate_inpatient_spells()/validate_inpatient_episodes() on data.frame", {

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
    patient_id = "1",
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 3*3600,
    episode_number = 1:3,
    last_episode_in_spell = 2,
    episode_start = c(date1, date1 + 3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  ))
  
  expect_equal(
    validate_inpatient_episodes(patients = dplyr::tibble(patient_id = "1"),
                                episodes = healthy_episodes),
    TRUE
  )
  expect_error(
    validate_inpatient_episodes(patients = dplyr::tibble(wrong_variable = "1"),
                                episodes = healthy_episodes)
  )
  expect_error(
    validate_inpatient_episodes(patients = dplyr::tibble(patient_id = "2"),
                                episodes = healthy_episodes)
  )
  
  overlap_episode <- data.frame(list(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 3*3600,
    episode_number = 1:3,
    last_episode_in_spell = 2,
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
    last_episode_in_spell = 2,
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
    last_episode_in_spell = 2,
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
    last_episode_in_spell = 2,
    episode_start = c(date1, date1 + 3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  ))
  
  expect_true(expect_warning(validate_inpatient_episodes(dplyr::tibble(patient_id = 1),
                                                         missing_first_episode)))
  expect_true(expect_warning(validate_inpatient_episodes(dplyr::tibble(patient_id = 1),
                                                         missing_intermediate_episode)))
  expect_true(expect_warning(validate_inpatient_episodes(dplyr::tibble(patient_id = 1),
                                                         missing_final_episode)))
  expect_false(expect_warning(validate_inpatient_episodes(dplyr::tibble(patient_id = 1),
                                                          overlap_episode)))
 
})


test_that("validate_inpatient_spells()/validate_inpatient_episodes() on data.table", {
  
  faulty_spells <- data.table::data.table(
    patient_id = c(1, 2, 2),
    spell_id = c(1, 2, 3),
    admission_date = c(date1, date1, date1 + 3*3600),
    discharge_date = c(date1 - 12*3600, 
                       date1 + 6*3600,
                       date1 + 6*3600)
  )
  
  expect_false(expect_warning(validate_inpatient_spells(faulty_spells[1, ])))
  expect_false(expect_warning(validate_inpatient_spells(faulty_spells[-1, ])))
  
  healthy_episodes <- data.table::data.table(
    patient_id = "1",
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 3*3600,
    episode_number = 1:3,
    last_episode_in_spell = 2,
    episode_start = c(date1, date1 + 3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  )
  
  expect_equal(
    validate_inpatient_episodes(patients = data.table::data.table(patient_id = "1"),
                                episodes = healthy_episodes),
    TRUE
  )
  expect_error(
    validate_inpatient_episodes(patients = data.table::data.table(wrong_variable = "1"),
                                episodes = healthy_episodes)
  )
  expect_error(
    validate_inpatient_episodes(patients = data.table::data.table(patient_id = "2"),
                                episodes = healthy_episodes)
  )
  
  overlap_episode <- data.table::data.table(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 3*3600,
    episode_number = 1:3,
    last_episode_in_spell = 2,
    episode_start = c(date1, date1 + 0.5*3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  )
  
  missing_first_episode <- data.table::data.table(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 4*3600,
    episode_number = 2:4,
    last_episode_in_spell = 2,
    episode_start = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    episode_end = c(date1 + 2*3600, date1 + 3*3600, date1 + 4*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  )
  
  missing_intermediate_episode <- data.table::data.table(
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 4*3600,
    episode_number = c(1, 3, 4),
    last_episode_in_spell = 2,
    episode_start = c(date1, date1 + 2*3600, date1 + 3*3600),
    episode_end = c(date1 + 3600, date1 + 3*3600, date1 + 4*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  )
  
  missing_final_episode <- data.table::data.table( 
    patient_id = 1,
    spell_id = 2,
    admission_date = date1,
    discharge_date = date1 + 4*3600,
    episode_number = 1:3,
    last_episode_in_spell = 2,
    episode_start = c(date1, date1 + 3600, date1 + 2*3600),
    episode_end = c(date1 + 3600, date1 + 2*3600, date1 + 3*3600),
    admission_method = "2",
    consultant_code = "CXXXXXX",
    main_specialty_code = "100"
  )
  
  expect_true(expect_warning(validate_inpatient_episodes(data.table::data.table(patient_id = 1),
                                                         missing_first_episode)))
  expect_true(expect_warning(validate_inpatient_episodes(data.table::data.table(patient_id = 1),
                                                         missing_intermediate_episode)))
  expect_true(expect_warning(validate_inpatient_episodes(data.table::data.table(patient_id = 1),
                                                         missing_final_episode)))
  expect_false(expect_warning(validate_inpatient_episodes(data.table::data.table(patient_id = 1),
                                                          overlap_episode)))
  
})


# validate_inpatient_diagnoses --------------------------------------------

test_that("validate_inpatient_diagnoses() on data.frame", {
  test_diagnoses <- data.frame(list(
    patient_id = 1,
    spell_id = 1,
    episode_number = 1,
    icd_code = c("J440", "N39", "N81", NA),
    diagnosis_position = 1,
    last_episode_in_spell = "2"
  ))
  test_lookup <- data.frame(list(
    icd_code = c("J44", "N39"),
    icd_display = c("J44", "N39"),
    icd_description = c("Other chronic obstructive pulmonary disease",
                  "Other disorders of urinary system"),
    category_code = c("J44", "N39"),
    category_description = c("Other chronic obstructive pulmonary disease",
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
  expect_error(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[3,], 
                                 diagnoses_lookup = dplyr::select(test_lookup, icd_code)))
  expect_false(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[4,], 
                                 diagnoses_lookup = test_lookup)
  ))
  
})

test_that("validate_inpatient_diagnoses() on data.table", {
  test_diagnoses <- data.table::data.table(
    patient_id = 1,
    spell_id = 1,
    episode_number = 1,
    icd_code = c("J440", "N39", "N81", NA),
    diagnosis_position = 1,
    last_episode_in_spell = "2"
  )
  test_lookup <- data.table::data.table(
    icd_code = c("J44", "N39"),
    icd_display = c("J44", "N39"),
    icd_description = c("Other chronic obstructive pulmonary disease",
                        "Other disorders of urinary system"),
    category_code = c("J44", "N39"),
    category_description = c("Other chronic obstructive pulmonary disease",
                             "Other disorders of urinary system")
  )
  
  expect_true(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[1,], 
                                 diagnoses_lookup = test_lookup)))
  expect_true(validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[2,], 
                                           diagnoses_lookup = test_lookup))
  expect_true(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[3,], 
                                 diagnoses_lookup = test_lookup)))
  expect_error(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[3,], 
                                 diagnoses_lookup = test_lookup[, icd_code]))
  expect_false(expect_warning(
    validate_inpatient_diagnoses(diagnoses_data = test_diagnoses[4,], 
                                 diagnoses_lookup = test_lookup)
  ))
  
})

# validate_prescriptions --------------------------------------------------

test_that("validate_prescriptions() on data.frame", {
  
  expect_error(validate_prescriptions(
    data.frame(list(
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      drug_group = "Beta-lactams/penicillins", 
      antiinfective_type = c("antibacterial"),
      ATC_code = "J01CF05",
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
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      drug_group = "Beta-lactams/penicillins", 
      antiinfective_type = c("antibacterial"),
      ATC_code = "J01CF05",
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
      patient_id = "5593245762", 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      drug_group = "Beta-lactams/penicillins", 
      antiinfective_type = "INVALID DATA",
      ATC_code = "J01CF05",
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
      patient_id = "5593245762", 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      drug_group = "Beta-lactams/penicillins", 
      antiinfective_type = c("antibacterial"),
      ATC_code = "J01CF05",
      ATC_route = "ERROR!!!!!!!",
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
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      drug_group = "Beta-lactams/penicillins", 
      antiinfective_type = c("antibacterial"),
      ATC_code = "J01CF05",
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
      drug_code = c("FLC","FLC"), drug_name = c("Flucloxacillin", "Flucloxacillin"), 
      drug_display_name = c("Flucloxacillin", "Flucloxacillin"),
      drug_group = c("Beta-lactams/penicillins",
                     "Beta-lactams/penicillins"),  
      antiinfective_type = c("antibacterial", "antibacterial"),
      ATC_code = c("J01CF05","J01CF05"),
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

test_that("validate_prescriptions() on data.table", {
  
  expect_error(validate_prescriptions(
    data.table::data.table(
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      antiinfective_type = c("antibacterial"),
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
      daily_frequency = 4, DDD = 4)
  ))
  
  expect_error(validate_prescriptions(
    data.table::data.table(
      patient_id = NA, 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      antiinfective_type = c("antibacterial"),
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
      daily_frequency = 4, DDD = 4)
  ))
  
  expect_error(validate_prescriptions(
    data.table::data.table(
      patient_id = "5593245762", 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      antiinfective_type = "INVALID DATA",
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
      daily_frequency = 4, DDD = 4)
  ))
  
  expect_error(validate_prescriptions(
    data.table::data.table(
      patient_id = "5593245762", 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      antiinfective_type = c("antibacterial"),
      ATC_code = "J01CF05",
      ATC_group = "Beta-lactam antibacterials, penicillins", 
      ATC_route = "ERROR!!!!!!!",
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
      daily_frequency = 4, DDD = 4)
  ))
  
  expect_null(validate_prescriptions(
    data.table::data.table(
      patient_id = "5593245762", 
      prescription_id = "6025e96e1cc750dc6ec7fb9aadca0dbd", 
      prescription_text = "Flucloxacillin IV 2g 3 days", 
      drug_code = "FLC", drug_name = "Flucloxacillin", 
      drug_display_name = "Flucloxacillin", 
      drug_group = "Beta-lactams/penicillins", 
      antiinfective_type = c("antibacterial"),
      ATC_code = "J01CF05",
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
      daily_frequency = 4, DDD = 4)
  ))
  
  expect_warning(validate_prescriptions(
    data.table::data.table(
      patient_id = c("5593245762", "5593245762"),
      prescription_id = c("6025e96e1cc750dc6ec7fb9aadca0dbd", "2"),
      prescription_text = c("Flucloxacillin IV 2g 3 days", "Flucloxacillin IV 2g 4 days"), 
      drug_code = c("FLC","FLC"), drug_name = c("Flucloxacillin", "Flucloxacillin"), 
      drug_display_name = c("Flucloxacillin", "Flucloxacillin"), 
      drug_group = c("Beta-lactams/penicillins",
                     "Beta-lactams/penicillins"), 
      antiinfective_type = c("antibacterial", "antibacterial"),
      ATC_code = c("J01CF05","J01CF05"),
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
      daily_frequency = c(4, 4), DDD = c( 4,  4))
  ))
  
})

# arrange_variables -------------------------------------------------------

test_that("arrange_variables", {
  testdf <- data.frame(list(
    misc = 3,
    spell_id = 2,
    patient_id = 1
  ))
  testDT <- data.table::data.table(
    misc = 3,
    spell_id = 2,
    patient_id = 1
  )

  expect_error(arrange_variables(testdf, "missing variable"))
  expect_equal(
    colnames(arrange_variables(testdf, c("patient_id", "spell_id"))), 
    c("patient_id", "spell_id", "misc"))
  expect_error(arrange_variables(testDT, "missing variable"))
  expect_equal(
    colnames(arrange_variables(testDT, c("patient_id", "spell_id"))), 
    c("patient_id", "spell_id", "misc"))
})


# .validate_values_unique -------------------------------------------------

test_that(".validate_values_unique()", {
  testdata <- data.frame(list(a = 1:4, b = rep(4, 4)))
  testdatatable <- data.table::data.table(a = 1:4, b = rep(4, 4))
  
  expect_true(.validate_values_unique(testdata, c("a")))
  expect_error(.validate_values_unique(testdata, c("a", "b")))
  expect_error(.validate_values_unique(testdata, c("b")))
  expect_true(.validate_values_unique(testdata, c("a", "c")))
  expect_true(.validate_values_unique(testdata, c("c")))
  expect_true(.validate_values_unique(testdata, c()))
  expect_true(.validate_values_unique(testdata, NULL))
  
  expect_true(.validate_values_unique(testdatatable, c("a")))
  expect_error(.validate_values_unique(testdatatable, c("a", "b")))
  expect_error(.validate_values_unique(testdatatable, c("b")))
  expect_true(.validate_values_unique(testdatatable, c("a", "c")))
  expect_true(.validate_values_unique(testdatatable, c("c")))
  expect_true(.validate_values_unique(testdatatable, c()))
  expect_true(.validate_values_unique(testdatatable, NULL))
})


# .validate_UCUM_codes ----------------------------------------------------

test_that(".validate_UCUM_codes()", {
  testdata <- c('absurd_code', NA, '', '%', 'degree_Celsius', 'mm_Hg', '10^9/L', 
                'mmol/L', 'fL', 'g/L', 'pg', 'micromol/L', 'mg/L')
  expect_error(.validate_UCUM_codes(testdata[1:5]))
  expect_true(.validate_UCUM_codes(testdata[-1]))
})


# validate_microbiology ---------------------------------------------------

test_that("validate_microbiology on data.frame", {
  testdata = list(
    specimens = data.frame(list(
      specimen_id = "1",
      patient_id = "1",
      status = "available",
      specimen_datetime = Sys.time(),
      specimen_type_code = "122575003",
      specimen_type_name = "Urine specimen",
      specimen_type_display = "Urine"
    ), stringsAsFactors = FALSE),
    isolates = data.frame(
      list(isolate_id = "1", 
           specimen_id = "1", 
           patient_id = "1", 
           organism_code = structure("B_ESCHR_COLI", class = c("mo", "character")), 
           organism_name = "Escherichia coli", 
           organism_display_name = "Escherichia coli", 
           multidrug_resistance = "Multi-drug-resistant (MDR)",
           isolation_datetime = Sys.time() + 259200
    ), stringsAsFactors = FALSE),
    susceptibilities = data.frame(list(
      isolate_id = "1", 
      specimen_id = "1",
      patient_id = "1",
      organism_code = "B_ESCHR_COLI",
      organism_name = "Escherichia coli",
      organism_display_name = "Escherichia coli", 
      agent_code = c("AMP", "AMC", "CAZ", "CTX", "CIP", "LEX", "CXM", "ETP", 
                  "FOS", "GEN", "MEM", "NIT", "TZP", "TMP"),
      agent_name = c("Ampicillin", "Amoxicillin/clavulanic acid", "Ceftazidime", "Cefotaxime", 
                    "Ciprofloxacin", "Cephalexin", "Cefuroxime", "Ertapenem", "Fosfomycin", 
                    "Gentamicin", "Meropenem", "Nitrofurantoin", "Piperacillin/tazobactam", 
                    "Trimethoprim"), 
      agent_display_name = c("Ampicillin", "Co-amoxiclav", "Ceftazidime", "Cefotaxime", "Ciprofloxacin",
                            "Cefalexin", "Cefuroxime", "Ertapenem", "Fosfomycin", "Gentamicin", 
                            "Meropenem", "Nitrofurantoin", "Piperacillin + Tazobactam", "Trimethoprim"),
      rsi_code = c("R", "R", "S", "S", "R", "S", "S", "S", "S", "S", "S", "S", "R", "R")
    ), stringsAsFactors = FALSE)
  )
  
  expect_true(validate_microbiology(
    testdata$specimens,
    testdata$isolates,
    testdata$susceptibilities
  ))
  
  # wrong specimen code
  testdata_spec_code <- testdata
  testdata_spec_code$specimens$specimen_type_code <- "biduletruccleurtruc"
  expect_warning(validate_microbiology(
    testdata_spec_code$specimens,
    testdata_spec_code$isolates,
    testdata_spec_code$susceptibilities
  ))
  
  # missing relations
  testdata_missing_id1 <- testdata
  testdata_missing_id1$susceptibilities$specimen_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_missing_id1$specimens,
    testdata_missing_id1$isolates,
    testdata_missing_id1$susceptibilities
  ))
  
  testdata_missing_id2 <- testdata
  testdata_missing_id2$susceptibilities$isolate_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_missing_id2$specimens,
    testdata_missing_id2$isolates,
    testdata_missing_id2$susceptibilities
  ))
  
  testdata_missing_id3 <- testdata
  testdata_missing_id3$isolates$specimen_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_missing_id3$specimens,
    testdata_missing_id3$isolates,
    testdata_missing_id3$susceptibilities
  ))
  
  testdata_invalid_organism_codes <- testdata
  testdata_invalid_organism_codes$isolates$isolate_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_invalid_organism_codes$specimens,
    testdata_invalid_organism_codes$isolates,
    testdata_invalid_organism_codes$susceptibilities
  ))
  
  testdata_invalid_organism_codes <- testdata
  testdata_invalid_organism_codes$susceptibilities$isolate_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_invalid_organism_codes$specimens,
    testdata_invalid_organism_codes$isolates,
    testdata_invalid_organism_codes$susceptibilities
  ))
  
  testdata_invalid_drug_codes <- testdata
  testdata_invalid_drug_codes$susceptibilities$agent_code[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_invalid_drug_codes$specimens,
    testdata_invalid_drug_codes$isolates,
    testdata_invalid_drug_codes$susceptibilities
  ))
  
})

test_that("validate_microbiology on data.table", {
  testdata = list(
    specimens = data.table::data.table(
      specimen_id = "1",
      patient_id = "1",
      status = "available",
      specimen_datetime = Sys.time(),
      specimen_type_code = "122575003",
      specimen_type_name = "Urine specimen",
      specimen_type_display = "Urine"
    ),
    isolates = data.table::data.table(
      isolate_id = "1", 
      specimen_id = "1", 
      patient_id = "1", 
      organism_code = structure("B_ESCHR_COLI", class = c("mo", "character")), 
      organism_name = "Escherichia coli", 
      organism_display_name = "Escherichia coli", 
      multidrug_resistance = "Multi-drug-resistant (MDR)",
      isolation_datetime = Sys.time() + 259200
      ),
    susceptibilities = data.table::data.table(
      isolate_id = "1", 
      specimen_id = "1",
      patient_id = "1",
      organism_code = "B_ESCHR_COLI",
      organism_name = "Escherichia coli",
      organism_display_name = "Escherichia coli", 
      agent_code = c("AMP", "AMC", "CAZ", "CTX", "CIP", "LEX", "CXM", "ETP", 
                  "FOS", "GEN", "MEM", "NIT", "TZP", "TMP"),
      agent_name = c("Ampicillin", "Amoxicillin/clavulanic acid", "Ceftazidime", "Cefotaxime", 
                    "Ciprofloxacin", "Cephalexin", "Cefuroxime", "Ertapenem", "Fosfomycin", 
                    "Gentamicin", "Meropenem", "Nitrofurantoin", "Piperacillin/tazobactam", 
                    "Trimethoprim"), 
      agent_display_name = c("Ampicillin", "Co-amoxiclav", "Ceftazidime", "Cefotaxime", "Ciprofloxacin",
                            "Cefalexin", "Cefuroxime", "Ertapenem", "Fosfomycin", "Gentamicin", 
                            "Meropenem", "Nitrofurantoin", "Piperacillin + Tazobactam", "Trimethoprim"),
      rsi_code = c("R", "R", "S", "S", "R", "S", "S", "S", "S", "S", "S", "S", "R", "R")
    )
  )
  
  expect_true(validate_microbiology(
    testdata$specimens,
    testdata$isolates,
    testdata$susceptibilities
  ))
  
  # wrong specimen code
  testdata_spec_code <- testdata
  testdata_spec_code$specimens$specimen_type_code <- "biduletruccleurtruc"
  expect_warning(validate_microbiology(
    testdata_spec_code$specimens,
    testdata_spec_code$isolates,
    testdata_spec_code$susceptibilities
  ))
  
  # missing relations
  testdata_missing_id1 <- testdata
  testdata_missing_id1$susceptibilities$specimen_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_missing_id1$specimens,
    testdata_missing_id1$isolates,
    testdata_missing_id1$susceptibilities
  ))
  
  testdata_missing_id2 <- testdata
  testdata_missing_id2$susceptibilities$isolate_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_missing_id2$specimens,
    testdata_missing_id2$isolates,
    testdata_missing_id2$susceptibilities
  ))
  
  testdata_missing_id3 <- testdata
  testdata_missing_id3$isolates$specimen_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_missing_id3$specimens,
    testdata_missing_id3$isolates,
    testdata_missing_id3$susceptibilities
  ))
  
  testdata_invalid_organism_codes <- testdata
  testdata_invalid_organism_codes$isolates$isolate_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_invalid_organism_codes$specimens,
    testdata_invalid_organism_codes$isolates,
    testdata_invalid_organism_codes$susceptibilities
  ))
  
  testdata_invalid_organism_codes <- testdata
  testdata_invalid_organism_codes$susceptibilities$isolate_id[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_invalid_organism_codes$specimens,
    testdata_invalid_organism_codes$isolates,
    testdata_invalid_organism_codes$susceptibilities
  ))
  
  testdata_invalid_drug_codes <- testdata
  testdata_invalid_drug_codes$susceptibilities$agent_code[1] <- "biduletruccleurtruc"
  expect_error(validate_microbiology(
    testdata_invalid_drug_codes$specimens,
    testdata_invalid_drug_codes$isolates,
    testdata_invalid_drug_codes$susceptibilities
  ))
  
})

# validate_investigations -------------------------------------------------

test_that("validate_investigations", {
  example_data <- dplyr::tibble(
    observation_id = c("1", "2"), 
    patient_id = "99999999999", 
    spell_id = "9278078393", status = "final", 
    request_datetime = as.POSIXct("2016-03-30 13:51:13"), 
    observation_datetime = as.POSIXct("2016-03-30 13:51:45"), 
    observation_value_text = "", 
    observation_value_numeric = c(35.7, 100.4), 
    observation_name = "Body temperature", 
    observation_display = "Temperature", 
    observation_code_system = "http://loinc.org", 
    observation_code = "8310-5", 
    observation_unit = c("degree_Celsius", "degree_F")
  )
  
  example_datatable <- data.table::data.table(
    observation_id = c("1", "2"), 
    patient_id = "99999999999", 
    spell_id = "9278078393", status = "final", 
    request_datetime = as.POSIXct("2016-03-30 13:51:13"), 
    observation_datetime = as.POSIXct("2016-03-30 13:51:45"), 
    observation_value_text = "", 
    observation_value_numeric = c(35.7, 100.4), 
    observation_name = "Body temperature", 
    observation_display = "Temperature", 
    observation_code_system = "http://loinc.org", 
    observation_code = "8310-5", 
    observation_unit = c("degree_Celsius", "degree_F")
  )
  
  expect_true(validate_investigations(example_data[1,]))
  expect_error(validate_investigations(example_data))
  
  expect_true(validate_investigations(example_datatable[1,]))
  expect_error(validate_investigations(example_datatable))
})
