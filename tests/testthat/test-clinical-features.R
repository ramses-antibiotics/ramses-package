

test_that(".clinical_investigation_code_validate", {
  investigationstable <- unique(
    dplyr::tibble(observation_code = c("A", "B", "A", "C"),
                  observation_code_system = c("http://codeA.com/", "http://codeA.com/", 
                                              "http://codeB.com/", "http://codeB.com/"))
  )
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit({DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)})
  
  dplyr::copy_to(fake_db_conn, 
                 df = investigationstable, 
                 name = "inpatient_investigations", 
                 temporary = FALSE)
  
  expect_error(
    .clinical_investigation_code_validate(conn = fake_db_conn, 
                                          observation_code = "A",
                                          observation_code_system = NULL)
  )
  expect_error(
    .clinical_investigation_code_validate(conn = fake_db_conn, 
                                          observation_code = "A",
                                          observation_code_system = c("http://codeA.com/", 
                                                                      "http://codeB.com/"))
  )
  expect_invisible(
    .clinical_investigation_code_validate(conn = fake_db_conn, 
                                          observation_code = "A",
                                          observation_code_system = "http://codeA.com/")
  )
  expect_invisible(
    .clinical_investigation_code_validate(conn = fake_db_conn, 
                                          observation_code = c("A", "B"),
                                          observation_code_system = "http://codeA.com/")
  )
  expect_error(
    .clinical_investigation_code_validate(conn = fake_db_conn, 
                                          observation_code = "D",
                                          observation_code_system = NULL)
  )
})


test_that(".clinical_feature_field_name_generate", {
  investigationstable <- dplyr::distinct(inpatient_investigations,
                                         observation_code_system,
                                         observation_code,
                                         observation_display)
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit({DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)})
  
  dplyr::copy_to(fake_db_conn, 
                 df = investigationstable, 
                 name = "inpatient_investigations", 
                 temporary = FALSE)
  
  expect_equal(
    .clinical_feature_field_name_generate(fake_db_conn, "last", "1104051000000101",  NULL, 72, NULL),
    "last_sews_score_72h"
  )
  expect_equal(
    .clinical_feature_field_name_generate(fake_db_conn, "last", "1104051000000101",  NULL, 72, "http://snomed.info/sct"),
    "last_sews_score_72h"
  )
  expect_error(
    .clinical_feature_field_name_generate(fake_db_conn, "last", "1104051000000101",  NULL, 72, "http://loinc.org")
  )
  expect_equal(
    .clinical_feature_field_name_generate(fake_db_conn, "range", "1104051000000101", "16_18", 72, NULL),
    "range_sews_score16_18_72h"
  )
  expect_equal(
    .clinical_feature_field_name_generate(fake_db_conn, "mean", "8480-6",  NULL, 48, NULL),
    "mean_systolic_bp_48h"
  )
  expect_error(
    .clinical_feature_field_name_generate(fake_db_conn, c("mean", "mean"), c("8480-6", "1104051000000101"), NULL, 48, NULL)
  )
  expect_error(
    .clinical_feature_field_name_generate(fake_db_conn, "mean", "8480-6",  NULL, NA, NULL)
  )
})

test_that(".clinical_feature_object_id_field", {
  fake_db_conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit({DBI::dbDisconnect(fake_db_conn, shutdown = TRUE)})
  
  dplyr::copy_to(fake_db_conn, 
                 df = dplyr::tibble(patient_id = 99), 
                 name = "patients", 
                 temporary = FALSE)
  
  patient_object <- new("Patient", 
                        id = 99, 
                        conn = fake_db_conn,
                        record = tbl(fake_db_conn, "patients"))
  encounter_object <- new("Encounter", 
                          id = 99, 
                          conn = fake_db_conn,
                          record = tbl(fake_db_conn, "patients"),
                          longitudinal_table = tbl(fake_db_conn, "patients"))
  therapy_episode_object <- new("TherapyEpisode", 
                                id = 99, 
                                conn = fake_db_conn,
                                record = tbl(fake_db_conn, "patients"),
                                longitudinal_table = tbl(fake_db_conn, "patients"))
  
  expect_error(
    .clinical_feature_object_id_field(patient_object),
    "[.]clinical_feature_object_id_field[(][)] is not implemented for Patient objects"
  )
  expect_equal(
    .clinical_feature_object_id_field(encounter_object),
    "encounter_id"
  )
  expect_equal(
    .clinical_feature_object_id_field(therapy_episode_object),
    "therapy_id"
  )
})