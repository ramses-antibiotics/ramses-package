

test_that(".clinical_feature_field_name_generate", {
  investigationstable <- unique(
    dplyr::tibble(observation_code = inpatient_investigations$observation_code,
                  observation_display = inpatient_investigations$observation_display)
  )
  fake_db_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  dplyr::copy_to(fake_db_conn, 
                 df = investigationstable, 
                 name = "inpatient_investigations", 
                 temporary = FALSE)
  
  expect_equal(
    .clinical_feature_field_name_generate(fake_db_conn, "last", "1104051000000101",  NULL, 72),
    "last_sews_score_72h"
  )
  expect_equal(
    .clinical_feature_field_name_generate(fake_db_conn, "range", "1104051000000101", "16_18", 72),
    "range_sews_score16_18_72h"
  )
  expect_equal(
    .clinical_feature_field_name_generate(fake_db_conn, "mean", "8480-6",  NULL, 48),
    "mean_systolic_bp_48h"
  )
  expect_error(
    .clinical_feature_field_name_generate(fake_db_conn, c("mean", "mean"), c("8480-6", "1104051000000101"), NULL, 48)
  )
  expect_error(
    .clinical_feature_field_name_generate(fake_db_conn, "mean", "8480-6",  NULL, NA)
  )
  DBI::dbDisconnect(fake_db_conn)
})