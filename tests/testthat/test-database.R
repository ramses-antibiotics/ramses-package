
test_that("transitive closure parameters are set", {
  
  expect_is(transitive_closure_control(), "list")
  expect_error(transitive_closure_control(-2, 0, 1))
  expect_error(transitive_closure_control(1, -2, 1))
  expect_error(transitive_closure_control(1, 1, -2))
  expect_error(transitive_closure_control(1, 1, c(-2, 0)))
  expect_is(transitive_closure_control()$max_continuation_gap, "integer")

})

test_that("Batch SQL scripts get split", {
  expect_equal(.split_sql_batch("blah1; blah2; WHILE stuff BEGIN balh3; balh3; END; blah4;"),
               c("blah1;", " blah2;", "WHILE stuff BEGIN balh3; balh3; END;", " blah4;"))
  
  expect_equal(.split_sql_batch("blah1; blah2; WHILE stuff BEGIN balh3; balh3; END; blah4; WHILE i'm bored BEGIN AH END;"),
               c("blah1;", " blah2;", "WHILE stuff BEGIN balh3; balh3; END;", " blah4;", "WHILE i'm bored BEGIN AH END;"))
  
})


test_that("bridge_episode_prescription_overlap", {
  emptydatabase <- dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  expect_error(bridge_episode_prescription_overlap(
    conn = "not_a_connection", overwrite = TRUE))
  expect_error(bridge_episode_prescription_overlap(
    conn = emptydatabase, overwrite = TRUE))
  expect_error(bridge_episode_prescription_overlap(
    conn = emptydatabase, overwrite = "TRUE"))
  dbDisconnect(emptydatabase)
})

test_that("bridge_episode_prescription_initiation", {
  emptydatabase <- dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  expect_error(bridge_episode_prescription_initiation(
    conn = "not_a_connection", overwrite = TRUE))
  expect_error(bridge_episode_prescription_initiation(
    conn = emptydatabase, overwrite = TRUE))
  expect_error(bridge_episode_prescription_initiation(
    conn = emptydatabase, overwrite = "TRUE"))
  dbDisconnect(emptydatabase)
})

test_that("bridge_spell_therapy_overlap", {
  emptydatabase <- dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  expect_error(bridge_spell_therapy_overlap(
    conn = "not_a_connection", overwrite = TRUE))
  expect_error(bridge_spell_therapy_overlap(
    conn = emptydatabase, overwrite = TRUE))
  expect_error(bridge_spell_therapy_overlap(
    conn = emptydatabase, overwrite = "TRUE"))
  dbDisconnect(emptydatabase)
})

test_that(".compute_bed_days.SQLiteConnection", {
  temp_db <- dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  dplyr::copy_to(
    dest = temp_db,
    df = data.frame(
      episode_start = "2015-01-01 10:00:00",
      episode_end = "2015-01-02 16:00:00"
    ), 
    name = "inpatient_episodes", 
    temporary = FALSE
  )
  .compute_bed_days.SQLiteConnection(temp_db)
  expect_equal(
    dplyr::collect(tbl(temp_db, "inpatient_episodes"))[["ramses_bed_days"]],
    1.25)
  dbDisconnect(temp_db)
})

test_that(".format_id_as_character", {
  expect_equal(
    .format_id_as_character(data.frame(list(patient_id = 1:2))),
    data.frame(list(patient_id = c("1", "2")), stringsAsFactors = F)
  )
  expect_equal(
    .format_id_as_character(data.frame(list(patient_id2 = 1:2))),
    data.frame(list(patient_id2 = 1:2), stringsAsFactors = F)
  )
})

test_that(".sql_data_type", {
  fake_db_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  dplyr::copy_to(dest = fake_db_conn,
                 df = dplyr::tibble(patient_id = "99999999999", 
                                    int_var = 1L,
                                    num_var = 1.1), 
                 name = "patients", 
                 temporary = FALSE)
  expect_equal(
    .sql_data_type(fake_db_conn, "patients", c("patient_id", "int_var")),
    c("patient_id" = "character",
      "int_var" = "integer")
  )
  expect_equal(
    .sql_data_type(fake_db_conn, "patients"),
    c(patient_id = "character", 
      int_var = "integer", 
      num_var = "numeric")
  )
  expect_error(
    .sql_data_type(fake_db_conn, "patients", 1)
  )
  DBI::dbDisconnect(fake_db_conn)
})