
test_that("transitive closure parameters are set", {
  
  expect_is(transitive_closure_control(), "list")
  expect_error(transitive_closure_control(-2, 0, 1))
  expect_error(transitive_closure_control(1, -2, 1))
  expect_error(transitive_closure_control(1, 1, -2))
  expect_error(transitive_closure_control(1, 1, c(-2, 0)))
  expect_is(transitive_closure_control()$max_continuation_gap, "integer")

})


test_edges <- tibble(
  from_id = as.character(c(1,1,2,5,6,7)),
  to_id = as.character(c(2,3,4,6,7,8))
)


# test_that("SQLite does transitive closure", {
#   
#   conSQLite <- connect_db_local("inst/ramses-db.sqlite")
#   dbplyr::db_copy_to(conSQLite,
#                      "ramses_test_edges",
#                      test_edges,
#                      overwrite = T,
#                      temporary = F)
#   
# }) 