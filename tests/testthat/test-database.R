
test_that("transitive closure parameters are set", {
  
  expect_is(transitive_closure_control(), "list")
  expect_error(transitive_closure_control(-2, 0, 1))
  expect_error(transitive_closure_control(1, -2, 1))
  expect_error(transitive_closure_control(1, 1, -2))
  expect_error(transitive_closure_control(1, 1, c(-2, 0)))
  expect_is(transitive_closure_control()$max_continuation_gap, "Period")

})