
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

