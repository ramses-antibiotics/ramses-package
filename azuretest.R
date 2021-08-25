devtools::load_all()
options("testthat.output_file" = "test-results.xml")
devtools::test(reporter = JunitReporter$new())
