library(covr)
devtools::load_all()

out <- testthat::test_file("travis/testthat/test_warehousing.R")

codecov()
