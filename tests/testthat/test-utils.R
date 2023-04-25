test_that(".throw_error_method_not_implemented", {
  expect_error(.throw_error_method_not_implemented("bidule"))
})

test_that(".compute_date_dimensions", {
  
  expect_error(
    .compute_date_dimensions(date_min = as.Date("1957-03-25"), date_max = as.Date(NA))
  )
  expect_error(
    .compute_date_dimensions(date_min = as.Date("1957-03-25"), date_max = "1957-03-25")
  )
  expect_error(
    .compute_date_dimensions(date_min = as.Date("1957-03-25"), date_max = 2)
  )
  expect_error(
    .compute_date_dimensions(date_min = as.Date("1957-03-25"), date_max = c(as.Date("1957-03-25"), as.Date("1957-03-26")))
  )
  expect_error(
    .compute_date_dimensions(date_min = c(as.Date("1957-03-25"), as.Date("1957-03-26")), date_max = as.Date("1957-03-25"))
  )
  
  expected_df <- data.frame(
    date = as.Date("1957-03-25"), 
    date_string_iso = "1957-03-25", 
    date_string_dd_mm_yyyy = "25/03/1957", 
    date_string_dd_mm_yy = "25/03/57", 
    date_string_full = "25 March 1957", 
    calendar_year = 1957L,
    calendar_quarter = "Q1",
    calendar_month = 3L, 
    calendar_month_name = "March", 
    calendar_month_short = "Mar", 
    day = 25L, 
    day_name = "Monday",
    day_name_short = "Mon", 
    week_day_numeric = 1L, 
    week_starting = as.Date("1957-03-25"), 
    week_ending = as.Date("1957-03-31"), 
    financial_year_uk = "1956/57", 
    financial_quarter_uk = "Q4", 
    financial_year_quarter_uk = "1956/57 Q4"
  )
  
  expect_equal(
    .compute_date_dimensions(date_min = as.Date("1957-03-25"), date_max = as.Date("1957-03-25")),
    expected_df
  )
})
