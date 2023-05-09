.print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

.throw_error_method_not_implemented <- function(function_name, class_name = "") {
  stopifnot(is.character(function_name) & length(function_name) == 1)
  stopifnot(is.character(class_name) & length(class_name) == 1)
  stop(paste(
    function_name,
    "is not implemented for",
    class_name,
    "objects.",
    "Please report this issue on https://github.com/ramses-antibiotics/ramses-package/issues",
    collapse = "\n"
  ))
}

.compute_date_dimensions <- function(date_min, date_max) {
  # Generate the sequence between two dates
  # and compute derived variables
  stopifnot(length(date_min) == 1 & !is.na(date_min) & ( is(date_min, "Date") | is(date_min, "POSIXt") ))
  stopifnot(length(date_max) == 1 & !is.na(date_max) & ( is(date_max, "Date") | is(date_max, "POSIXt") ))
  if(is(date_min, "POSIXt")) date_min <- as.Date(date_min)
  if(is(date_max, "POSIXt")) date_max <- as.Date(date_max)
  
  
  data.frame(date = seq(date_min, date_max, 1)) %>% 
    dplyr::mutate(
      date_string_iso = format(.data$date, format = "%F"),
      date_string_dd_mm_yyyy = format(.data$date, format = "%d/%m/%Y"),
      date_string_dd_mm_yy = format(.data$date, format = "%d/%m/%y"),
      date_string_full = trimws(format(.data$date, format = "%e %B %Y")),
      calendar_year = as.integer(format(.data$date, "%Y")),
      calendar_quarter = paste0("Q", as.POSIXlt(.data$date)$mon%/%3L + 1),
      calendar_month = as.integer(format(.data$date, format = "%m")),
      calendar_month_name = format(.data$date, format = "%B"),
      calendar_month_short = format(.data$date, format = "%b"),
      day = as.integer(format(.data$date, format = "%d")),
      day_name = format(.data$date, format = "%A"),
      day_name_short = format(.data$date, format = "%a"),
      week_day_numeric = as.integer(format(.data$date, format = "%u")),
      week_starting = .data$date - .data$week_day_numeric + 1,
      week_ending = .data$date + 7 - .data$week_day_numeric,
      financial_year_uk = paste0(.data$calendar_year - (.data$calendar_month < 4), "/",
                                 substr(.data$calendar_year - (.data$calendar_month < 4) + 1, 3, 4)),
      financial_quarter_uk = paste0("Q", (.data$calendar_month - 1) %/% 3L + 4 * (.data$calendar_month < 4)),
      financial_year_quarter_uk = paste(.data$financial_year_uk, .data$financial_quarter_uk)
   )
}
