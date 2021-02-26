
#' Verify that the `observation_code` is unique
#'
#' @param conn a database connection
#' @param observation_code a string
#' @param observation_code_system (optional) reserved to  instance where 
#' \code{observation_code} is ambiguous and does not uniquely identify 
#' observations across code systems (vocabularies), specify a single
#' code system identifier (eg \code{"http://snomed.info/sct"}) to use.
#' The default is \code{NULL} and will only filter observation using
#' \code{observation_code}.
#' @return a string
#' @noRd
.clinical_investigation_code_validate <- function(
  conn, 
  observation_code,
  observation_code_system
) {
  
  if ( !is.null(observation_code_system) & length(observation_code_system) != 1) {
    stop("`observation_code_system` must be a string of length 1.")
  }
  if ( !is.null(observation_code_system) ) {
  db_observation_codes <- dplyr::filter(
    dplyr::tbl(conn, "inpatient_investigations"), 
    observation_code %in% !!observation_code & 
      observation_code_system == !!observation_code_system
    )
  } else {
    db_observation_codes <- dplyr::filter(
      dplyr::tbl(conn, "inpatient_investigations"),
      observation_code %in% !!observation_code
    ) 
  }
  
  db_observation_codes <- dplyr::distinct(db_observation_codes,
                                          observation_code_system, 
                                          observation_code) %>% 
    dplyr::group_by(observation_code) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::collect() 

  if ( any(db_observation_codes[["n"]] > 1) ) {
    stop(paste0(
      "Some codes are ambiguous (exist in multiple code systems): '",
      paste(db_observation_codes[db_observation_codes[["n"]] > 1, 
                                 "observation_code"], collapse = "', '"), "'\n",
      "Please use the `observation_code_system` option to avoid ambiguity."
    ))
  } 
  
  if ( any(!observation_code %in% db_observation_codes[["observation_code"]]) ) {
    stop(paste0(
      "Some `observation_code` were not found in the database: '",
      paste(
        observation_code[which(!observation_code %in% 
                                 db_observation_codes[["observation_code"]])],
        collapse = "', '"), "'"
    ))
  }
}

#' Generate a field name for a new therapy table clinical feature
#'
#' @param conn a database connection
#' @param operation a string, eg `"last"`, `"mean"`, `"range"`, `"trend"`
#' @param observation_code a string 
#' @param range_threshold a string, eg "16_18"
#' @param hours an integer
#' @param observation_code_system (optional) reserved to  instance where 
#' \code{observation_code} is ambiguous and does not uniquely identify 
#' observations across code systems (vocabularies), specify a single
#' code system identifier (eg \code{"http://snomed.info/sct"}) to use.
#' The default is \code{NULL} and will only filter observation using
#' \code{observation_code}.
#' @return a string
#' @noRd
.clinical_feature_field_name_generate <- function(conn, 
                                                  operation, 
                                                  observation_code,
                                                  range_threshold = NULL,
                                                  hours,
                                                  observation_code_system) {
  
  stopifnot(all(lapply(list(observation_code,
                            operation,
                            hours), length) == 1))
  stopifnot(is.null(observation_code_system) | (
    length(observation_code_system) == 1 & 
      !is.na(observation_code_system)
  ))
  stopifnot(is.null(range_threshold) | length(range_threshold) == 1)
  stopifnot(!any(unlist(lapply(list(observation_code,
                                    operation,
                                    hours), is.na))))
  operation <- tolower(operation)
  if( is.null(observation_code_system) ) {
    parameter_name <- tbl(conn, "inpatient_investigations") %>% 
      dplyr::filter(observation_code == !!observation_code) %>% 
      dplyr::distinct(observation_display) %>% 
      dplyr::collect()
  } else {
    parameter_name <- tbl(conn, "inpatient_investigations") %>% 
      dplyr::filter(observation_code == !!observation_code &
                      observation_code_system == !!observation_code_system) %>% 
      dplyr::distinct(observation_display) %>% 
      dplyr::collect()
  }
  if( nrow(parameter_name) == 0 ) {
    stop("`observation_code` ", observation_code, " not found in database.")
  }
  parameter_name <- parameter_name[["observation_display"]]
  parameter_name <- gsub("[[:punct:]]", "", tolower(unique(parameter_name)))
  parameter_name <- gsub(" ", "_", trimws(parameter_name))
  if (!is.null(range_threshold)){
    parameter_name <- paste0(parameter_name, range_threshold)
  }
  paste0(operation, "_", parameter_name, "_", as.integer(hours), "h")
}


#' @importFrom dbplyr sql
.clinical_feature_observations_fetch <- function(x, 
                                                 TT, 
                                                 therapy_record, 
                                                 observation_code, 
                                                 hours,
                                                 observation_code_system) {
  
  if(is(x@conn, "SQLiteConnection")) {
    sql_condition_0 <- paste0(
      "datetime(observation_datetime) BETWEEN '",
      .format_str_time_sqlite.POSIXct(therapy_record$therapy_start - 3600 * hours), "' AND '",
      .format_str_time_sqlite.POSIXct(therapy_record$therapy_end), 
      "'"
    )
    sql_condition_1 <- paste0(
      "datetime(observation_datetime) <= datetime(t_start) AND ",
      "datetime(observation_datetime) >= ", 
      "datetime(t_start, -", hours," || ' hours')")
  } else if(is(x@conn, "PqConnection")) {
    sql_condition_0 <- paste0(
      "observation_datetime BETWEEN '",
      format(therapy_record$therapy_start - 3600 * hours, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "' AND '",
      format(therapy_record$therapy_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "'"
    )
    sql_condition_1 <- paste0(
      "observation_datetime <= t_start AND ",
      "observation_datetime >= (t_start - interval '", hours, "h')")
  } else {
    .throw_error_method_not_implemented(".clinical_feature_threshold()",
                                        class(x@conn))
  }
  
  if( !is.null(observation_code_system) ) {
    all_observations <- tbl(x@conn, "inpatient_investigations") %>%
      dplyr::filter(observation_code_system == !!observation_code_system)
  } else {
    all_observations <- tbl(x@conn, "inpatient_investigations")
  }
  
  all_observations <- all_observations %>% 
    dplyr::filter(patient_id %in% !!therapy_record$patient_id &
                    observation_code %in% !!observation_code &
                    status %in% c("final", "preliminary", "corrected", "amended") &
                    !is.na(observation_value_numeric) &
                    dplyr::sql(sql_condition_0)) %>%
    dplyr::select(patient_id, observation_datetime,
                  observation_code, observation_value_numeric)
  
  observations_linked <- dplyr::inner_join(TT, 
                                           all_observations, 
                                           by = "patient_id") %>% 
    dplyr::filter(dplyr::sql(sql_condition_1))
  
  observations_linked
}


.clinical_feature_last <- function(x, observation_code, hours, observation_code_system) {
  
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "last", 
    observation_code = observation_code, 
    hours = hours,
    observation_code_system = observation_code_system)
  
  observations_linked <- .clinical_feature_observations_fetch(
    x = x, 
    TT = TT,
    therapy_record = therapy_record,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(patient_id, t) %>% 
    dplyr::mutate(keep = dplyr::row_number(dplyr::desc(observation_datetime))) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(keep == 1) %>% 
    dplyr::transmute(patient_id, 
                     t, 
                     {{field_name}} := observation_value_numeric)
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}


#' Therapy table feature: latest clinical observation value
#'
#' @description Add a clinical feature (variable) to a therapy episode table 
#' containing the latest value of clinical observations of interest
#' @param x a \code{\link{TherapyEpisode}} object
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()})
#' @param hours the maximum number of hours the observation should date back from
#' \code{t_start}, the starting time of every row in the therapy table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier (example:
#'  \code{"http://snomed.info/sct"}) of \code{observation_code}.
#' 
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @details The feature will be computed exclusively on numeric investigations 
#' marked with status \code{"final"}, \code{"preliminary"}, \code{"corrected"}, 
#' or \code{"amended"}.
#' @return an object of class \code{\link{TherapyEpisode}}
#' @rdname clinical_feature_last
#' @export
#' @include objects.R
#' @examples 
#' \dontrun{
#' conSQLite <- create_mock_database("example.sqlite")
#' temperature_check <- clinical_feature_last(
#'    TherapyEpisode(conSQLite, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_code = "8310-5",
#'    hours = 24
#'    )
#' str(therapy_table(temperature_check, collect = TRUE))
#' }
setGeneric(
  "clinical_feature_last", 
  function(x, observation_code, hours, observation_code_system = NULL) standardGeneric("clinical_feature_last"), 
  signature = "x")


#' @rdname clinical_feature_last
#' @export
setMethod(
  "clinical_feature_last",
  c(x = "TherapyEpisode"),
  function(x, observation_code, hours, observation_code_system = NULL) {
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    .clinical_investigation_code_validate(conn = x@conn, 
                                          observation_code = observation_code, 
                                          observation_code_system = observation_code_system)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_last(x = x, 
                                  observation_code = observation_code[[i]], 
                                  hours = hours, 
                                  observation_code_system = observation_code_system)
    }
    
    return(x)
  }
)


.clinical_feature_mean <- function(x, observation_code, hours, observation_code_system) {
  
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "mean", 
    observation_code = observation_code, 
    hours = hours,
    observation_code_system = observation_code_system)
  field_name_N <- paste0(field_name, "_N")
  
  observations_linked <- .clinical_feature_observations_fetch(
    x = x, 
    TT = TT,
    therapy_record = therapy_record,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(patient_id, t) %>% 
    dplyr::summarise(
      {{field_name}} := mean(observation_value_numeric, na.rm = TRUE),
      {{field_name_N}} := dplyr::n()
    )
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}


#' Therapy table feature: running mean value of a clinical observation
#'
#' @description Add a clinical feature (variable) to a therapy episode table 
#' containing the arithmetic mean of clinical observations of interest
#' @param x a \code{\link{TherapyEpisode}} object
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}
#' @param hours the maximum number of hours the observations included in the mean
#' should date back from \code{t_start}, the starting time of every row 
#' in the therapy table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier (example:
#'  \code{"http://snomed.info/sct"}) of \code{observation_code}.
#' 
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @details The feature will be computed exclusively on numeric investigations 
#' marked with status \code{"final"}, \code{"preliminary"}, \code{"corrected"}, 
#' or \code{"amended"}.
#' 
#' @return an object of class \code{\link{TherapyEpisode}}
#' @rdname clinical_feature_mean
#' @export
#' @include objects.R
#' @examples 
#' \dontrun{
#' conSQLite <- create_mock_database("example.sqlite")
#' temperature_check <- clinical_feature_mean(
#'    TherapyEpisode(conSQLite, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_code = "8310-5",
#'    hours = 24
#'    )
#' str(therapy_table(temperature_check, collect = TRUE))
#' }
setGeneric(
  "clinical_feature_mean", 
  function(x, observation_code, hours, observation_code_system = NULL) standardGeneric("clinical_feature_mean"), 
  signature = "x")


#' @rdname clinical_feature_mean
#' @export
setMethod(
  "clinical_feature_mean",
  c(x = "TherapyEpisode"),
  function(x, observation_code, hours, observation_code_system = NULL) {
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    .clinical_investigation_code_validate(x@conn, 
                                          observation_code, 
                                          observation_code_system)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_mean(x, observation_code[[i]], hours, observation_code_system)
    }
    
    return(x)
  }
)


.clinical_feature_ols_trend <- function(x, observation_code, hours, observation_code_system) {
  
  final_slope <- observation_datetime_int <- regression_N <- NULL
  slope_denominator <- slope_numerator <- t_bar <- y_bar <- NULL
  
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "ols", 
    observation_code = observation_code, 
    hours = hours,
    observation_code_system = observation_code_system)
  field_name_slope <- paste0(field_name, "_slope")
  field_name_intercept <- paste0(field_name, "_intercept")
  field_name_N <- paste0(field_name, "_N")
  
  observations_linked <- .clinical_feature_observations_fetch(
    x = x, 
    TT = TT,
    therapy_record = therapy_record,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  )
  
  if(is(x@conn, "SQLiteConnection")) {
    observations_linked <- dplyr::mutate(
      observations_linked,
      observation_datetime_int = dplyr::sql(
        "( strftime('%s', observation_datetime) - 
           strftime('%s', t_start) ) / 3600.0")
      )
  } else if(is(x@conn, "PqConnection")) {
    observations_linked <- dplyr::mutate(
      observations_linked,
      observation_datetime_int = dplyr::sql(
        "( EXTRACT(EPOCH FROM observation_datetime) - 
           EXTRACT(EPOCH FROM t_start) ) / 3600.0"
        ))
  } else {
    .throw_error_method_not_implemented(".clinical_feature_ols_trend()",
                                        class(x@conn))
  }
  
  observations_linked <- observations_linked %>% 
    dplyr::group_by(patient_id, t) %>%
    dplyr::mutate(
      y_bar = mean(observation_value_numeric, na.rm = TRUE),
      t_bar = mean(observation_datetime_int, na.rm = TRUE)
    ) %>% 
    dplyr::summarise(
      slope_numerator = sum(
        (observation_value_numeric - y_bar) *
          (observation_datetime_int - t_bar),
        na.rm = TRUE),
      slope_denominator = sum(
        (observation_datetime_int - t_bar) *
          (observation_datetime_int - t_bar),
        na.rm = TRUE),
      regression_N = dplyr::n(),
      y_bar = mean(y_bar, na.rm = TRUE),
      t_bar = mean(t_bar, na.rm = TRUE)
    ) %>%
    dplyr::mutate(final_slope = slope_numerator /
                    dplyr::if_else(slope_denominator == 0, NA_real_, slope_denominator)
    ) %>%
    dplyr::transmute(t,
                     patient_id,
                     {{field_name_intercept}} := dplyr::if_else(
                       is.na(final_slope),
                       NA_real_,
                       y_bar - final_slope * t_bar),
                     {{field_name_slope}} := final_slope,
                     {{field_name_N}} := regression_N)
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}


#' Therapy table feature: temporal trend of clinical observations
#'
#' @description Add clinical feature (variables) to a therapy episode table 
#' containing the Ordinary Least Squares (OLS) intercept and slope of clinical
#' observations of interest
#' @param x a \code{\link{TherapyEpisode}} object
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}
#' @param hours the maximum number of hours the observations should date back from
#' \code{t_start}, the starting time of every row in the therapy table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier (example:
#'  \code{"http://snomed.info/sct"}) of \code{observation_code}.
#' 
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @details The feature will be computed exclusively on numeric investigations 
#' marked with status \code{"final"}, \code{"preliminary"}, \code{"corrected"}, 
#' or \code{"amended"}.
#' 
#' The returned regression slope coefficient corresponds to the mean change 
#' associated with a 1-hour time increment.
#' 
#' The returned regression intercept is defined with respect to time equals
#' zero at \code{t_start}. It thus corresponds to the value of the linear 
#' (straight line) extrapolation of the trend to \code{t_start}.
#' @return an object of class \code{\link{TherapyEpisode}}
#' @rdname clinical_feature_ols_trend
#' @export
#' @include objects.R
#' @examples 
#' \dontrun{
#' conSQLite <- create_mock_database("example.sqlite")
#' temperature_check <- clinical_feature_ols_trend(
#'    TherapyEpisode(conSQLite, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_code = "8310-5",
#'    hours = 24
#'    )
#' str(therapy_table(temperature_check, collect = TRUE))
#' }
setGeneric(
  "clinical_feature_ols_trend", 
  function(x, observation_code, hours, observation_code_system = NULL) standardGeneric("clinical_feature_ols_trend"), 
  signature = "x")


#' @rdname clinical_feature_ols_trend
#' @export
setMethod(
  "clinical_feature_ols_trend",
  c(x = "TherapyEpisode"),
  function(x, observation_code, hours, observation_code_system = NULL) {
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    .clinical_investigation_code_validate(x@conn, 
                                          observation_code, 
                                          observation_code_system)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_ols_trend(x, observation_code[[i]], hours, observation_code_system)
    }
    
    return(x)
  }
)



.clinical_feature_threshold <- function(x, observation_code, threshold, hours, observation_code_system) {
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
      conn = x@conn, 
      operation = "threshold",
      observation_code = observation_code, 
      hours = hours,
      range_threshold = threshold,
      observation_code_system = observation_code_system)

  field_name_under <- paste0(field_name, "_under")
  field_name_over <- paste0(field_name, "_strictly_over")
  
  sql_under <- paste0("observation_value_numeric <= ", threshold)
  sql_over <- paste0("observation_value_numeric > ", threshold)
  
  observations_linked <- .clinical_feature_observations_fetch(
    x = x, 
    TT = TT,
    therapy_record = therapy_record,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(patient_id, t) %>%
    dplyr::summarise(
      {{field_name_under}} := sum(dplyr::case_when(
        dplyr::sql(sql_under) ~ 1L, TRUE ~ 0L
      ), na.rm = TRUE),
      {{field_name_over}} := sum(dplyr::case_when(
        dplyr::sql(sql_over) ~ 1L, TRUE ~ 0L
      ), na.rm = TRUE))
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}

.clinical_feature_interval <- function(x, observation_code, lower_bound, upper_bound, hours, observation_code_system) {
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = paste0("range"),
    range_threshold = paste0(lower_bound, "_", upper_bound),
    observation_code = observation_code, 
    hours = hours,
    observation_code_system = observation_code_system)
  field_name_under <- paste0(field_name, "_strictly_under")
  field_name_in_range <- paste0(field_name, "_in_range")
  field_name_over <- paste0(field_name, "_strictly_over")
  
  sql_under <- paste0("observation_value_numeric < ", lower_bound)
  sql_in_range <- paste0("observation_value_numeric BETWEEN ", lower_bound, " AND ", upper_bound)
  sql_over <- paste0("observation_value_numeric > ", upper_bound)
  
  observations_linked <- .clinical_feature_observations_fetch(
    x = x, 
    TT = TT,
    therapy_record = therapy_record,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(patient_id, t) %>%
    dplyr::summarise(
      {{field_name_under}} := sum(dplyr::case_when(
        dplyr::sql(sql_under) ~ 1L, TRUE ~ 0L
      ), na.rm = TRUE),
      {{field_name_in_range}} := sum(dplyr::case_when(
        dplyr::sql(sql_in_range) ~ 1L, TRUE ~ 0L
      ), na.rm = TRUE),
      {{field_name_over}} := sum(dplyr::case_when(
        dplyr::sql(sql_over) ~ 1L, TRUE ~ 0L
      ), na.rm = TRUE))
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}


#' Therapy table feature: number of clinical observations falling in an interval
#' 
#' @description Add clinical features (variables) to a therapy episode table 
#' containing the number of observations falling (a) above/below a given threshold
#' or (b) inside/outside a given interval.
#' @param x a \code{\link{TherapyEpisode}} object
#' @param observation_intervals a named list of numeric vectors of length 1 
#' (for a threshold) or 2 (for an interval). Names of vectors must match the
#'  \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}. See example
#' @param hours the maximum number of hours the observation should date back from
#' \code{t_start}, the starting time of every row in the therapy table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier (example:
#'  \code{"http://snomed.info/sct"}) of \code{observation_code}.
#' 
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @details The feature will be computed exclusively on numeric investigations 
#' marked with status \code{"final"}, \code{"preliminary"}, \code{"corrected"}, 
#' or \code{"amended"}.
#' @return an object of class \code{\link{TherapyEpisode}}
#' @rdname clinical_feature_interval
#' @export
#' @examples 
#' \dontrun{
#' conSQLite <- create_mock_database("example.sqlite")
#' 
#' temperature_interval <- clinical_feature_interval(
#'    TherapyEpisode(conSQLite, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_intervals = list("8310-5" = c(36, 38)),
#'    hours = 24
#'    )
#' str(therapy_table(temperature_interval, collect = TRUE))
#' 
#' temperature_threshold <- clinical_feature_interval(
#'    TherapyEpisode(conSQLite, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_intervals = list("8310-5" = 38),
#'    hours = 24
#'    )
#' str(therapy_table(temperature_threshold, collect = TRUE))
#' }
#' @include objects.R
setGeneric(
  "clinical_feature_interval", 
  function(x, observation_intervals, hours, observation_code_system = NULL) standardGeneric("clinical_feature_interval"), 
  signature = "x")


#' @rdname clinical_feature_interval
#' @export
setMethod(
  "clinical_feature_interval",
  c(x = "TherapyEpisode"),
  function(x, observation_intervals, hours, observation_code_system = NULL) {
    stopifnot(is.list(observation_intervals))
    stopifnot(all(unlist(lapply(observation_intervals, length)) %in% 1:2))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    
    input_observation_codes <- names(observation_intervals)
    .clinical_investigation_code_validate(x@conn, 
                                          input_observation_codes,
                                          observation_code_system)
    
    for (i in seq_len(length(observation_intervals))) {
      if(length(observation_intervals[[i]]) == 1) {
        stopifnot(!is.na(observation_intervals[[i]]) &
                    !is.infinite(observation_intervals[[i]]))
        x <- .clinical_feature_threshold(x = x, 
                                         observation_code = input_observation_codes[[i]], 
                                         threshold = observation_intervals[[i]],
                                         hours = hours, 
                                         observation_code_system = observation_code_system)
      } else {
        stopifnot(!any(is.na(observation_intervals[[i]])) &
                    !any(is.infinite(observation_intervals[[i]])))
        x <- .clinical_feature_interval(x = x, 
                                        observation_code = input_observation_codes[[i]], 
                                        lower_bound = sort(observation_intervals[[i]])[1],
                                        upper_bound = sort(observation_intervals[[i]])[2],
                                        hours = hours,
                                        observation_code_system = observation_code_system)
      }
    }
    
    return(x)
  }
)

