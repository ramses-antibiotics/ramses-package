
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
    stop("`observation_code_system` must be a string of length 1.", call. = FALSE)
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

  if ( any(as.numeric(db_observation_codes[["n"]]) > 1) ) {
    stop(paste0(
      "Some codes are ambiguous (exist in multiple code systems): '",
      paste(db_observation_codes[db_observation_codes[["n"]] > 1, 
                                 "observation_code"], collapse = "', '"), "'\n",
      "Please use the `observation_code_system` option to avoid ambiguity."
    ), call. = FALSE)
  } 
  
  if ( any(!observation_code %in% db_observation_codes[["observation_code"]]) ) {
    stop(paste0(
      "Some `observation_code` were not found in the database: '",
      paste(
        observation_code[which(!observation_code %in% 
                                 db_observation_codes[["observation_code"]])],
        collapse = "', '"), "'"
    ), call. = FALSE)
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
    stop(paste0("`observation_code` ", observation_code, " not found in database."), 
         call. = FALSE)
  }
  parameter_name <- parameter_name[["observation_display"]]
  parameter_name <- gsub("[[:punct:]]", "", tolower(unique(parameter_name)))
  parameter_name <- gsub(" ", "_", trimws(parameter_name))
  if (!is.null(range_threshold)){
    parameter_name <- paste0(parameter_name, range_threshold)
  }
  paste0(operation, "_", parameter_name, "_", as.integer(hours), "h")
}


#' Get the object identifier field name in the remote table
#'
#' @param x an object of class `TherapyEpisode` or `Encounter`
#' @return a string
#' @noRd
.clinical_feature_object_id_field <- function(x) {
  if (is(x, "Encounter")) {
    return("encounter_id")
  } else if (is(x, "TherapyEpisode")) {
    return("therapy_id")
  } else {
    .throw_error_method_not_implemented(".clinical_feature_object_id_field()", 
                                        class(x))
  }
}

#' @importFrom dbplyr sql
.clinical_feature_observations_fetch <- function(x, 
                                                 LT, 
                                                 observation_code, 
                                                 hours,
                                                 observation_code_system) {
  
  if(is(x@conn, "PqConnection") | is(x@conn, "duckdb_connection")) {
    sql_condition_1 <- paste0(
      "(t_start) > (observation_datetime) ",
      "AND (t_start) <= (observation_datetime + interval '", hours, "h' )"
      )
  } else {
    .throw_error_method_not_implemented(".clinical_feature_threshold()",
                                        class(x@conn))
  }
  
  observations_linked <- dplyr::inner_join(
    LT, 
    dplyr::select(tbl(x@conn, "inpatient_investigations"),
                  patient_id, observation_datetime,
                  observation_code, observation_value_numeric,
                  status, observation_code_system),
    by = "patient_id"
  ) %>% 
    dplyr::filter(
      dplyr::sql(sql_condition_1) &
        status %in% c("final", "preliminary", "corrected", "amended") &
        observation_code %in% !!observation_code & 
        !is.na(observation_value_numeric)
    )
  
  if( !is.null(observation_code_system) ) {
    observations_linked <- observations_linked %>% 
      dplyr::filter(observation_code_system == !!observation_code_system)
  }
  
  observations_linked
}


.clinical_feature_last <- function(x, observation_code, hours, observation_code_system, compute) {
  stopifnot(is.logical(compute))
  x_entity_id_field_name <- .clinical_feature_object_id_field(x)
  if(compute) {
    x <- compute(x)
  }
  LT <- x@longitudinal_table
  
  object_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "last", 
    observation_code = observation_code, 
    hours = hours,
    observation_code_system = observation_code_system)
  
  observations_linked <- .clinical_feature_observations_fetch(
    x = x, 
    LT = LT,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(.data$patient_id, .data[[x_entity_id_field_name]], .data$t) %>% 
    dplyr::mutate(keep = dplyr::row_number(dplyr::desc(observation_datetime))) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(.data$keep == 1) %>% 
    dplyr::transmute(.data$t,
                     .data$patient_id, 
                     .data[[x_entity_id_field_name]], 
                     {{field_name}} := .data$observation_value_numeric)
  
  x@longitudinal_table <- dplyr::left_join(
    LT,
    observations_linked,
    by = c("patient_id", x_entity_id_field_name, "t")
  )
  if(compute) {
    x <- compute(x)
  }
  
  return(x)
}


#' Clinical feature: latest clinical observation value
#'
#' @description Add a clinical feature (variable) to a therapy or encounter 
#' longitudinal table. The feature corresponds to the latest value of 
#' clinical observations of interest carried forward for up to a maximum set by
#' \code{hours}.
#' @param x an object of class \code{\link{TherapyEpisode}} or \code{\link{Encounter}}
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()})
#' @param hours the maximum number of hours the observation should date back from
#' \code{t_start}, the starting time of every row in the longitudinal table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier of \code{observation_code} 
#' (for example: \code{"http://snomed.info/sct"}).
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @param compute if \code{TRUE} (the default), the remote therapy table will 
#' be computed on the remote server. This is generally faster.
#' @details The feature will be computed exclusively on numeric investigations 
#' marked with status \code{"final"}, \code{"preliminary"}, \code{"corrected"}, 
#' or \code{"amended"}.
#' @return an object of class \code{\link{TherapyEpisode}} or 
#' \code{\link{Encounter}}
#' @rdname clinical_feature_last
#' @export
#' @include objects.R
#' @examples 
#' \dontrun{
#' fake_db <- create_mock_database("example.duckdb")
#' temperature_check <- clinical_feature_last(
#'    TherapyEpisode(fake_db, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_code = "8310-5",
#'    hours = 24
#'    )
#' str(longitudinal_table(temperature_check, collect = TRUE))
#' }
setGeneric(
  "clinical_feature_last", 
  function(x, observation_code, hours, observation_code_system = NULL, compute = TRUE) {
    tryCatch(x)
    standardGeneric("clinical_feature_last")
  }, 
  signature = "x")


#' @rdname clinical_feature_last
#' @export
setMethod(
  "clinical_feature_last",
  c(x = "RamsesObject"),
  function(x, observation_code, hours, observation_code_system = NULL, compute = TRUE) {
    if (!(is(x, "TherapyEpisode") || is(x, "Encounter"))) {
      stop("`x` must be an object of class `TherapyEpisode` or `Encounter`.")
    }
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    .clinical_investigation_code_validate(conn = x@conn, 
                                          observation_code = observation_code, 
                                          observation_code_system = observation_code_system)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_last(x = x, 
                                  observation_code = observation_code[[i]], 
                                  hours = hours, 
                                  observation_code_system = observation_code_system,
                                  compute = compute)
    }
    
    return(x)
  }
)


.clinical_feature_mean <- function(x, observation_code, hours, observation_code_system, compute) {
  stopifnot(is.logical(compute))
  x_entity_id_field_name <- .clinical_feature_object_id_field(x)
  if(compute) {
    x <- compute(x)
  }
  LT <- x@longitudinal_table
  
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "mean", 
    observation_code = observation_code, 
    hours = hours,
    observation_code_system = observation_code_system)
  field_name_N <- paste0(field_name, "_N")
  
  observations_linked <- .clinical_feature_observations_fetch(
    x = x, 
    LT = LT,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(.data$patient_id, .data[[x_entity_id_field_name]], .data$t) %>% 
    dplyr::summarise(
      {{field_name}} := mean(.data$observation_value_numeric, na.rm = TRUE),
      {{field_name_N}} := dplyr::n()
    )
  
  x@longitudinal_table <- dplyr::left_join(
    LT,
    observations_linked,
    by = c("patient_id", x_entity_id_field_name, "t")
  )
  
  if(compute) {
    x <- compute(x)
  }
  
  return(x)
}


#' Clinical feature: running mean value of a clinical observation
#'
#' @description Add a clinical feature (variable) to a therapy or encounter 
#' longitudinal table. The feature corresponds to the arithmetic mean of
#' clinical observations of interest over a time span set by \code{hours}.
#' @param x an object of class \code{\link{TherapyEpisode}} or \code{\link{Encounter}}
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}).
#' @param hours the maximum number of hours the observations included in the mean
#' should date back from \code{t_start}, the starting time of every row 
#' in the therapy table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier of \code{observation_code} 
#' (for example: \code{"http://snomed.info/sct"}).
#' 
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @param compute if \code{TRUE} (the default), the remote therapy table will 
#' be computed on the remote server. This is generally faster.
#' @details The feature will be computed exclusively on numeric investigations 
#' marked with status \code{"final"}, \code{"preliminary"}, \code{"corrected"}, 
#' or \code{"amended"}.
#' 
#' @return an object of class \code{\link{TherapyEpisode}} or 
#' \code{\link{Encounter}}
#' @rdname clinical_feature_mean
#' @export
#' @include objects.R
#' @examples 
#' \dontrun{
#' fake_db <- create_mock_database("example.duckdb")
#' temperature_check <- clinical_feature_mean(
#'    TherapyEpisode(fake_db, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_code = "8310-5",
#'    hours = 24
#'    )
#' str(longitudinal_table(temperature_check, collect = TRUE))
#' }
setGeneric(
  "clinical_feature_mean", 
  function(x, observation_code, hours, observation_code_system = NULL, compute = TRUE) {
    tryCatch(x)
    standardGeneric("clinical_feature_mean")
  }, 
  signature = "x")


#' @rdname clinical_feature_mean
#' @export
setMethod(
  "clinical_feature_mean",
  c(x = "RamsesObject"),
  function(x, observation_code, hours, observation_code_system = NULL, compute = TRUE) {
    if (!(is(x, "TherapyEpisode") || is(x, "Encounter"))) {
      stop("`x` must be an object of class `TherapyEpisode` or `Encounter`.")
    }
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    .clinical_investigation_code_validate(x@conn, 
                                          observation_code, 
                                          observation_code_system)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_mean(x, observation_code[[i]], hours, observation_code_system, compute = compute)
    }
    
    return(x)
  }
)


.clinical_feature_ols_trend <- function(x, observation_code, hours, observation_code_system, compute) {
  stopifnot(is.logical(compute))
  x_entity_id_field_name <- .clinical_feature_object_id_field(x)
  final_slope <- observation_datetime_int <- regression_N <- NULL
  slope_denominator <- slope_numerator <- t_bar <- y_bar <- NULL
  
  if(compute) {
    x <- compute(x)
  }
  LT <- x@longitudinal_table

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
    LT = LT,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  )
  
  if(is(x@conn, "PqConnection") | is(x@conn, "duckdb_connection")) {
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
    dplyr::group_by(.data$patient_id, .data[[x_entity_id_field_name]], .data$t) %>%
    dplyr::mutate(
      y_bar = mean(.data$observation_value_numeric, na.rm = TRUE),
      t_bar = mean(.data$observation_datetime_int, na.rm = TRUE)
    ) %>% 
    dplyr::summarise(
      slope_numerator = sum(
        (.data$observation_value_numeric - .data$y_bar) *
          (.data$observation_datetime_int - .data$t_bar),
        na.rm = TRUE),
      slope_denominator = sum(
        (.data$observation_datetime_int - .data$t_bar) *
          (.data$observation_datetime_int - .data$t_bar),
        na.rm = TRUE),
      regression_N = dplyr::n(),
      y_bar = mean(.data$y_bar, na.rm = TRUE),
      t_bar = mean(.data$t_bar, na.rm = TRUE)
    ) %>%
    dplyr::mutate(final_slope = .data$slope_numerator /
                    dplyr::if_else(.data$slope_denominator == 0,
                                   NA_real_, 
                                   .data$slope_denominator)
    ) %>%
    dplyr::transmute(.data$t,
                     .data$patient_id,
                     .data[[x_entity_id_field_name]],
                     {{field_name_intercept}} := dplyr::if_else(
                       is.na(.data$final_slope),
                       NA_real_,
                       .data$y_bar - .data$final_slope * .data$t_bar),
                     {{field_name_slope}} := .data$final_slope,
                     {{field_name_N}} := .data$regression_N)
  
  x@longitudinal_table <- dplyr::left_join(
    LT,
    observations_linked,
    by = c("patient_id", x_entity_id_field_name, "t")
  )
  
  if(compute) {
    x <- compute(x)
  }
  
  return(x)
}


#' Clinical feature: temporal trend of clinical observations
#'
#' @description Add a clinical feature (variable) to a therapy or encounter 
#' longitudinal table. The feature corresponds to the Ordinary Least Squares (OLS) 
#' intercept and slope of clinical observations of interest
#' @param x an object of class \code{\link{TherapyEpisode}} or \code{\link{Encounter}}
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}
#' @param hours the maximum number of hours the observations should date back from
#' \code{t_start}, the starting time of every row in the longitudinal table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier of \code{observation_code} 
#' (for example: \code{"http://snomed.info/sct"}).
#' 
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @param compute if \code{TRUE} (the default), the remote therapy table will 
#' be computed on the remote server. This is generally faster.
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
#' @return an object of class \code{\link{TherapyEpisode}} or 
#' \code{\link{Encounter}}
#' @rdname clinical_feature_ols_trend
#' @export
#' @include objects.R
#' @examples 
#' \dontrun{
#' fake_db <- create_mock_database("example.duckdb")
#' temperature_check <- clinical_feature_ols_trend(
#'    TherapyEpisode(fake_db, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_code = "8310-5",
#'    hours = 24
#'    )
#' str(longitudinal_table(temperature_check, collect = TRUE))
#' }
setGeneric(
  "clinical_feature_ols_trend", 
  function(x, observation_code, hours, observation_code_system = NULL, compute = TRUE) {
    tryCatch(x)
    standardGeneric("clinical_feature_ols_trend")
  }, 
  signature = "x")


#' @rdname clinical_feature_ols_trend
#' @export
setMethod(
  "clinical_feature_ols_trend",
  c(x = "RamsesObject"),
  function(x, observation_code, hours, observation_code_system = NULL, compute = TRUE) {
    if (!(is(x, "TherapyEpisode") || is(x, "Encounter"))) {
      stop("`x` must be an object of class `TherapyEpisode` or `Encounter`.")
    }
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    .clinical_investigation_code_validate(x@conn, 
                                          observation_code, 
                                          observation_code_system)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_ols_trend(x, observation_code[[i]], hours, observation_code_system, compute = compute)
    }
    
    return(x)
  }
)



.clinical_feature_threshold <- function(x, observation_code, threshold, hours, observation_code_system, compute) {
  stopifnot(is.logical(compute))
  x_entity_id_field_name <- .clinical_feature_object_id_field(x)
  if(compute) {
    x <- compute(x)
  }
  LT <- x@longitudinal_table
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
    LT = LT,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(.data$patient_id, .data[[x_entity_id_field_name]], .data$t) %>%
    dplyr::summarise(
      {{field_name_under}} := sum(dplyr::case_when(
        dplyr::sql(sql_under) ~ 1L, TRUE ~ 0L
      ), na.rm = TRUE),
      {{field_name_over}} := sum(dplyr::case_when(
        dplyr::sql(sql_over) ~ 1L, TRUE ~ 0L
      ), na.rm = TRUE))
  
  x@longitudinal_table <- dplyr::left_join(
    LT,
    observations_linked,
    by = c("patient_id", x_entity_id_field_name, "t")
  )
  
  if(compute) {
    x <- compute(x)
  }
  
  return(x)
}

.clinical_feature_interval <- function(x, observation_code, lower_bound, upper_bound, hours, observation_code_system, compute) {
  stopifnot(is.logical(compute))
  x_entity_id_field_name <- .clinical_feature_object_id_field(x)
  if(compute) {
    x <- compute(x)
  }
  LT <- x@longitudinal_table
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
    LT = LT,
    observation_code = observation_code,
    hours = hours,
    observation_code_system = observation_code_system
  ) %>% 
    dplyr::group_by(.data$patient_id, .data[[x_entity_id_field_name]], .data$t) %>%
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
  
  x@longitudinal_table <- dplyr::left_join(
      LT,
      observations_linked,
      by = c("patient_id", x_entity_id_field_name, "t")
    )
  
  if(compute) {
    x <- compute(x)
  }
  
  return(x)
}


#' Clinical feature: number of clinical observations falling in an interval
#' 
#' @description Add a clinical feature (variable) to a therapy or encounter 
#' longitudinal table. The feature corresponds to the number of observations falling 
#' (a) above/below a given threshold or (b) inside/outside a given interval 
#' depending on values provided to \code{observation_intervals}.
#' @param x an object of class \code{\link{TherapyEpisode}} or \code{\link{Encounter}}
#' @param observation_intervals a named list of numeric vectors of length 1 
#' (for a threshold) or 2 (for an interval). Names of vectors must match the
#'  \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}).
#' @param hours the maximum number of hours the observation should date back from
#' \code{t_start}, the starting time of every row in the longitudinal table
#' @param observation_code_system (optional, reserved to situations where 
#' \code{observation_code} is ambiguous across code systems/vocabularies) a single 
#' character string specifying the code system identifier of \code{observation_code} 
#' (for example: \code{"http://snomed.info/sct"}).
#' 
#' The default (\code{NULL}) filters observations using the \code{observation_code} only.
#' @param compute if \code{TRUE} (the default), the remote therapy table will 
#' be computed on the remote server. This is generally faster.
#' @details The feature will be computed exclusively on numeric investigations 
#' marked with status \code{"final"}, \code{"preliminary"}, \code{"corrected"}, 
#' or \code{"amended"}.
#' @return an object of class \code{\link{TherapyEpisode}} or 
#' \code{\link{Encounter}}
#' @rdname clinical_feature_interval
#' @export
#' @examples 
#' \dontrun{
#' fake_db <- create_mock_database("example.duckdb")
#' 
#' temperature_interval <- clinical_feature_interval(
#'    TherapyEpisode(fake_db, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_intervals = list("8310-5" = c(36, 38)),
#'    hours = 24
#'    )
#' str(longitudinal_table(temperature_interval, collect = TRUE))
#' 
#' temperature_threshold <- clinical_feature_interval(
#'    TherapyEpisode(fake_db, "4d611fc8886c23ab047ad5f74e5080d7"),
#'    observation_intervals = list("8310-5" = 38),
#'    hours = 24
#'    )
#' str(longitudinal_table(temperature_threshold, collect = TRUE))
#' }
#' @include objects.R
setGeneric(
  "clinical_feature_interval", 
  function(x, observation_intervals, hours, observation_code_system = NULL, compute = TRUE) {
    tryCatch(x)
    standardGeneric("clinical_feature_interval")
  }, 
  signature = "x")


#' @rdname clinical_feature_interval
#' @export
setMethod(
  "clinical_feature_interval",
  c(x = "RamsesObject"),
  function(x, observation_intervals, hours, observation_code_system = NULL, compute = TRUE) {
    if (!(is(x, "TherapyEpisode") || is(x, "Encounter"))) {
      stop("`x` must be an object of class `TherapyEpisode` or `Encounter`.")
    }
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
                                         observation_code_system = observation_code_system, 
                                         compute = compute)
      } else {
        stopifnot(!any(is.na(observation_intervals[[i]])) &
                    !any(is.infinite(observation_intervals[[i]])))
        x <- .clinical_feature_interval(x = x, 
                                        observation_code = input_observation_codes[[i]], 
                                        lower_bound = sort(observation_intervals[[i]])[1],
                                        upper_bound = sort(observation_intervals[[i]])[2],
                                        hours = hours,
                                        observation_code_system = observation_code_system, 
                                        compute = compute)
      }
    }
    
    return(x)
  }
)

