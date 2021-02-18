

#' Generate a field name for a new therapy table clinical feature
#'
#' @param conn a database connection
#' @param operation a string, eg `"last"`, `"mean"`, `"range"`, `"trend"`
#' @param observation_code a string 
#' @param hours an integer
#' @return a string
#' @noRd
.clinical_feature_field_name_generate <- function(conn, operation, observation_code, hours) {
  
  stopifnot(all(lapply(list(observation_code,
                            operation,
                            hours), length) == 1))
  stopifnot(!any(unlist(lapply(list(observation_code,
                                    operation,
                                    hours), is.na))))
  stopifnot(all(lapply(list(observation_code,
                            operation,
                            hours), length) == 1))
  operation <- tolower(operation)
  parameter_name <- tbl(conn, "inpatient_investigations") %>% 
    dplyr::filter(observation_code == !!observation_code) %>% 
    dplyr::distinct(observation_display) %>% 
    dplyr::collect()
  parameter_name <- parameter_name[["observation_display"]]
  parameter_name <- gsub("[[:punct:]]", "", tolower(unique(parameter_name)))
  parameter_name <- gsub(" ", "_", trimws(parameter_name))
  
  paste0(operation, "_", parameter_name, "_", as.integer(hours), "h")
}


.clinical_feature_last <- function(x, observation_code, hours) {
  
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "last", 
    observation_code = observation_code, 
    hours = hours)
  
  all_observations <- tbl(x@conn, "inpatient_investigations") %>%
    dplyr::filter(patient_id %in% !!therapy_record$patient_id &
                    observation_code %in% !!observation_code &
                    status %in% c("final", "preliminary", "corrected", "amended") &
                    !is.na(observation_value_numeric) &
                    dplyr::between(observation_datetime,
                                   !!(therapy_record$therapy_start - 3600 * hours),
                                   !!therapy_record$therapy_end)) %>%
    dplyr::select(patient_id, observation_datetime,
                  observation_code, observation_value_numeric)
  
  if(is(x@conn, "SQLiteConnection")) {
    
    sql_condition_1 <- paste0("datetime(t_start, -", hours," || ' hours')")
    observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
      dplyr::filter(
        dplyr::sql("datetime(observation_datetime) <= datetime(t_start)") &
          dplyr::sql("datetime(observation_datetime)") >= dplyr::sql(sql_condition_1)
      ) %>% 
      dplyr::group_by(patient_id, t) %>%
      dplyr::mutate(keep = dplyr::row_number(dplyr::desc(observation_datetime))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(keep == 1) %>%
      dplyr::transmute(patient_id,
                       t,
                       {{field_name}} := observation_value_numeric)
    
  } else if(is(x@conn, "PqConnection")) {
    
    sql_condition_1 <- paste0("observation_datetime >= (t_start - interval '", hours, "h')")
    observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
      dplyr::filter(
        dplyr::sql("observation_datetime <= t_start") &
          dplyr::sql(sql_condition_1)
      ) %>% 
      dplyr::group_by(patient_id, t) %>% 
      dplyr::mutate(keep = dplyr::row_number(dplyr::desc(observation_datetime))) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(keep == 1) %>% 
      dplyr::transmute(patient_id, 
                       t, 
                       {{field_name}} := observation_value_numeric)
    
  } else {
    .throw_error_method_not_implemented("clinical_feature_last()",
                                        class(x@conn))
  }
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}


#' Therapy table feature: most recent clinical observation
#'
#' @description Add a clinical feature to a therapy episode table indicating 
#' the most recent observation by table time \code{t}
#' @param x a \code{\link{TherapyEpisode}} object
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}
#' @param hours the maximum number of hours the observation should date back from
#' \code{t_start}, the starting time of every row in the therapy table
#' @details NOTE: only numeric investigations marked with status \code{"final"}, 
#' \code{"preliminary"}, \code{"corrected"}, or \code{"amended"} will be sourced.
#' @return an object of class \code{\link{TherapyEpisode}}
#' @rdname clinical_feature_last
#' @export
#' @include objects.R
setGeneric(
  "clinical_feature_last", 
  function(x, observation_code, hours) standardGeneric("clinical_feature_last"), 
  signature = "x")


#' @rdname clinical_feature_last
#' @export
setMethod(
  "clinical_feature_last",
  c(x = "TherapyEpisode"),
  function(x, observation_code, hours) {
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_last(x, observation_code[[i]], hours)
    }
    
    return(x)
  }
)


.clinical_feature_mean <- function(x, observation_code, hours) {
  
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "mean", 
    observation_code = observation_code, 
    hours = hours)
  field_name_N <- paste0(field_name, "_N")
  
  all_observations <- tbl(x@conn, "inpatient_investigations") %>%
    dplyr::filter(patient_id %in% !!therapy_record$patient_id &
                    observation_code %in% !!observation_code &
                    status %in% c("final", "preliminary", "corrected", "amended") &
                    !is.na(observation_value_numeric) &
                    dplyr::between(observation_datetime,
                                   !!(therapy_record$therapy_start - 3600 * hours),
                                   !!therapy_record$therapy_end)) %>%
    dplyr::select(patient_id, observation_datetime,
                  observation_code, observation_value_numeric)
  
  if(is(x@conn, "SQLiteConnection")) {
    
    sql_condition_1 <- paste0("datetime(t_start, -", hours," || ' hours')")
    observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
      dplyr::filter(
        dplyr::sql("datetime(observation_datetime) <= datetime(t_start)") &
          dplyr::sql("datetime(observation_datetime)") >= dplyr::sql(sql_condition_1)
      ) %>% 
      dplyr::group_by(patient_id, t) %>% 
      dplyr::summarise(
        {{field_name}} := mean(observation_value_numeric, na.rm = TRUE),
        {{field_name_N}} := dplyr::n()
      )
    
  } else if(is(x@conn, "PqConnection")) {
    
    sql_condition_1 <- paste0("observation_datetime >= (t_start, - interval '", hours, "h')")
    observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
      dplyr::filter(
        dplyr::sql("observation_datetime <= t_start") &
          dplyr::sql(sql_condition_1)
      ) %>% 
      dplyr::group_by(patient_id, t) %>% 
      dplyr::mutate(keep = dplyr::row_number(dplyr::desc(observation_datetime))) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(keep == 1) %>% 
      dplyr::summarise(
        {{field_name}} := mean(observation_value_numeric, na.rm = TRUE),
        {{field_name_N}} := dplyr::n()
      )
    
  } else {
    .throw_error_method_not_implemented("clinical_feature_mean()",
                                        class(x@conn))
  }
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}


#' Therapy table feature: running mean value of clinical observation
#'
#' @description Add a clinical feature to a therapy episode table indicating 
#' the running mean value of clinical observations made until table time \code{t}
#' @param x a \code{\link{TherapyEpisode}} object
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}
#' @param hours the maximum number of hours the observation should date back from
#' \code{t_start}, the starting time of every row in the therapy table
#' @details NOTE: only numeric investigations marked with status \code{"final"}, 
#' \code{"preliminary"}, \code{"corrected"}, or \code{"amended"} will be sourced.
#' 
#' The returned regression slope coefficient corresponds to the mean change 
#' associated with a 1-hour time increment.
#' @return an object of class \code{\link{TherapyEpisode}}
#' @rdname clinical_feature_mean
#' @export
#' @include objects.R
setGeneric(
  "clinical_feature_mean", 
  function(x, observation_code, hours) standardGeneric("clinical_feature_mean"), 
  signature = "x")


#' @rdname clinical_feature_mean
#' @export
setMethod(
  "clinical_feature_mean",
  c(x = "TherapyEpisode"),
  function(x, observation_code, hours) {
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_mean(x, observation_code[[i]], hours)
    }
    
    return(x)
  }
)


.clinical_feature_ols_trend <- function(x, observation_code, hours) {
  
  final_slope <- observation_datetime_int <- regression_N <- NULL
  slope_denominator <- slope_numerator <- t_bar <- y_bar <- NULL
  
  TT <- .therapy_table_create(x@conn, x@id)
  therapy_record <- collect(x)
  field_name <- .clinical_feature_field_name_generate(
    conn = x@conn, 
    operation = "ols", 
    observation_code = observation_code, 
    hours = hours)
  field_name_slope <- paste0(field_name, "_slope")
  field_name_intercept <- paste0(field_name, "_intercept")
  field_name_N <- paste0(field_name, "_N")
  
  all_observations <- tbl(x@conn, "inpatient_investigations") %>%
    dplyr::filter(patient_id %in% !!therapy_record$patient_id &
                    observation_code %in% !!observation_code &
                    status %in% c("final", "preliminary", "corrected", "amended") &
                    !is.na(observation_value_numeric) &
                    dplyr::between(observation_datetime,
                                   !!(therapy_record$therapy_start - 3600 * hours),
                                   !!therapy_record$therapy_end)) %>%
    dplyr::select(patient_id, observation_datetime,
                  observation_code, observation_value_numeric)
  
  if(is(x@conn, "SQLiteConnection")) {
    
    sql_condition_1 <- paste0("datetime(t_start, -", hours," || ' hours')")
    observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
      dplyr::filter(
        dplyr::sql("datetime(observation_datetime) <= datetime(t_start)") &
          dplyr::sql("datetime(observation_datetime)") >= dplyr::sql(sql_condition_1)
      ) %>% 
      dplyr::mutate(
        observation_datetime_int = dplyr::sql(
          "strftime('%s', observation_datetime)/(3600.0)")) %>% 
      dplyr::group_by(patient_id, t) %>%
      dplyr::mutate(
        y_bar = mean(observation_value_numeric, na.rm = TRUE),
        t_bar = 0.0,
        observation_datetime_int = observation_datetime_int - 
          mean(observation_datetime_int, na.rm = T)
      ) %>% 
      dplyr::summarise(
        slope_numerator = sum(
          (observation_value_numeric - y_bar) *
            (observation_datetime_int - t_bar),
          na.rm = TRUE),
        slope_denominator = sum(
          dplyr::sql("square(observation_datetime_int - t_bar)"), 
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
    
  } else if(is(x@conn, "PqConnection")) {
    
    sql_condition_1 <- paste0("(t_start, - interval '", hours, "h')")
    observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
      dplyr::filter(
        dplyr::sql("datetime(observation_datetime) <= datetime(t_start)") &
          dplyr::sql("datetime(observation_datetime)") >= dplyr::sql(sql_condition_1)
      ) %>% 
      dplyr::mutate(
        observation_datetime_int = dplyr::sql(
          "EXTRACT(EPOCH FROM TIMESTAMP WITH TIME ZONE observation_datetime)/3600.0"
          )) %>% 
      dplyr::group_by(patient_id, t) %>%
      dplyr::mutate(
        y_bar = mean(observation_value_numeric, na.rm = TRUE),
        t_bar = 0.0,
        observation_datetime_int = observation_datetime_int - 
          mean(observation_datetime_int, na.rm = T)
      ) %>% 
      dplyr::summarise(
        slope_numerator = sum(
          (observation_value_numeric - y_bar) *
            (observation_datetime_int - t_bar),
          na.rm = TRUE),
        slope_denominator = sum(
          dplyr::sql("power(observation_datetime_int - t_bar), 2)"), 
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
    
  } else {
    .throw_error_method_not_implemented("clinical_feature_mean()",
                                        class(x@conn))
  }
  
  x@therapy_table <- dplyr::left_join(
    x@therapy_table,
    observations_linked,
    by = c("patient_id", "t")
  )
  
  return(x)
}


#' Therapy table feature: temporal trend of clinical observations
#'
#' @description Add a clinical feature to a therapy episode table corresponding
#' to the Ordinary Least Squares (OLS) intercept and slope of clinical
#' observations made until table time \code{t}
#' @param x a \code{\link{TherapyEpisode}} object
#' @param observation_code a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}
#' @param hours the maximum number of hours the observations should date back from
#' \code{t_start}, the starting time of every row in the therapy table
#' @details NOTE: only numeric investigations marked with status \code{"final"}, 
#' \code{"preliminary"}, \code{"corrected"}, or \code{"amended"} will be sourced.
#' @return an object of class \code{\link{TherapyEpisode}}
#' @rdname clinical_feature_ols_trend
#' @export
#' @include objects.R
setGeneric(
  "clinical_feature_ols_trend", 
  function(x, observation_code, hours) standardGeneric("clinical_feature_ols_trend"), 
  signature = "x")


#' @rdname clinical_feature_ols_trend
#' @export
setMethod(
  "clinical_feature_ols_trend",
  c(x = "TherapyEpisode"),
  function(x, observation_code, hours) {
    stopifnot(is.character(observation_code))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)
    
    for (i in seq_len(length(observation_code))) {
      x <- .clinical_feature_ols_trend(x, observation_code[[i]], hours)
    }
    
    return(x)
  }
)