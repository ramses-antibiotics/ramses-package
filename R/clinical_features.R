

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
    
    observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
      dplyr::filter(
        dplyr::sql("observation_datetime <= t_start") &
          dplyr::sql(paste0("observation_datetime >= (t_start, - interval '", hours, "h')"))
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


#' Therapy table feature: most recent observation
#'
#'
#' @description Add a clinical feature to a therapy episode table indicating 
#' the most recent observation before table time \code{t}
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





