


#' Therapy table feature: most recent observation
#'
#'
#' @description Add a clinical feature to a therapy episode table indicating 
#' the most recent observation before table time \code{t}
#' @param x a \link{TherapyEpisode} object
#' @param investigation_codes a character vector of clinical investigation codes
#' matching the \code{observation_code} field in the \code{inpatient_investigation}
#' table (see \code{\link{validate_investigations}()}
#' @param hours the maximum number of hours the observation should date back from
#' \code{t_start}, the starting time of every row in the therapy table
#' @details NOTE: only numeric investigations marked with status \code{"final"}, 
#' \code{"preliminary"}, \code{"corrected"}, or \code{"amended"} will be sourced.
#' @return 
#' @rdname clinical_feature_last
#' @export
#' @include objects.R
setGeneric(
  "clinical_feature_last", 
  function(x, investigation_codes, hours) standardGeneric("clinical_feature_last"), 
  signature = "x")


#' @rdname clinical_feature_last
#' @export
setMethod(
  "clinical_feature_last",
  c(x = "TherapyEpisode"),
  function(x, investigation_codes, hours) {
    stopifnot(is.character(investigation_codes))
    stopifnot(is.numeric(hours) & length(hours) == 1 & hours >= 0)

    TT <- .therapy_table_create(x@conn, x@id)
    therapy_record <- collect(x)

    all_observations <- tbl(x@conn, "inpatient_investigations") %>%
      dplyr::filter(patient_id %in% !!therapy_record$patient_id &
                      observation_code %in% !!investigation_codes &
                      status %in% c("final", "preliminary", "corrected", "amended") &
                      !is.na(observation_value_numeric) &
                      dplyr::between(observation_datetime,
                                     !!(therapy_record$therapy_start - 3600 * hours),
                                     !!therapy_record$therapy_end)) %>%
      dplyr::select(patient_id, observation_datetime,
                    observation_code, observation_value_numeric)

    if(is(x@conn, "SQLiteConnection")) {
      
      sql_condition1 <- paste0("datetime(t_start, -", hours," || ' hours')")
      observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
        dplyr::filter(
          datetime(observation_datetime) <= datetime(t_start) &
             datetime(observation_datetime) >= dplyr::sql(sql_condition1)
          ) %>% 
        dplyr::group_by(patient_id, t) %>% 
        dplyr::mutate(last_value = dplyr::row_number(dplyr::desc(observation_datetime))) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(last_value == 1) %>% 
        dplyr::select(patient_id, t, observation_value_numeric)
      
    } else if(is(x@conn, "PqConnection")) {
      
      observations_linked <- dplyr::inner_join(TT, all_observations, by = "patient_id") %>% 
        dplyr::filter(
          dplyr::sql("observation_datetime <= t_start") &
            dplyr::sql(paste0("observation_datetime >= (t_start, - interval '", hours, "h')"))
        ) %>% 
        dplyr::group_by(patient_id, t) %>% 
        dplyr::mutate(last_value = dplyr::row_number(dplyr::desc(observation_datetime))) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(last_value == 1) %>% 
        dplyr::select(patient_id, t, observation_value_numeric)
      
    } else {
      .throw_error_method_not_implemented("clinical_feature_last()",
                                          class(conn))
    }
    
    x@therapy_table <- dplyr::left_join(
      x@therapy_table,
      observations_linked,
      by = c("patient_id", "t")
    )
    
    return(x)
  }
)



