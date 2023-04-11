
setOldClass(c("tbl_duckdb_connection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))
setOldClass(c("tbl_PqConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))

# Interface ---------------------------------------------------------------

#' An S4 virtual class for Ramses objects
#'
#' @slot id an identifier or vector of identifiers
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @rdname RamsesObject
#' @export
#' @importClassesFrom duckdb duckdb_connection
setClass(
  "RamsesObject", 
  slots = c(
    id = "vector", 
    conn = "DBIConnection",
    record = "tbl_sql"
  ),
  contains = "VIRTUAL"
)


# Patient -----------------------------------------------------------------


#' An S4 class to represent patients
#'
#' @slot id a patient identifier 
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @param id a patient identifier 
#' @param conn a database connection
#' @param x an object inheriting class \link[Ramses]{RamsesObject}
#' @rdname Patient
#' @export
setClass(
  "Patient", 
  contains = "RamsesObject"
)

#' @rdname Patient
#' @param ... generic signature
#' @export
Patient <- function(...) {
  UseMethod("Patient")
}

#' @rdname Patient
#' @export
Patient.DBIConnection <- function(conn, id) {
  if ( is.null(id) | length(id) != 1 ) {
    stop("`id` must have length 1")
  }
  if (is.na(id)) {
    stop("`id` must not be NA.")
  }
  id_data_type <- .sql_data_type(conn = conn,
                                 table = "patients",
                                 field = "patient_id")
  if (is.numeric(id) & id_data_type == "character") {
    stop("`id` must be character")
  }
  if (is.character(id) & id_data_type !="character") {
    stop(paste("`id` must be", id_data_type))
  }
  
  record <- tbl(conn, "patients") %>% 
    dplyr::filter(.data$patient_id == !!id)
  
  methods::new("Patient", 
               id = id,
               conn = conn,
               record = record)
}

#' @rdname Patient
#' @export
Patient.RamsesObject <- function(x) {
  conn <- x@conn
  record <- collect(x)
  id <- unique(na.omit(record$patient_id)) 
  
  Patient.DBIConnection(conn = conn, id = id)
}

#' @export
#' @noRd  
setGeneric(name = "Patient", def = Patient)


# Encounter ---------------------------------------------------------------


#' An S4 class to represent inpatient encounters
#'
#' @slot id an encounter identifier 
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @slot longitudinal_table a \code{tbl_sql} for the longitudinal encounter table
#' @param id an encounter identifier 
#' @param conn a database connection
#' @param extend_table_start optional integer to specify an earlier start
#' (in hours) in the longitudinal table of the object. For example, a value of 
#' 6 means the longitudinal table will begin 6 hours prior to the admission
#' date. The value must be a positive number. Decimal numbers will be
#' rounded up to the nearest integer. The default is \code{NULL}.
#' @rdname Encounter
#' @export
setClass(
  "Encounter", 
  slot = c(longitudinal_table = "tbl"),
  contains = "RamsesObject"
)

#' @rdname Encounter
#' @export
Encounter <- function(conn, id, extend_table_start = NULL) {
  
  id <- sort(na.omit(unique(id)))
  if ( is.null(id) | length(id) < 1) {
    stop("`id` must contain at least one identifier")
  }
  id_data_type <- .sql_data_type(conn = conn,
                                 table = "inpatient_episodes",
                                 field = "encounter_id")
  if (is.numeric(id) & id_data_type == "character") {
    stop("`id` must be character")
  }
  if (is.character(id) & id_data_type !="character") {
    stop(paste("`id` must be", id_data_type))
  }
  
  extend_table_start <- .validate_extended_table_input(extend_table_start)
  
  record <- dplyr::inner_join(
    tbl(conn, "inpatient_episodes"),
    dplyr::tibble(encounter_id = id),
    by = "encounter_id", copy = TRUE)
  
  longitudinal_table <- .longitudinal_table_create.Encounter(
    conn = conn, 
    id = id,
    extend_table_start = extend_table_start
  )
  
  # TODO
  # longitudinal_table <- .longitudinal_table_parenteral_indicator(
  #   longitudinal_table)
  
  methods::new("Encounter", 
               id = id,
               conn = conn,
               record = record, 
               longitudinal_table = longitudinal_table)
}


#' Create the therapy episode longitudinal table
#'
#' @param conn a database connection
#' @param id a vector of encounter identifiers 
#' @param extend_table_start optional integer to specify an earlier start
#' (in hours) in the longitudinal table of the object. For example, a value of 
#' 6 means the longitudinal table will begin 6 hours prior to the start of
#' therapy. The value must be a positive number. Decimal numbers will be
#' rounded up to the nearest integer. The default is \code{NULL}.
#' @noRd
.longitudinal_table_create.Encounter <- function(conn, id, extend_table_start) {

  .build_tally_table(conn)
  
  longitudinal_table <- dplyr::inner_join(
    tbl(conn, "inpatient_episodes"), 
    dplyr::tibble(encounter_id = sort(unique(id))), 
    by = "encounter_id", copy = TRUE
  ) %>% 
    dplyr::distinct(.data$patient_id, 
                    .data$encounter_id, 
                    .data$admission_date, 
                    .data$discharge_date)
  
  if(is(conn, "PqConnection") | is(conn, "duckdb_connection")) {
    tbl(conn, "ramses_tally") %>%
      dplyr::mutate(t = .data$t - as.integer(extend_table_start)) %>% 
      dplyr::cross_join(longitudinal_table) %>%
      dplyr::mutate(t_start = dplyr::sql("admission_date + interval '1h' * t ")) %>% 
      dplyr::filter(.data$t_start < .data$discharge_date) %>% 
      dplyr::mutate(t_end = dplyr::sql("admission_date + interval '1h' * (t + 1)")) %>% 
      dplyr::group_by(.data$patient_id, .data$encounter_id) %>% 
      dplyr::mutate(t_end = dplyr::if_else(
        .data$t == max(.data$t, na.rm = TRUE),
        .data$discharge_date,
        .data$t_end
      )) %>% 
      dplyr::ungroup()
  } else {
    .throw_error_method_not_implemented(".longitudinal_table_create.Encounter()",
                                        class(conn))
  }
}

# MedicationRequest -------------------------------------------------------


#' An S4 class to represent a drug-dose medication request
#'
#' @description This class represent a clinician's request for a single 
#' drug-dose order, whether it is to be used as monotherapy or combination therapy.
#' @slot id a prescription identifier
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @param id a prescription identifier
#' @param conn a database connection
#' @rdname MedicationRequest
#' @export
setClass(
  "MedicationRequest",
  contains = "RamsesObject"
)

#' @rdname MedicationRequest
#' @export
MedicationRequest <- function(conn, id) {
  if ( is.null(id) | length(id) == 0 ) {
    stop("`id` must have length 1.")
    id <- id[1]
  }
  if ( length(id) > 1 ) {
    warning("`id` must have length 1. The first value will be used")
    id <- id[1]
  }
  if (is.na(id)) {
    stop("`id` must not be NA.")
  }
  id_data_type <- .sql_data_type(conn = conn,
                                 table = "drug_prescriptions",
                                 field = "prescription_id")
  if (is.numeric(id) & id_data_type == "character") {
    stop("`id` must be character")
  }
  if (is.character(id) & id_data_type !="character") {
    stop(paste("`id` must be", id_data_type))
  }
  
  record <- dplyr::filter(tbl(conn, "drug_prescriptions"),
                          .data$prescription_id == !!id)
  methods::new("MedicationRequest", 
               id = id,
               conn = conn,
               record = record)
}



# TherapyEpisode ----------------------------------------------------------


#' An S4 class to represent episodes of antimicrobial therapy
#'
#' @slot id a therapy episode identifier
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @slot longitudinal_table a \code{tbl_sql} for the longitudinal therapy table
#' @param id a vector of one or several therapy episode identifiers (from
#' database field \code{drug_therapy_episodes.therapy_id} as generated by 
#' \code{\link{load_medications}()} or
#' \code{\link{create_therapy_episodes}()})
#' @param conn a database connection
#' @param x an object of class \code{MedicationRequest}
#' @param extend_table_start optional integer to specify an earlier start
#' (in hours) in the longitudinal table of the object. For example, a value of 
#' 6 means the longitudinal table will begin 6 hours prior to the start of 
#' antimicrobial therapy. The value must be a positive number. Decimal numbers 
#' will be rounded up to the nearest integer. The default is \code{NULL}.
#' @rdname TherapyEpisode
#' @export
setClass(
  "TherapyEpisode",
  slot = c(longitudinal_table = "tbl"),
  contains = "RamsesObject"
)

#' @rdname TherapyEpisode
#' @param ... generic signature
#' @export
TherapyEpisode <- function(...) {
  UseMethod("TherapyEpisode")
}

#' @rdname TherapyEpisode
#' @export
TherapyEpisode.DBIConnection <- function(conn, id, extend_table_start = NULL) {

  id <- sort(na.omit(unique(id)))
  if ( is.null(id) | length(id) < 1) {
    stop("`id` must contain at least one identifier")
  }
  id_data_type <- .sql_data_type(conn = conn,
                                 table = "drug_therapy_episodes",
                                 field = "therapy_id")
  if (is.numeric(id) & id_data_type == "character") {
    stop("`id` must be character")
  }
  if (is.character(id) & id_data_type !="character") {
    stop(paste("`id` must be", id_data_type))
  }
  
  extend_table_start <- .validate_extended_table_input(extend_table_start)
  
  record <- dplyr::inner_join(
    tbl(conn, "drug_therapy_episodes"),
    dplyr::tibble(therapy_id = id),
    by = "therapy_id", copy = TRUE)
  longitudinal_table <- .longitudinal_table_create.TherapyEpisode(
    conn = conn, 
    id = id,
    extend_table_start)
  longitudinal_table <- .longitudinal_table_parenteral_indicator(longitudinal_table)
  methods::new("TherapyEpisode", 
               id = id,
               conn = conn,
               record = record, 
               longitudinal_table = longitudinal_table)
}

#' @rdname TherapyEpisode
#' @export
TherapyEpisode.MedicationRequest <- function(x, extend_table_start = NULL) {
  conn <- x@conn
  record <- collect(x)
  id <- unique(na.omit(record$therapy_id)) 
  
  TherapyEpisode.DBIConnection(conn = conn, id = id, extend_table_start = extend_table_start)
}

#' @export
#' @noRd  
setGeneric(name = "TherapyEpisode", def = TherapyEpisode)


#' Create the therapy episode longitudinal table
#'
#' @param conn a database connection
#' @param id a vector of therapy episode character identifiers (by design, Ramses creates
#' this as the identifier of the first prescription ordered in an episode)
#' @param extend_table_start optional integer to specify an earlier start
#' (in hours) in the longitudinal table of the object. For example, a value of 
#' 6 means the longitudinal table will begin 6 hours prior to the admission
#' date. The value must be a positive number. Decimal numbers will be
#' rounded up to the nearest integer. The default is \code{NULL}.
#' @noRd
.longitudinal_table_create.TherapyEpisode <- function(conn, id, extend_table_start) {

  .build_tally_table(conn)
  
  longitudinal_table <- dplyr::inner_join(
    tbl(conn, "drug_therapy_episodes"), 
    dplyr::tibble(therapy_id = sort(unique(id))), 
    by = "therapy_id", copy = TRUE
  ) %>% 
    dplyr::select("patient_id", 
                  "therapy_id", 
                  "therapy_start", 
                  "therapy_end")

  if(is(conn, "PqConnection") | is(conn, "duckdb_connection")) {
    tbl(conn, "ramses_tally") %>%
      dplyr::mutate(t = .data$t - as.integer(extend_table_start)) %>% 
      dplyr::cross_join(longitudinal_table) %>%
      dplyr::mutate(t_start = dplyr::sql("therapy_start + interval '1h' * t "))%>% 
      dplyr::filter(.data$t_start < .data$therapy_end) %>% 
      dplyr::mutate(t_end = dplyr::sql("therapy_start + interval '1h' * (t + 1)")) %>% 
      dplyr::group_by(.data$patient_id, .data$therapy_id) %>% 
      dplyr::mutate(t_end = dplyr::if_else(
        t == max(.data$t, na.rm = TRUE),
        .data$therapy_end,
        .data$t_end
      )) %>% 
      dplyr::ungroup()
  } else {
    .throw_error_method_not_implemented(".longitudinal_table_create.TherapyEpisode()",
                                        class(conn))
  }
}



#' Create parenteral indicator
#'
#' @param longitudinal_table `tbl_sql` for the spine of the longitudinal_table
#' @return a longitudinal_table with an IV column (1 = 1+ drugs are parenteral,
#' 0 = all drugs administered via oral route)
#' @noRd
.longitudinal_table_parenteral_indicator <- function(longitudinal_table) {
  
  medication_requests <- dplyr::inner_join(
    tbl(longitudinal_table$src$con, "drug_prescriptions"),
    dplyr::distinct(longitudinal_table, .data$therapy_id),
    by = "therapy_id", copy = TRUE
  )
  
  if (is(longitudinal_table$src$con, "PqConnection") | is(longitudinal_table$src$con, "duckdb_connection") ) {
    longitudinal_table_meds_join <- dplyr::left_join(
      longitudinal_table, 
      medication_requests, 
      by = c("patient_id", "therapy_id")) %>% 
      dplyr::filter(
        dplyr::between(.data$t_start, .data$prescription_start, .data$prescription_end) |
          dplyr::between(.data$prescription_start, .data$t_start, .data$t_end) | 
          dplyr::between(.data$prescription_end, .data$t_start, .data$t_end)
      ) %>% 
      dplyr::group_by(.data$patient_id, .data$therapy_id, .data$t) %>%
      dplyr::summarise(parenteral = dplyr::if_else(
        mean(
          dplyr::if_else(.data$ATC_route == "P", 1.0, 0.0), 
          na.rm = T) == 0.0,
        0L,
        1L
      ))
  } else {
    .throw_error_method_not_implemented(".longitudinal_table_parenteral_indicator()",
                                        class(longitudinal_table$src$con))
  }
  
  longitudinal_table_parenteral <- dplyr::left_join(
    longitudinal_table, 
    longitudinal_table_meds_join, 
    by = c("patient_id", "therapy_id", "t")
  )
  
  return(longitudinal_table_parenteral)
}

#' Get parenteral antimicrobial therapy sequences
#'
#' @description Timely switch to oral therapy is a widely recommended
#' antimicrobial stewardship behaviour. \code{parenteral_changes()}
#' extracts 'therapy sequences', which are defined as either:
#' \itemize{
#'    \item a period of parenteral antimicrobial therapy subsequently converted
#'    into oral therapy 
#'    \item a period of parenteral antimicrobial therapy never converted into 
#'    oral therapy
#' }
#' @param therapy_episode a \code{\link{TherapyEpisode}} object
#' @param tolerance_hours integer for the maximum number of hours during which 
#' an absence of prescription or the administration of some oral drugs 
#' will be ignored. The default is 12.
#' @details Antimicrobial drugs may be administered via oral or parenteral 
#' (usually intravenous) route. Oral administration is preferred when patients
#' can tolerate them and infections are not deep seated. 
#' 
#' This function analyses the \code{parenteral} field in a
#' longitudinal table. By default, it returns positions of sequences of at least 
#' twelve "1" (= parenteral drug administration) followed by a 
#' "0" (= oral administration) or a terminating sequence composed exclusively of "0".
#' 
#' Short periods without therapy lasting up to twelve hours (e.g. between two prescriptions)
#' are ignored by default (see \code{tolerance_hours} parameter).
#' 
#' Short periods of oral administration lasting up to twelve hours are also ignored by default.
#' 
#' This ensures that very short oral prescriptions (including when parenteral 
#' therapy continues uninterrupted) do not distort the analysis of therapy 
#' changes.
#' @return a named list containing, for every therapy episode, a nested list of 
#' vectors describing therapy sequences. Each vector consists
#' of three integers coding for the time \code{t}:
#' \enumerate{ 
#'    \item when the sequence is initiated (parenteral administration begins)
#'    \item when the sequence ends
#'    \item \emph{if the therapy is converted to oral administration:} time of conversion,
#'    (otherwise \code{NA_integer_})
#' }
#' 
#' The integers are in direct correspondence with field \code{t} in the 
#' therapy episode table (\code{\link{longitudinal_table}()}).
#' @export
#' @examples 
#' \dontrun{
#' library(dplyr)
#' ramses_db <- create_mock_database("Ramses_mock_database_example.duckdb")
#' example_therapy <- TherapyEpisode(ramses_db, "2a2b6da3b67f6f495f4cedafb029f631")
#' 
#' # Obtain the parenteral to oral sequence indexes
#' therapy_sequence <- parenteral_changes(example_therapy)
#' therapy_sequence
#' 
#' # Look for the section of the longitudinal_table where 0 <= t <= 267
#' filter(longitudinal_table(example_therapy, collect = TRUE),
#'        between(t,
#'                therapy_sequence[["2a2b6da3b67f6f495f4cedafb029f631"]][[1]][1],
#'                therapy_sequence[["2a2b6da3b67f6f495f4cedafb029f631"]][[1]][2])) %>% head()
#' # Look for the section of the longitudinal_table near conversion (t = 220)
#' filter(longitudinal_table(example_therapy, collect = TRUE),
#'        between(t, 218, 223))
#' }
parenteral_changes <- function(therapy_episode, tolerance_hours = 12L) {
  if (!is(therapy_episode, "TherapyEpisode")) {
    .throw_error_method_not_implemented("parenteral_changes", class(therapy_episode))
  }
  stopifnot(is.numeric(tolerance_hours) | length(tolerance_hours) != 1)
  tolerance_hours <- as.integer(tolerance_hours)
  TT <- longitudinal_table(therapy_episode, collect = T)
  TT <- split(x = TT, f = TT$therapy_id)
  lapply(TT, function(x, input_tolerance_hours){
    .parenteral_vector_process(x[["parenteral"]], 
                               input_tolerance_hours)
    
  }, input_tolerance_hours = tolerance_hours)
}

.parenteral_vector_process <- function(x, tolerance_hours) {
  
  iosequence <- gsub("NA", "#", paste(x, collapse = ""))
  match_indices <- list()
  i = 1
  position = 0
  
  while ( nchar(iosequence) > 0 ) {
    
    # eg, if tolerance set to 9 hours, regex is:
    # "1{8,}((10{1,8}1)|1|#{1,9})*(1{9,}|1$|#{9,}){1}"
    # "1{8,}((10{1,9}1)|1|(1#{1,9}1))*(0{10,}|1$|#{10,}){1}"
    match_index <- regexpr(
      paste0(
        # START OF SEQUENCE
        "1{5,}", 
        # IN-SEQUENCE BLIPS
        "(",
        "(10{1,", tolerance_hours, "})|",
        "1|",
        "(1#{1,", tolerance_hours, "})",
        ")*",
        # END OF SEQUENCE
        "(0{", tolerance_hours, ",}|1|$){1}"
      ), 
      iosequence
    )
    
    conversion_index <- regexpr(
      paste0(
        # END OF SEQUENCE with CONVERSION
        "0{", tolerance_hours, ",}$"
      ), 
      substr(iosequence,
             as.integer(match_index), 
             as.integer(match_index) + attr(match_index, "match.length") -1)
    )
    # If no conversion, return NA
    if (conversion_index == -1) {
      conversion_index <- NA_integer_
    }
    
    if (match_index == -1) {
      break
    }
    
    match_indices[[i]] <- c(as.integer(match_index) + position - 1, 
                            as.integer(match_index) + attr(match_index, "match.length") + position - 2,
                            as.integer(conversion_index) + as.integer(match_index) + position - 2)
    # reduce sequence to what is left beyond the match
    position <- as.integer(match_index) + attr(match_index, "match.length") + position - 1
    iosequence <- substr(iosequence, 
                         as.integer(match_index) + attr(match_index, "match.length"), 
                         nchar(iosequence))
    rm(match_index, conversion_index)
    i <- i + 1
    next
  }
  
  match_indices
}


# longitudinal_table -------------------------------------------------------

#' Verify that all therapy episodes/encounters are present in record
#'
#' @description To verify that a `tbl_objects` table contains 
#' all therapy episodes referenced in the `object@id` slot
#' @param x a `TherapyEpisode` or `Encounter` object
#' @param tbl_object a `tbl_sql` or `tbl_df` containing a `therapy_id` column
#' @param silent if `TRUE`, will not throw a warning if not all therapy 
#' episodes are present
#' @return a boolean (and throws a warning)
#' @noRd
.longitudinal_table_completeness_check <- function(x, tbl_object, silent = FALSE) {
  if (is(x, "Encounter")) {
    object_id_field <- "encounter_id"
    warning_object_class <- "encounters"
  } else if (is(x, "TherapyEpisode")) {
    object_id_field <- "therapy_id"
    warning_object_class <- "therapy episodes"
  } else {
    .throw_error_method_not_implemented(
      function_name = ".longitudinal_table_completeness_check()",
      class_name = class(x)
    )
  }
  
  remote_ids <- dplyr::distinct(tbl_object, .data[[object_id_field]]) %>% 
    dplyr::collect()
  missing <- !x@id %in% remote_ids[[object_id_field]]
  
  if (any(missing)) {
    if (!silent) {
      warning("Some ", warning_object_class, " were not found:\n",
              paste(utils::head(x@id[missing]), collapse = ", "),
              ifelse(sum(!missing) > 5, "...", ""), call. = FALSE)
    }
  }
  
  all(!missing)
}


#' Get the longitudinal_table
#'
#' @param x an object of class \code{Encounter}, \code{TherapyEpisode}, 
#' or \code{MedicationRequest}
#' @param collect if \code{TRUE}, collect the remote \code{tbl_sql} and return a local 
#' \code{tbl_df}. The default is \code{FALSE}, and simply returns the remote \code{tbl_sql}
#' @return an object of class \code{tbl}
#' @rdname longitudinal_table
#' @export
setGeneric("longitudinal_table", function(x, collect = FALSE) standardGeneric("longitudinal_table"))

#' @rdname longitudinal_table
#' @export
setMethod("longitudinal_table", "TherapyEpisode", function(x, collect = FALSE) {
  stopifnot(is.logical(collect))
  .longitudinal_table_completeness_check(x, x@longitudinal_table)
  if( collect ) {
    dplyr::collect(x@longitudinal_table) %>% 
      dplyr::arrange(.data$therapy_id, .data$t)
  } else {
    x@longitudinal_table
  }
})

#' @rdname longitudinal_table
#' @export
setMethod("longitudinal_table", "MedicationRequest", function(x, collect = FALSE) {
  stopifnot(is.logical(collect))
  x <- TherapyEpisode(x)
  .longitudinal_table_completeness_check(x, x@longitudinal_table)
  if( collect ) {
    dplyr::collect(x@longitudinal_table) %>% 
      dplyr::arrange(.data$therapy_id, .data$t)
  } else {
    x@longitudinal_table
  }
})


#' @rdname longitudinal_table
#' @export
setMethod("longitudinal_table", "Encounter", function(x, collect = FALSE) {
  stopifnot(is.logical(collect))
  .longitudinal_table_completeness_check(x, x@longitudinal_table)
  if( collect ) {
    dplyr::collect(x@longitudinal_table) %>% 
      dplyr::arrange(.data$encounter_id, .data$t)
  } else {
    x@longitudinal_table
  }
})


#' Return a single positive integer
#'
#' @param x input
#' @noRd
.validate_extended_table_input <- function(x) {
  if ( is.null(x) || all(is.na(x)) ) {
    x <- 0
  }
  if ( length(x) > 1 || !is.numeric(x) ) {
    stop("`", substitute(x), "` must be a numeric or integer of length 1.")
  }
  if ( x < 0 ) {
    stop("`", substitute(x), "` must be >= 0")
  } 
  x <- ceiling(x)
  
  x
}

# show methods ------------------------------------------------------------


#' Show a Ramses object
#'
#' @description Print a summary of a Ramses object.
#' @param object an object of class RamsesObject
#'
#' @return show returns an invisible \code{NULL}.
#' @rdname show
#' @seealso \code{\link[methods]{show}()}
#' @importFrom methods show
#' @export
setMethod("show", "RamsesObject", function(object) {
  cat(class(object), as.character(object@id), "\n")
  cat("\nDatabase connection:\n")
  print(object@conn)
  invisible()
})

#' @rdname show
#' @export
setMethod("show", "Encounter", function(object) {
  if( length(object@id) == 1 ) {
    cat("Encounter", paste(as.character(object@id), collapse = ", "), "\n")
  } else if( length(object@id) <= 3 ) {
    cat("Encounters", paste(as.character(object@id), collapse = ", "), "\n")
  } else if( length(object@id) > 3 ) {
    cat("Encounters", paste(as.character(object@id)[1:3], collapse = ", "), "...\n")
  }
  record <- dplyr::collect(object@record)
  
  if( nrow(record) == 0 ) {
    cat("Record is not available.\n")
    cat("Please check object id is valid\n")
  } else if( length(object@id) == 1 ) {
    cat("Patient:  ", unique(record$patient_id), "\n")
    cat("Start:    ", format(unique(record$admission_date), format = "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("End:      ", format(unique(record$discharge_date), format = "%Y-%m-%d %H:%M:%S %Z"), "\n\n")
  } else if( length(object@id) > 1 ) {
    cat("[total of", length(object@id), "encounters]\n")
    record <- dplyr::arrange(record, .data$encounter_id, .data$episode_number)
    if (dplyr::n_distinct(record$patient_id) > 3) {
      cat("Patients:  ", paste(as.character(unique(record$patient_id)[1:3]), collapse = ", "), ", ...\n")
    } else {
      cat("Patient(s):  ", paste(as.character(unique(record$patient_id)), collapse = ", "), "\n")
    }
  }
  
  cat("\nDatabase connection:\n")
  show(object@conn)
  invisible()
})

#' @rdname show
#' @export
setMethod("show", "TherapyEpisode", function(object) {
   if( length(object@id) <= 3 ) {
    cat(class(object), paste(as.character(object@id), collapse = ", "), "\n")
  } else if( length(object@id) > 3 ) {
    cat(class(object), paste(as.character(object@id)[1:3], collapse = ", "), "...\n")
  }
  record <- dplyr::collect(object@record)

  if( nrow(record) == 0 ) {
    cat("Record is not available.\n")
    cat("Please check object id is valid\n")
  } else if( length(object@id) == 1 ) {
    prescriptions <- tbl(object@conn, "drug_prescriptions") %>%
      dplyr::filter(.data$patient_id == !!record$patient_id &
                      .data$therapy_id == !!object@id) %>%
      dplyr::arrange(.data$therapy_rank) %>%
      dplyr::select("prescription_text") %>%
      dplyr::collect()
    cat("Patient:  ", record$patient_id, "\n")
    cat("Start:    ", format(record$therapy_start, format = "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("End:      ", format(record$therapy_end, format = "%Y-%m-%d %H:%M:%S %Z"), "\n\n")
    cat("Medications:\n ")
    if(nrow(prescriptions) > 5) {
      cat(paste0(" > ", prescriptions$prescription_text[1:4], "\n"))
      cat(paste0("  ... (", nrow(prescriptions) - 4, " additional medication requests)\n"))
    } else {
      cat(paste0(" > ", prescriptions$prescription_text, "\n"))
    }
  } else if( length(object@id) > 1 ) {
    cat("[total of", nrow(record), "therapy episodes]\n")
    record <- dplyr::arrange(record, .data$therapy_id)
    record <- record[order(object@id), ]
    if (length(unique(record$patient_id)) > 3) {
      cat("Patients:  ", paste(as.character(unique(record$patient_id)[1:3]), collapse = ", "), ", ...\n")
    } else {
      cat("Patient(s):  ", paste(as.character(unique(record$patient_id)), collapse = ", "), "\n")
    }
  }

  cat("\nDatabase connection:\n")
  show(object@conn)
  invisible()
})

#' @rdname show
#' @export
setMethod("show", "MedicationRequest", function(object) {
  cat(class(object), as.character(object@id), "\n")
  record <- dplyr::collect(object@record)
  if( nrow(record) == 0 ) {
    cat("Record is not available.\n")
    cat("Please check object id is valid\n")
  } else {
    cat(record$prescription_text, "\n")
    cat("Patient:    ", as.character(record$patient_id), "\n")
    cat("Start:       ", format(record$prescription_start, format = "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("End:         ", format(record$prescription_end, format = "%Y-%m-%d %H:%M:%S %Z"), "\n")
    if( !is.na(record$combination_id) ) {
      cat("Combination: ", as.character(record$combination_id), "\n")
    }
    cat("Therapy:     ", as.character(record$therapy_id), "\n")
  }
  cat("\nDatabase connection:\n")
  print(object@conn)
  invisible()
})


# compute methods ---------------------------------------------------------

.compute_longitudinal_objects <- function(x) {
  .longitudinal_table_completeness_check(x, x@record)
  x@record <- dplyr::compute(x@record)
  x@longitudinal_table <- dplyr::compute(x@longitudinal_table)
  
  .create_sql_primary_key(
    conn = x@conn,
    table = dbplyr::remote_name(x@longitudinal_table),
    field = ifelse(
      methods::is(x, "Encounter"),
      "t, patient_id, encounter_id",
      "t, patient_id, therapy_id"
    ),
    override_index_name = paste0("idx_pk_", dbplyr::remote_name(x@longitudinal_table))
  )
  .create_sql_index(
    conn = x@conn,
    table = dbplyr::remote_name(x@longitudinal_table),
    fields = "patient_id, t_start",
    override_index_name = paste0("idx_pt_time_", dbplyr::remote_name(x@longitudinal_table))
  )
  
  x
}

#' Compute the database records of a Ramses object
#'
#' @description Compute the \code{tbl_sql} records of a Ramses object in the 
#' backend database (see \code{dplyr::\link[dplyr]{compute}()}). This function 
#' creates a database index of \code{Encounter} and \code{TherapyEpisode} 
#' longitudinal tables, which can speed up operations such as those performed
#' by \code{clinical_feature_x()} functions.
#' @param x an object of class \code{RamsesObject}
#' @param ... arguments passed on to methods (compatibility with \code{dplyr}'s
#' S3 generic)
#' 
#' @return an object of class \code{RamsesObject}
#' @seealso \code{\link[dplyr]{compute}()}
#' @rdname compute
#' @importFrom dplyr compute
#' @export
setGeneric("compute", function(x, ...) standardGeneric("compute"))

#' @rdname compute
#' @export
setMethod("compute", "TherapyEpisode", function(x) {
  .compute_longitudinal_objects(x)
})

#' @rdname compute
#' @export
setMethod("compute", "Encounter", function(x) {
  .compute_longitudinal_objects(x)
})

#' @rdname compute
#' @export
setMethod("compute", "RamsesObject", function(x) {
  x@record <- dplyr::compute(x@record)
  x
})

# collect methods ---------------------------------------------------------


#' Retrieve the database record of a Ramses object
#' 
#' @description Retrieve a data frame of the record belonging to a Ramses 
#' object instance.
#' @param x an object of class \code{RamsesObject}
#' @param ... arguments passed on to methods (compatibility with \code{dplyr}'s
#' S3 generic)
#' @return a data frame of class \code{tbl_df}
#' @rdname collect
#' @importFrom dplyr collect
#' @seealso \code{\link[dplyr]{collect}()}, \code{\link[Ramses]{longitudinal_table}()}
#' @export
setGeneric("collect", function(x, ...) standardGeneric("collect"))

#' @rdname collect
#' @export
setMethod("collect", "TherapyEpisode", function(x) {
  .longitudinal_table_completeness_check(x, x@record)
  dplyr::collect(x@record)
})

#' @rdname collect
#' @export
setMethod("collect", "Encounter", function(x) {
  .longitudinal_table_completeness_check(x, x@record)
  dplyr::collect(x@record)
})

#' @rdname collect
#' @export
setMethod("collect", "RamsesObject", function(x) {
  dplyr::collect(x@record)
})
