


# Interface ---------------------------------------------------------------

setOldClass(c("tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))
setOldClass(c("tbl_PqConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))
setGeneric("collect", function(x) standardGeneric("collect"))

#' An S4 virtual class for Ramses objects
#'
#' @slot id a character identifier 
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @rdname RamsesObject
#' @importFrom dplyr collect
#' @importFrom dplyr compute
#' @export
#' @importClassesFrom RSQLite SQLiteConnection
setClass(
  "RamsesObject", 
  slots = c(
    id = "character", 
    conn = "DBIConnection",
    record = "tbl_sql"
  ),
  contains = "VIRTUAL"
)

setValidity("RamsesObject", function(object) {
  if ( length(object@id) != 1 ) {
    "`id` must be a string of length 1"
  } else if(object@id == "") {
    '`id` must not equal ""'
  } else {
    TRUE
  }
})

setMethod("collect", "RamsesObject", function(x) {
  collect_ramses_tbl(x@record)
})

setMethod("compute", "RamsesObject", function(x) {
  x@record <- dplyr::compute(x@record)
  x
})



# Patient -----------------------------------------------------------------


#' An S4 class to represent patients
#'
#' @slot id a patient identifier 
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @param id a character identifier 
#' @param conn a database connection
#' @rdname Patient
#' @export
setClass(
  "Patient", 
  contains = "RamsesObject"
)

#' @rdname Patient
#' @export
Patient <- function(conn, id) {
  id <- as.character(id)[1]
  record <- tbl(conn, "patients") %>% 
    dplyr::filter(patient_id == !!id)
  
  new("Patient", 
      id = id,
      conn = conn,
      record = record)
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
  id <- as.character(id)[1]
  record <- dplyr::filter(tbl(conn, "drug_prescriptions"),
                          prescription_id == !!id)
  new("MedicationRequest", 
      id = id,
      conn = conn,
      record = record)
}


# TherapyEpisode ----------------------------------------------------------


#' An S4 class to represent therapy episodes
#'
#' @slot id a character therapy episode identifier
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @slot therapy_table a \code{tbl_sql} for the longitudinal therapy table
#' @param id a character therapy episode identifier
#' @param conn a database connection
#' @param object an object of class \code{MedicationRequest} or 
#' \code{Prescription}
#' @rdname TherapyEpisode
#' @export
setClass(
  "TherapyEpisode",
  slot = c(therapy_table = "tbl"),
  contains = "RamsesObject"
)

#' @rdname TherapyEpisode
#' @param ... generic signature
#' @export
TherapyEpisode <- function(...){
  UseMethod("TherapyEpisode")
}

#' @rdname TherapyEpisode
#' @export
TherapyEpisode.DBIConnection <- function(conn, id) {
  id <- as.character(id)[1]
  record <- tbl(conn, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == !!id)
  therapy_table <- .therapy_table_create(conn = conn, id = id)
  therapy_table <- .therapy_table_parenteral_indicator(therapy_table, therapy_id = id)
  new("TherapyEpisode", 
      id = id,
      conn = conn,
      record = record, 
      therapy_table = therapy_table)
}

#' @rdname TherapyEpisode
#' @export
TherapyEpisode.RamsesObject <- function(object) {
  if( !is(object, "MedicationRequest") &
      !is(object, "Prescription") ) {
    stop("`object` must be of class `MedicationRequest` or `Prescription`")
  }
  conn <- object@conn
  record <- collect(object)
  id <- unique(na.omit(record$therapy_id)) 
  
  TherapyEpisode.DBIConnection(conn = conn, id = id)
}

#' @export
#' @noRd  
setGeneric(name = "TherapyEpisode", def = TherapyEpisode)




#' Create the therapy episode longitudinal table
#'
#' @param conn a database connection
#' @param id a therapy episode character identifier (by design, Ramses creates
#' this as the identifier of the first prescription ordered in an episode)  
#' @noRd
.therapy_table_create <- function(conn, id) {

  if( !DBI::dbExistsTable(conn, "ramses_tally") ){
    .build_tally_table(conn)
  }

  therapy_table <- tbl(conn, "drug_therapy_episodes") %>%
    dplyr::filter(therapy_id == !!id) %>%
    dplyr::select(patient_id, therapy_id, therapy_start, therapy_end)

  if(is(conn, "SQLiteConnection")) {
    tbl(conn, "ramses_tally") %>%
      dplyr::full_join(therapy_table, by = character()) %>%
      dplyr::mutate(t_start = dplyr::sql("datetime(therapy_start, (t || ' hours')) || '+00:00'")) %>% 
      dplyr::filter( dplyr::sql("DATETIME(t_start) < DATETIME(therapy_end)")) %>% 
      dplyr::mutate(t_end = dplyr::sql("datetime(therapy_start, ((t + 1) || ' hours')) || '+00:00'")) %>% 
      dplyr::mutate(t_end = dplyr::if_else(
        t == max(t, na.rm = TRUE),
        therapy_end,
        t_end
      ))
  } else if(is(conn, "PqConnection")) {
    tbl(conn, "ramses_tally") %>%
      dplyr::full_join(therapy_table, by = character()) %>%
      dplyr::mutate(t_start = dplyr::sql("therapy_start + interval '1h' * t "))%>% 
      dplyr::filter(t_start < therapy_end) %>% 
      dplyr::mutate(t_end = dplyr::sql("therapy_start + interval '1h' * (t + 1)")) %>% 
      dplyr::mutate(t_end = dplyr::if_else(
        t == max(t, na.rm = TRUE),
        therapy_end,
        t_end
      ))
  } else {
    .throw_error_method_not_implemented(".create_therapy_table()",
                                        class(conn))
  }
}



#' Create parenteral indicator
#'
#' @param therapy_table `tbl_sql` for the spine of the therapy table
#' @return a therapy table with an IV column (1 = 1+ drugs are parenteral,
#' 0 = all drugs administered via oral route)
#' @noRd
.therapy_table_parenteral_indicator <- function(therapy_table, therapy_id){
  
  medication_requests <- tbl(therapy_table$src$con,
                             "drug_prescriptions") %>% 
    dplyr::filter(therapy_id == !!therapy_id)
  
  if (is(therapy_table$src$con, "SQLiteConnection")) {
    therapy_table_meds_join <- dplyr::left_join(
      therapy_table, 
      medication_requests, 
      by = c("patient_id", "therapy_id")) %>% 
      dplyr::filter(
        dplyr::sql(
          "DATETIME(t_start) BETWEEN DATETIME(prescription_start) AND DATETIME(prescription_end) OR
          DATETIME(prescription_start) BETWEEN DATETIME(t_start) AND DATETIME(t_end) OR
          DATETIME(prescription_end) BETWEEN DATETIME(t_start) AND DATETIME(t_end)"
          )
      ) %>% 
      dplyr::group_by(patient_id, therapy_id, t) %>%
      dplyr::summarise(parenteral = dplyr::if_else(
        mean(
          dplyr::if_else(ATC_route == "P", 1.0, 0.0), 
          na.rm = T) == 0.0,
        0L,
        1L
      ))
  } else if (is(therapy_table$src$con, "PqConnection")) {
    therapy_table_meds_join <- dplyr::left_join(
      therapy_table, 
      medication_requests, 
      by = c("patient_id", "therapy_id")) %>% 
      dplyr::filter(
        dplyr::between(t_start, prescription_start, prescription_end) |
          dplyr::between(prescription_start, t_start, t_end) | 
          dplyr::between(prescription_end, t_start, t_end)
      ) %>% 
      dplyr::group_by(patient_id, therapy_id, t) %>%
      dplyr::summarise(parenteral = dplyr::if_else(
        mean(
          dplyr::if_else(ATC_route == "P", 1.0, 0.0), 
          na.rm = T) == 0.0,
        0L,
        1L
      ))
  } else {
    .throw_error_method_not_implemented(".therapy_table_parenteral_indicator()",
                                        class(therapy_table$src$con))
  }
  
  therapy_table_parenteral <- dplyr::left_join(
    therapy_table, 
    therapy_table_meds_join, 
    by = c("patient_id", "therapy_id", "t")
  )
  
  return(therapy_table_parenteral)
}

#' Get 'parenteral' drug administration sequences
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
#' therapy table. By default, it returns positions of sequences of at least 
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
#' @return a list containing one vector per therapy sequence. Each vector consists
#' of three integers coding for the time \code{t}:
#' \enumerate{ 
#'    \item when the sequence is initiated (parenteral administration begins)
#'    \item when the sequence ends
#'    \item \emph{if the therapy is converted to oral administration:} time of conversion,
#'    (otherwise \code{NA_integer_})
#' }
#' 
#' The integers are in direct correspondance with field \code{t} in the 
#' therapy episode's table (\code{\link{therapy_table}()}).
#' @export
#' @examples 
#' \dontrun{
#' library(dplyr)
#' conSQLite <- create_mock_database("Ramses_mock_database_example.sqlite")
#' example_therapy <- TherapyEpisode(conSQLite, "2a2b6da3b67f6f495f4cedafb029f631")
#' 
#' # Obtain the parenteral to oral sequence indexes
#' therapy_sequence <- parenteral_changes(example_therapy)
#' therapy_sequence
#' 
#' # Look for the section of the therapy table where 0 <= t <= 145
#' filter(therapy_table(example_therapy, collect = TRUE),
#'        between(t,
#'                therapy_sequence[[1]][1],
#'                therapy_sequence[[1]][2])) %>% head()
#' # Look for the section of the therapy table near conversion (t = 73)
#' filter(therapy_table(example_therapy, collect = TRUE),
#'        between(t, 70, 75))
#' }
parenteral_changes <- function(therapy_episode, tolerance_hours = 12L) {
  stopifnot(is.numeric(tolerance_hours) | length(tolerance_hours) != 1)
  tolerance_hours <- as.integer(tolerance_hours)
  TT <- therapy_table(therapy_episode, collect = T)
  .parenteral_vector_process(TT[["parenteral"]], tolerance_hours)
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

# therapy_table -------------------------------------------------------


#' Get the therapy table
#'
#' @param object an object of class \code{TherapyEpisode}
#' @param collect whether to collect the remote \code{tbl_sql} and return a local 
#' \code{tbl_df}. The default is \code{FALSE}
#' @return an object of class \code{tbl}
#' @rdname therapy_table
#' @export
setGeneric("therapy_table", function(object, collect = FALSE) standardGeneric("therapy_table"))

#' @rdname therapy_table
#' @export
setMethod("therapy_table", "TherapyEpisode", function(object, collect = FALSE) {
  stopifnot(is.logical(collect))
  if( collect ) {
    collect_ramses_tbl(object@therapy_table)
  } else {
    object@therapy_table
  }
})

#' @rdname therapy_table
#' @export
setMethod("therapy_table", "MedicationRequest", function(object, collect = FALSE) {
  stopifnot(is.logical(collect))
  object <- TherapyEpisode(object)
  if( collect ) {
    collect_ramses_tbl(object@therapy_table)
  } else {
    object@therapy_table
  }
})


# show methods ------------------------------------------------------------

setMethod("show", "RamsesObject", function(object) {
  cat(class(object), object@id, "\n")
  cat("\nDatabase connection:\n")
  print(object@conn)
})

setMethod("show", "TherapyEpisode", function(object) {
  cat(class(object), object@id, "\n")
  record <- collect_ramses_tbl(object@record)
  if( nrow(record) == 0 ) {
    cat("Record is not available.\n")
    cat("Please check object id is valid\n")
  } else {
    prescriptions <- tbl(object@conn, "drug_prescriptions") %>% 
      dplyr::filter(patient_id == !!record$patient_id & 
                      therapy_id == !!object@id) %>% 
      dplyr::arrange(therapy_rank) %>% 
      dplyr::select(prescription_text) %>% 
      dplyr::collect()
    cat("Patient:  ", record$patient_id, "\n")
    cat("Start:    ", as.character(record$therapy_start, format = "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("End:      ", as.character(record$therapy_end, format = "%Y-%m-%d %H:%M:%S %Z"), "\n\n")
    cat("Medications:\n ")
    if(nrow(prescriptions) > 5) {
      cat(paste0(" > ", prescriptions$prescription_text[1:4], "\n"))
      cat(paste0("  ... (", nrow(prescriptions) - 4, " additional medication requests)\n"))
    } else {
      cat(paste0(" > ", prescriptions$prescription_text, "\n"))
    }
  }
  cat("\nDatabase connection:\n")
  show(object@conn)
})

setMethod("show", "MedicationRequest", function(object) {
  cat(class(object), object@id, "\n")
  record <- collect_ramses_tbl(object@record)
  if( nrow(record) == 0 ) {
    cat("Record is not available.\n")
    cat("Please check object id is valid\n")
  } else {
    cat(record$prescription_text, "\n")
    cat("Patient:    ", record$patient_id, "\n")
    cat("Start:       ", as.character(record$prescription_start, format = "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("End:         ", as.character(record$prescription_end, format = "%Y-%m-%d %H:%M:%S %Z"), "\n")
    if( !is.na(record$combination_id) ) {
      cat("Combination: ", record$combination_id, "\n")
    }
    cat("Therapy:     ", record$therapy_id, "\n")
  }
  cat("\nDatabase connection:\n")
  print(object@conn)
})

