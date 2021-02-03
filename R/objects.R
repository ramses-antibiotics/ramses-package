


# Interface ---------------------------------------------------------------

setOldClass(c("tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))
setOldClass(c("tbl_PqConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))
setGeneric("collect", function(object) standardGeneric("collect"))
setGeneric("compute", function(object) standardGeneric("compute"))

#' An S4 virtual class for Ramses objects
#'
#' @slot id a character identifier 
#' @slot conn a database connection
#' @slot record a \code{tbl_sql} for the corresponding database record
#' @rdname RamsesObject
#' @importFrom dplyr collect compute
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

setMethod("collect", "RamsesObject", function(object) {
  dplyr::collect(object@record)
})

setMethod("compute", "RamsesObject", function(object) {
  object@record <- dplyr::compute(object@record)
  object
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
#' drug-dose order. as monotherapy a combination therapy
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
      dplyr::mutate(t_start = dplyr::sql("datetime(therapy_start, (t || ' hours'))")) %>% 
      dplyr::filter(t_start < therapy_end) %>% 
      dplyr::mutate(t_end = dplyr::sql("datetime(therapy_start, ((t + 1) || ' hours'))")) %>% 
      dplyr::mutate(t_end = dplyr::if_else(
        t == max(t, na.rm = TRUE),
        therapy_end,
        t_end
      )) %>% 
      .therapy_table_parenteral_indicator(therapy_id = id)
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
      )) %>% 
      .therapy_table_parenteral_indicator(therapy_id = id)
  } else {
    .throw_error_DBI_subclass_not_implemented(".create_therapy_table()")
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
        na.rm = T) == 1.0,
      1L,
      0L
    ))
  
  therapy_table_parenteral <- dplyr::left_join(
    therapy_table, 
    therapy_table_meds_join, 
    by = c("patient_id", "therapy_id", "t")
  )
  
  return(therapy_table_parenteral)
}


# get_therapy_table -------------------------------------------------------


#' Get the therapy table
#'
#' @param object an object of class \code{TherapyEpisode}
#' @param collect whether to collect the remote \code{tbl_sql} and return a local 
#' \code{tbl_df}. The default is \code{FALSE}
#' @return an object of class \code{tbl}
#' @rdname get_therapy_table
#' @export
setGeneric("get_therapy_table", function(object, collect = FALSE) standardGeneric("get_therapy_table"))

#' @rdname get_therapy_table
#' @export
setMethod("get_therapy_table", "TherapyEpisode", function(object, collect = FALSE) {
  stopifnot(is.logical(collect))
  if( collect ) {
    collect_ramses_tbl(object@therapy_table)
  } else {
    object@therapy_table
  }
})
