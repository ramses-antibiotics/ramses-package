




setOldClass("tbl_sql")

#' An S4 class to handle therapy episodes
#'
#' @slot id a character therapy episode identifier 
#' @slot record a `tbl_sql` for the corresponding database record
#' @slot therapy_table a `tbl_sql` for the longitudinal therapy table
#'
#' @return
#' @export
setClass(
  "TherapyEpisode", 
  slots = c(
    id = "character", 
    record = "tbl_sql",
    therapy_table = "tbl_sql"
  ),
  package = "dplyr"
)

setValidity("TherapyEpisode", function(object) {
  if (!is.character(object@id) & length(object@id) != 1) {
    "@id must be a character string of length 1"
  } else if(!is(object@record, "tbl_sql")) {
    "@record must be `tbl_sql`"
  } else {
    TRUE
  }
})

#' Create a `TherapyTable` object
#'
#' @param conn a database connection
#' @param id the therapy episode's character identifier
#' @param verify if `TRUE`, the function will first verify that the `id` 
#' exists in the `drug_therapy_episodes` database table. The default is `FALSE`
#'
#' @return a `TherapyTable` object
#' @export
TherapyEpisode <- function(conn, id, verify = FALSE) {
  
  stopifnot(is.logical(verify))
  stopifnot(is.character(id) | length(id) != 1)
  stopifnot(is(conn, "DBIConnection"))
  
  record <- tbl(conn, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == id)
  
  if(verify) {
    validation_results <- collect(record)
    if(nrow(collect(record)) == 0) {
      stop(paste0(
        "No record found for \"", 
        id, 
        "\" in the database."
      ))
    }
  }
  
  therapy_table <- .create_therapy_table(conn, id)
  
  new("TherapyTable", id = id, record = record, therapy_table = therapy_table)
}


.create_therapy_table <- function(conn, id) {
  
  if( !DBI::dbExistsTable(conn, "ramses_tally") ){
    .build_tally_table(conn)
  }
  
  record <- tbl(conn, "drug_therapy_episodes") %>% 
    dplyr::filter(therapy_id == id) %>% 
    dplyr::select(patient_id, therapy_id, therapy_start)  
  
  if(is(conn, "SQLiteConnection")) {
    tbl(conn, "ramses_tally") %>% 
      dplyr::full_join(record, by = character()) %>% 
      dplyr::mutate(time = dplyr::sql("datetime(therapy_start, (t || ' hours'))")) 
  } else {
    stop("not implemented")
  }
}

# ramses_db <- connect_local_database("vignettes/ramses-db_perm.sqlite")
# .create_therapy_table(ramses_db, "9a2268f40e891b22611c9912c834cb52")

