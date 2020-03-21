#' Create a local database
#'
#' @description Create a local database in memory or on disk using RSQLite. This is 
#' the easiest method to work on a small scale, with a limited number of users.
#'
#' @details This function creates a database on disk at the desired path. The database and
#' its content will persist on disconnection.
#'     
#' @section Warning:     
#' This method does not provide any encryption or password protection. For work with 
#' real patient data, only use suitably encrypted databases.
#'
#' @param file A file path to an existing or new database file with an .sqlite extension.
#' @return An object of class SQLiteConnection.
#' @seealso The dbplyr website provides excellent guidance on how to connect to databases: 
#' \url{https://db.rstudio.com/getting-started/connect-to-database}
#' @importFrom DBI dbConnect dbDisconnect
#' @export
#'
#' @examples
#'     # Create database and load data
#'     con <- connect_db_local("ramses-db.sqlite")
#'     
#'     dplyr::copy_to(dest = con, df = aware, name = "LU_AWaRe", 
#'                    overwrite = FALSE, temporary = FALSE)      
#'     
#'     # Close connection to database
#'     DBI::dbDisconnect(con)
#'     
#'     # Connect to the database again and show data
#'     con <- connect_db_local("ramses-db.sqlite")
#'     dplyr::tbl(con, "LU_AWaRe")
#'     DBI::dbDisconnect(con)
connect_db_local <- function(file) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  build_tally_table(con)
  warning(paste0("SQLite database created in ", con@dbname, 
                 "\nPlease do not use real patient data."))
  
  con
}


#' Get warehouse status
#'
#' @description Inspect the status of tables making up the warehouse
#' @param conn a database connection
#'
#' @return 
#'   \describe{
#'     \item{diagnoses}{a flag indicating whether }
#'   }
#' @export
get_warehouse_status <- function(conn) {
  
  if (DBI::dbExistsTable(conn, "inpatient_diagnoses")) {
    diagnoses <- dplyr::tbl(conn, "inpatient_diagnoses") %>% 
      dplyr::filter(.data, row_number()==1) %>% 
      dplyr::collect()
  } else {
    diagnoses = NULL
  }
  
  inspect_diagnoses <- (
    is.null(diagnoses) || nrow(diagnoses) == 0 || 
      any(!c("icd_code", "patient_id", "spell_id", "episode_number") %in% colnames(diagnoses))
    )
  
  list(
    diagnoses = inspect_diagnoses
  )
  
}
 
#' Build the RAMSES schema
#' 
#' The RAMSES schema is the set of tables required to import health records
#' for curation and analysis 
#'
#' @param conn a database connection
#'
build_ramses_schema <- function(conn) {
  
  UseMethod("build_ramses_schema")
  
}


build_ramses_schema.SQLiteConnection <- function(conn) {
  
}

build_tally_table <- function(conn) {
  dbplyr::db_copy_to(conn, 
                     table = "ramses_tally",
                     values = data.frame(t = 1:20000),
                     temporary = FALSE,
                     overwrite = TRUE)
}


#' Load inpatient diagnosis records into the warehouse
#' 
#' @description Validate, then load inpatient diagnosis records into the 
#' warehouse. This function automatically generates derived ICD-10 look up 
#' tables for comorbidities, infection indications, and the \code{\link{ccs}}.
#' @param conn a database connection
#' @param diagnoses_data a data frame validated with 
#' \code{\link{validate_inpatient_diagnoses}()}
#' @param diagnoses_lookup a data frame containing an ICD-10 reference 
#' lookup table
#' @param overwrite if `TRUE` (the default), will overwrite an existing 
#' diagnosis table
#' @seealso \code{\link{validate_inpatient_episodes}()}, 
#' \code{\link{map_infections_abx_indications}()},
#' \code{\link{map_charlson_comorbidities}()},
#' \code{\link{map_ICD10_CCS}()},
#' \code{\link{map_ICD10_CCSR}()}
#' @return `TRUE` if the function ran successfully, otherwise object of class
#' "try-error" containing error messages trigger during warehouse data loading.
#' @import methods
#' @export
load_diagnoses <- function(conn, diagnoses_data, diagnoses_lookup, overwrite = FALSE) {
   
  if (!validate_inpatient_diagnoses(diagnoses_data, diagnoses_lookup)) {
    stop(simpleError("`diagnoses_data` and `diagnoses_lookup` must pass `validate_inpatient_diagnoses()`"))
  }
  
  icd_infection_lkup <- map_infections_abx_indications(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code")
  
  comorbidity_lkup <- map_charlson_comorbidities(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code")
  
  ccsr_lkup <- map_ICD10_CCSR(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code")
  
  ccs_lkup <- map_ICD10_CCS(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code")
  
  diagnoses_data <- arrange_variables(
    data = diagnoses_data,
    first_column_names = c(
      "patient_id", 
      "spell_id", 
      "episode_number",
      "icd_code",
      "diagnosis_position"
    ))
  
  load_errors <- try({
    dbplyr::db_copy_to(con = conn, table = "inpatient_diagnoses", 
                       values = diagnoses_data, overwrite = overwrite, temporary = FALSE,
                       indexes = list("patient_id", "spell_id", "episode_number", "icd_code"))
    dbplyr::db_copy_to(con = conn, table = "icd_reference", 
                       values = diagnoses_lookup, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code"))
    dbplyr::db_copy_to(con = conn, table = "icd_charlson_comorbidities", 
                       values = comorbidity_lkup, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code"))
    dbplyr::db_copy_to(con = conn, table = "icd_infections", 
                       values = icd_infection_lkup, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code"))
    dbplyr::db_copy_to(con = conn, table = "icd_ccs", 
                       values = ccs_lkup, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code", "ccs_cat_code", "ccs_L1_code", "ccs_L2_code"))
    dbplyr::db_copy_to(con = conn, table = "icd_ccsr", 
                       values = ccsr_lkup, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code", "ccsr_body_system_code", "ccsr_cat_code"))
  })
  
  if (is(load_errors, "try-error")) {
    return(load_errors)
  } else {
    return(TRUE)
  }
  
}

#' Set parameters for building combination therapy identifiers and
#' therapy episode identifiers
#'
#' @description Set parameters controlling how prescriptions are 
#' linked into combinations and therapy episodes
#' @details Electronic prescribing and administration systems do not usually 
#' define:
#' \itemize{
#'   \item combination therapy: prescribing two or more antimicrobials  
#'   concurrently for the same indication
#'   \item therapy episodes: the set of antimicrobial prescriptions  
#'   administered consecutively or concurrently and corresponding to 
#'   a single indication and intent. 
#' }
#' 
#' Because of this, \code{\link{load_medications}}() first examines all 
#' prescriptions authored in a given hospital admission to ascertain such 
#' links between prescriptions. This involves looking at patterns of 
#' overlap and succession of prescriptions, and using a graph theory method 
#' known as 'transitive closure'.
#' 
#' @param max_continuation_gap a positive integer setting the maximum number of hours
#' tolerated between the end of a prescription and the start of a subsequent 
#' prescription in the same therapy episode. The default is 36 hours. 
#' @param max_combination_authoring_gap a positive integer setting the maximum number
#' of hours tolerated between the time of authoring of two prescriptions 
#' administered as combination therapy. The default is 6 hours.
  #' @param max_combination_start_gap a positive integer setting the maximum number of 
#' hours tolerated between the start of administration of two prescriptions
#' administred as combination therapy. The default is 24 hours.
#'
#' @return A list of three positive integer variables.
#' @export
#'
#' @examples
#' # default parameters
#' transitive_closure_control(
#'   max_continuation_gap = 36,
#'   max_combination_authoring_gap = 6,
#'   max_combination_start_gap = 24)
transitive_closure_control <- function(max_continuation_gap = 36,
                                       max_combination_authoring_gap = 6,
                                       max_combination_start_gap = 24) {
  
  argument_length <- sapply(list(max_continuation_gap,
                                 max_combination_authoring_gap,
                                 max_combination_start_gap), length) != 1
  
  if(any(argument_length)) {
    stop(simpleError("Parameters must have length == 1"))
  }
  
  argument_negative <- sapply(list(max_continuation_gap,
                                   max_combination_authoring_gap,
                                   max_combination_start_gap), sign) == -1
  
  if(any(argument_negative)) {
    stop(simpleError("Parameters must be >= 0"))
  }
  
  list(max_combination_authoring_gap = as.integer(max_continuation_gap),
       max_combination_start_gap = as.integer(max_combination_start_gap),
       max_continuation_gap = as.integer(max_continuation_gap))
  
}



#' Load drug prescription records into the warehouse
#'
#' @param conn a database connection
#' @param prescriptions a data frame of prescriptions passing the 
#' \code{\link{validate_prescriptions}()} function
#' @param administrations a data frame of drug administrations passing the 
#' \code{\link{validate_administrations}()} function
#' @param overwrite if `TRUE` (the default), will overwrite an existing 
#' diagnosis table
#' @param transitive_closure_controls parameters controlling (see 
#' \code{\link{transitive_closure_control}()})
#'
#' @return `TRUE` if the function ran successfully, otherwise object of class
#' "try-error" containing error messages trigger during warehouse data loading.
#' @rdname load_medications
#' @export
load_medications <- function(
  conn, prescriptions, administrations, overwrite = FALSE,
  transitive_closure_controls = transitive_closure_control()) {
  UseMethod("load_medications")
}
#' @export
load_medications.SQLiteConnection <- function(
  conn, prescriptions, administrations, overwrite = FALSE,
  transitive_closure_controls = transitive_closure_control()) {
  
  prescriptions$authoring_date <- as.character(prescriptions$authoring_date)
  prescriptions$prescription_start <- as.character(prescriptions$prescription_start)
  prescriptions$prescription_end <- as.character(prescriptions$prescription_end)

  first_column_names <- .drug_prescriptions_variables()[["variable_name"]]
  first_column_names <- first_column_names[
    first_column_names %in% names(prescriptions)
  ]
  prescriptions <- arrange_variables(prescriptions, 
                                     first_column_names = first_column_names)
 
  prescriptions <- dbplyr::db_copy_to(
    con = conn,
    table = "ramses_temp_prescriptions",
    values = prescriptions,
    temporary = FALSE,
    overwrite = TRUE,
    indexes = list(
      "patient_id", 
      "prescription_id",
      "authoring_date",
      "prescription_start", 
      "prescription_end"
    )
    )
  
  if( DBI::dbExistsTable(conn, "drug_prescriptions_edges") ){
    DBI::dbRemoveTable(conn, "drug_prescriptions_edges")
  }
  
  DBI::dbExecute(
    conn = conn,
    statement = .read_sql_syntax(
      system.file("SQL", 
                  "create_drug_prescription_edges_SQLite.sql", 
                  package = "Ramses")))
 
  
  statement_edges <- .read_sql_syntax(
    system.file("SQL", 
                "drug_prescriptions_edges_SQLite.sql", 
                package = "Ramses"))
  
  # replace variables in SQL code by their value
  for(i in seq_along(transitive_closure_controls)) {
    statement_edges <- gsub(
      paste0("@", names(transitive_closure_controls[i])),
      transitive_closure_controls[[i]], statement_edges)
  }

  ignore <- DBI::dbExecute(
    conn = conn,
    statement = statement_edges)
  
  
}
  






#' Manage internal tables used for transitive closures
#'
#' @description This attempts to remove all `Ramses_TC_*`
#' tables in the database connection if they exist.
#' @param conn a database connection
#'
#' @return NULL if ran to the end
#' @noRd
.remove_Ramses_TC_tables <- function(conn){
  
  for( i in c("ramses_TC_input",
              "ramses_TC_init") ) {
    DBI::dbRemoveTable(conn = conn,
                       name = i, 
                       fail_if_missing = FALSE)
  }
  
  NULL
}

.create_ramses_TC_graphs <- function(conn){
  UseMethod(".create_ramses_TC_graphs")
}
.create_ramses_TC_graphs.SQLiteConnection <- function(conn){
  DBI::dbRemoveTable(conn = conn,
                     name = "ramses_TC_graphs",
                     fail_if_missing = FALSE)
  
  DBI::dbExecute(
    conn = conn,
    statement = .read_sql_syntax("inst/SQL/create_ramses_TC_graphs_SQLite.sql"))

}


.read_sql_syntax <- function(file.path) {
  
  statement <- readLines(file.path)
  # remove comments
  statement <- sapply(statement, gsub, pattern = "--.*", replacement = "")
  # concatenate on single line
  statement <- paste(statement, collapse = " ")
  
  statement
}


#' Run transitive closure
#'
#' @param conn a database connection
#' @param edges a table of class `tbl_dbi` containing two columns
#' named `from_id` and `to_id` 
#' @param tablename a table name where to store the output. Any 
#' existing table under this name will be overwritten.
#' @return a connection to a non-temporary table of class `tbl_dbi`
#' under the name `tablename`. 
#' @noRd
.run_transitive_closure <- function(conn, edges, tablename) {
  
  .remove_Ramses_TC_tables(conn)
  .create_ramses_TC_graphs(conn)
  
  
  #edges must have two text variables from_id and to_id
  Ramses_closure_input <- edges %>% 
    select(from_id, to_id) %>% 
    compute(.data, name = "ramses_TC_input")
  
  Ramses_closure_init <- Ramses_closure_input %>% 
    top_n(1) %>% 
    compute(.data, name = "ramses_TC_init")
  
  
}

build_table_drug_prescriptions_edges <- function(transitive_closure_controls) {
  
}

build_table_drug_prescriptions_edges.SQLiteConnection <- function() {
  
}

build_table_drug_prescriptions_edges.PostgreSQLConnection <- function() {
  
}

build_table_drug_prescriptions_edges.MSSQLConnection <- function() {
  
}

build_temp_table_rx_combinations <- function() {
  
}

build_temp_table_rx_therapy <- function() {
  
}

built_table_drug_prescriptions <- function() {
  
}



