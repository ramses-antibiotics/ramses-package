

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
  .build_tally_table(con)
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
#' @description The RAMSES schema is the set of tables required to import health records
#' for curation and analysis 
#' @param conn a database connection
#' @rdname build_ramses_schema
#' @export
build_ramses_schema <- function(conn) {
  UseMethod("build_ramses_schema")
}
#' @export
build_ramses_schema.SQLiteConnection <- function(conn) {
  
}

.build_tally_table <- function(conn) {
  # Build table to use in joins to create therapy tables
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
#' @param overwrite if `TRUE` (the default), will overwrite any existing
#' `inpatient_diagnoses` database table
#' @seealso \code{\link{validate_inpatient_episodes}()}, 
#' \code{\link{map_infections_abx_indications}()},
#' \code{\link{map_charlson_comorbidities}()},
#' \code{\link{map_ICD10_CCS}()},
#' \code{\link{map_ICD10_CCSR}()}
#' @return `TRUE` if the function ran successfully, otherwise object of class
#' "try-error" containing error messages trigger during warehouse data loading.
#' @import methods
#' @export
load_inpatient_diagnoses <- function(conn, diagnoses_data, diagnoses_lookup, overwrite = FALSE) {
   
  if (!validate_inpatient_diagnoses(diagnoses_data, diagnoses_lookup)) {
    stop(simpleError("`diagnoses_data` and `diagnoses_lookup` must pass `validate_inpatient_diagnoses()`"))
  }
  
  reference_icd_infections <- map_infections_abx_indications(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code") %>% 
    na.omit()
  
  reference_icd_comorbidity <- map_charlson_comorbidities(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code") %>% 
    na.omit()
  
  reference_icd_ccsr <- map_ICD10_CCSR(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code") %>% 
    na.omit()
  
  reference_icd_ccs <- map_ICD10_CCS(
    df = dplyr::select(diagnoses_lookup, icd_code), 
    icd_column = "icd_code") %>% 
    na.omit()
  
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
    dbplyr::db_copy_to(con = conn, table = "reference_icd", 
                       values = diagnoses_lookup, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code"))
    dbplyr::db_copy_to(con = conn, table = "reference_icd_comorbidity", 
                       values = reference_icd_comorbidity, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code", "comorb", "comorb_group"))
    dbplyr::db_copy_to(con = conn, table = "reference_icd_infections", 
                       values = reference_icd_infections, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code"))
    dbplyr::db_copy_to(con = conn, table = "reference_icd_ccs", 
                       values = reference_icd_ccs, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code", "ccs_cat_code", "ccs_L1_code", "ccs_L2_code"))
    dbplyr::db_copy_to(con = conn, table = "reference_icd_ccsr", 
                       values = reference_icd_ccsr, overwrite = overwrite, temporary = FALSE,
                       indexes = list("icd_code", "ccsr_body_system_code", "ccsr_cat_code"))
  })
  
  if (is(load_errors, "try-error")) {
    return(load_errors)
  } else {
    return(TRUE)
  }
  
}

#' Load episodes of care records into the warehouse
#'
#' @param conn a database connection
#' @param episodes_data  a data frame validated with 
#' \code{\link{validate_inpatient_episodes}()}
#' @param overwrite if `TRUE` (the default), will overwrite any existing
#' `inpatient_episodes` database table
#' @return `TRUE` if the function ran successfully, otherwise object of class
#' "try-error" containing error messages trigger during warehouse data loading.
#' @export
load_inpatient_episodes <- function(conn, episodes_data, overwrite = TRUE) {
  
  administrations <- dplyr::arrange(administrations, 
                                    patient_id, administration_date)
  
  administrations$id <- 1:nrow(administrations)
  
  first_column_names <- .drug_administrations_variables()[["variable_name"]]
  first_column_names <- c(
    "id",
    first_column_names[first_column_names %in% names(administrations)]
  )
  administrations <- arrange_variables(
    administrations, 
    first_column_names = first_column_names) 
  
  administrations <- dbplyr::db_copy_to(
    con = conn,
    table = "drug_administrations",
    values = administrations,
    temporary = FALSE,
    overwrite = overwrite,
    indexes = list(
      "id",
      "patient_id", 
      "administration_id",
      "drug_id",
      "ATC_code",
      "ATC_route",
      "administration_date"
    ))
  
  TRUE
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
#' \code{\link{validate_prescriptions}()} function. Note: if variables 
#' `therapy_id` or `combination_id` are provided, they will be preserved
#' as they are instead of being populated by `Ramses`
#' @param administrations a data frame of drug administrations passing the 
#' \code{\link{validate_administrations}()} function
#' @param overwrite if `TRUE` (the default), will overwrite an existing 
#' diagnosis table
#' @param transitive_closure_controls parameters controlling (see 
#' \code{\link{transitive_closure_control}()})
#' @param silent a boolean indicating whether the function should be
#' executed without progress message. Default is `FALSE`
#'
#' @return `TRUE` if the function ran successfully, otherwise object of class
#' "try-error" containing error messages trigger during warehouse data loading.
#' @rdname load_medications
#' @export
load_medications <- function(
  conn, prescriptions, administrations = NULL, overwrite = FALSE,
  transitive_closure_controls = transitive_closure_control(),
  silent = FALSE) {
  UseMethod("load_medications")
}
#' @export
load_medications.SQLiteConnection <- function(
  conn, prescriptions, administrations, overwrite = FALSE,
  transitive_closure_controls = transitive_closure_control(),
  silent = FALSE) {
  
  prescription_load_errors <- try({
    .load_prescriptions.SQLiteConnection(
      conn = conn, 
      prescriptions = prescriptions, 
      overwrite = overwrite,
      transitive_closure_controls,
      silent = silent)
  })
  
  
  if(!is.null(administrations)) {
    administration_load_errors <- try({
    .load_administrations.SQLiteConnection(
      conn = conn, 
      administrations = administrations, 
      overwrite = overwrite)
    })
    
    return(list(prescription_load_errors = 
                  prescription_load_errors,
                administration_load_errors = 
                  administration_load_errors))
  } else {
    
    return(list(prescription_load_errors = 
                  prescription_load_errors))
  }
  
}


.load_prescriptions.SQLiteConnection <- function(
  conn, prescriptions, overwrite, 
  transitive_closure_controls, silent) {
  
  create_therapy_id <- !exists("therapy_id", prescriptions)
  create_combination_id <- !exists("combination_id", prescriptions)
  
  prescriptions <- dplyr::arrange(prescriptions, patient_id, prescription_start)
  prescriptions$id <- 1:nrow(prescriptions)
  prescriptions$authoring_date <- as.character(prescriptions$authoring_date)
  prescriptions$prescription_start <- as.character(prescriptions$prescription_start)
  prescriptions$prescription_end <- as.character(prescriptions$prescription_end)
  if(create_combination_id) prescriptions$combination_id <- NA_character_
  if(create_therapy_id) prescriptions$therapy_id <- NA_character_
  
  first_column_names <- .drug_prescriptions_variables()[["variable_name"]]
  first_column_names <- c(
    "id",
    first_column_names[first_column_names %in% names(prescriptions)]
  )
  prescriptions <- arrange_variables(
    prescriptions, 
    first_column_names = first_column_names) 
  
  prescriptions <- dbplyr::db_copy_to(
      con = conn,
      table = "drug_prescriptions",
      values = prescriptions,
      temporary = FALSE,
      overwrite = overwrite,
      indexes = list(
        "id",
        "patient_id", 
        "prescription_id",
        "combination_id",
        "therapy_id",
        "drug_id",
        "ATC_code",
        "ATC_route",
        "authoring_date",
        "prescription_start", 
        "prescription_end"
      ))
  
  .create_table_drug_prescriptions_edges.SQLiteConnection(
    conn = conn, transitive_closure_controls)
  
  if(create_therapy_id) .create_therapy_id.SQLiteConnection(conn = conn, 
                                                            silent = silent)
  if(create_combination_id) .create_combination_id.SQLiteConnection(conn = conn, 
                                                                    silent = silent)
  
  TRUE
}



#' Create table `drug_prescriptions_edges`
#'
#' @description Create or reoverwrite the database table 
#' `drug_prescriptions_edges` and populate by drawing edges between
#' prescriptions in the `drug_prescriptions` table based on 
#' patterns of overlap determined by `transitive_closure_controls`
#' TODO:put link to online documentation describing the classification
#' of patterns of prescription overlap  
#' @param conn a database connection
#' @param transitive_closure_controls parameters controlling the creation
#' of edges between prescriptions
#' @return NULL
#' @noRd
.create_table_drug_prescriptions_edges <- function(conn, transitive_closure_controls) {
  UseMethod(".create_table_drug_prescriptions_edges")
}

.create_table_drug_prescriptions_edges.SQLiteConnection <- 
  function(conn, transitive_closure_controls) {
  
  if( DBI::dbExistsTable(conn, "drug_prescriptions_edges") ){
    DBI::dbRemoveTable(conn, "drug_prescriptions_edges")
  }
  DBI::dbExecute(
    conn = conn,
    statement = .read_sql_syntax("create_drug_prescription_edges_SQLite.sql"))
  
  statement_edges <- .read_sql_syntax("drug_prescriptions_edges_SQLite.sql")
  
  # replace variables in SQL code by their value
  for(i in seq_along(transitive_closure_controls)) {
    statement_edges <- gsub(
      paste0("@", names(transitive_closure_controls[i])),
      transitive_closure_controls[[i]], statement_edges)
  }
  
  DBI::dbExecute(
    conn = conn,
    statement = statement_edges)
}


#' Populate `drug_prescription.therapy_id`
#'
#' @param conn a data source with a table `drug_prescriptions_edges`
#' already populated
#' @param silent a boolean indicating whether the function should be
#' executed without progress message
#' @noRd
.create_therapy_id.SQLiteConnection <- function(conn, silent) {
  
  DBI::dbRemoveTable(conn, "ramses_TC_edges", fail_if_missing = F)
  
  edges_table <- tbl(conn, "drug_prescriptions_edges") %>% 
    dplyr::transmute(id1 = from_id, id2 = to_id) %>% 
    dplyr::compute(name = "ramses_TC_edges", temporary = F)
  
  DBI::dbExecute(conn, "CREATE INDEX ramses_TC_edges_idx1 ON ramses_TC_edges (id1, id2);")
  DBI::dbExecute(conn, "CREATE INDEX ramses_TC_edges_idx2 ON ramses_TC_edges (id2, id1);")
  
  if(!silent) message("Transitive closure of therapy episodes beginning...")
  therapy_grps <- .run_transitive_closure(conn = conn, 
                                          edge_table = "ramses_TC_edges",
                                          silent = silent)
  if(!silent) message("\n")
  
  therapy_grps <- tbl(conn, "ramses_TC_group") %>% 
    dplyr::left_join(
      dplyr::select(tbl(conn, "drug_prescriptions"), 
                    id, prescription_id), 
      by = c("id" = "id")
    ) %>% 
    group_by(grp) %>% 
    mutate(therapy_id = min(prescription_id, na.rm = T)) %>% 
    ungroup() %>% 
    distinct(prescription_id, therapy_id) %>% 
    compute(name = "ramses_TC_therapy")
  
  update_therapy_id <- .read_sql_syntax("update_drug_prescriptions_therapy_id_SQLite.sql")
  update_therapy_id <- gsub("@@@ramses_TC_table", "ramses_TC_therapy", update_therapy_id)
  update_therapy_id <- .split_sql_batch(update_therapy_id)
  for(i in update_therapy_id) {
    DBI::dbExecute(conn, i)
  }
  .remove_db_tables(conn, c("ramses_TC_group", "ramses_TC_therapy"))
}


#' Populate `drug_prescription.combination_id`
#'
#' @param conn a data source with a table `drug_prescriptions_edges`
#' already populated
#' @param silent a boolean indicating whether the function should be
#' executed without progress message
#' @noRd
.create_combination_id.SQLiteConnection <- function(conn, silent) {
  
  DBI::dbRemoveTable(conn, "ramses_TC_edges", fail_if_missing = F)
  
  edges_table <- tbl(conn, "drug_prescriptions_edges") %>% 
    dplyr::filter(edge_type == "combination") %>% 
    dplyr::transmute(id1 = from_id, id2 = to_id) %>% 
    dplyr::compute(name = "ramses_TC_edges", temporary = F)
  
  DBI::dbExecute(conn, "CREATE INDEX ramses_TC_edges_idx1 ON ramses_TC_edges (id1, id2);")
  DBI::dbExecute(conn, "CREATE INDEX ramses_TC_edges_idx2 ON ramses_TC_edges (id2, id1);")
  
  if(!silent) message("Transitive closure of therapy combinations beginning...")
  therapy_grps <- .run_transitive_closure(conn = conn, 
                                          edge_table = "ramses_TC_edges",
                                          silent = silent)
  if(!silent) message("\n")
  
  therapy_grps <- tbl(conn, "ramses_TC_group") %>% 
    dplyr::left_join(
      dplyr::select(tbl(conn, "drug_prescriptions"), 
                    id, prescription_id), 
      by = c("id" = "id")
    ) %>% 
    group_by(grp) %>% 
    mutate(combination_id = min(prescription_id, na.rm = T)) %>% 
    ungroup() %>% 
    distinct(prescription_id, combination_id) %>% 
    compute(name = "ramses_TC_combination")
  
  update_combination_id <- .read_sql_syntax("update_drug_prescriptions_combination_id_SQLite.sql")
  update_combination_id <- gsub("@@@ramses_TC_table", "ramses_TC_combination", update_combination_id)
  update_combination_id <- .split_sql_batch(update_combination_id)
  for(i in update_combination_id) {
    DBI::dbExecute(conn, i)
  }
  .remove_db_tables(conn, c("ramses_TC_group", "ramses_TC_combination"))
}



.load_administrations.SQLiteConnection <- function(
  conn, administrations, overwrite) {
  
  administrations <- dplyr::arrange(administrations, 
                                    patient_id, administration_date)
  administrations$id <- 1:nrow(administrations)
   
  first_column_names <- .drug_administrations_variables()[["variable_name"]]
  first_column_names <- c(
    "id",
    first_column_names[first_column_names %in% names(administrations)]
  )
  administrations <- arrange_variables(
    administrations, 
    first_column_names = first_column_names) 
  
  load_output <- dbplyr::db_copy_to(
    con = conn,
    table = "drug_administrations",
    values = administrations,
    temporary = FALSE,
    overwrite = overwrite,
    indexes = list(
      "id",
      "patient_id", 
      "administration_id",
      "drug_id",
      "ATC_code",
      "ATC_route",
      "administration_date"
    ))
  
  if(load_output == "drug_administrations") {
    TRUE
  } else {
    load_output
  }
}
  
#' Read SQL scripts
#'
#' @description Reads SQL script, remove comments and concatenate
#' all script lines into one line ready for parsing.
#' @param script.name file name of a script located under `inst/SQL/`
#' @return A character string
#' @noRd
.read_sql_syntax <- function(script.name) {
  
  statement <- readLines(
    system.file("SQL", script.name, package = "Ramses", mustWork = TRUE)
  )
  # remove comments
  statement <- sapply(statement, gsub, pattern = "--.*", replacement = "")
  # concatenate on single line
  statement <- paste(statement, collapse = " ")
  
  statement
}

#' Split SQL statements from a batch script
#'
#' @description Reads SQL script, remove comments and concatenate
#' all script lines into one line ready for parsing.
#' @note IMPORTANT: This function may not be used for statements containing
#' nested loops (WHILE, BEGIN, END; within a loop)
#' @param script.name file name of a script located under `inst/SQL/`
#' @return A character vector of SQL statements
#' @noRd
.split_sql_batch <- function(string) {
  unlist(regmatches(
    string,
    gregexpr("(WHILE(?:(?!WHILE|END;).)*END;)|((?:(?!WHILE|END;|;).)*;)",
             string,
             perl = TRUE)))
}

#' Remove database tables
#'
#' @description This attempts to remove one or more tables
#' in the database connection if they exist. Does t
#' @param conn a database connection
#' @param tables a character vector of table names
#'
#' @return NULL if ran to the end
#' @noRd
.remove_db_tables <- function(conn, table_names){
  
  for( i in table_names) {
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
  .remove_db_tables(conn, "ramses_TC_group")
  job <- .split_sql_batch(.read_sql_syntax("create_ramses_TC_group_SQLite.sql"))
  for(i in seq_along(job)) {
    DBI::dbExecute(conn, job[i])
  }
}

.nrow_sql_table <- function(conn, table){
  nrow <- tbl(conn, table) %>% 
    dplyr::summarise( n = n()) %>% 
    dplyr::collect()
  
  nrow$n
}

#' Run transitive closure
#'
#' @param conn a database connection
#' @param edge_table a table of class `tbl_dbi` containing two columns
#' named `id1` and `id2`
#' @return a connection to a non-temporary table of class `tbl_dbi`
#' under the name `ramses_TC_group`.
#' @references Inspired by Itzik Ben-Gan's T-SQL challenge solution
#' https://www.itprotoday.com/sql-server/t-sql-puzzle-challenge-grouping-connected-items
#' @noRd
.run_transitive_closure <- function(conn, edge_table, silent = FALSE) {
  UseMethod(".run_transitive_closure")
}
#' @export
.run_transitive_closure.SQLiteConnection <- function(conn, edge_table, silent) {
  
  .create_ramses_TC_graphs(conn)
  
  var_lvl <- 1
  var_ids <- tbl(conn, edge_table) %>% 
    dplyr::arrange(id1, id2) %>% 
    utils::head(.data, n = 1) %>% 
    dplyr::collect() %>% 
    data.frame()
  
  var_rowcount <- nrow(var_ids)
  
  if(!silent) {
    progress <- dplyr::progress_estimated(
      n = .nrow_sql_table(conn, edge_table))
  }
  
  while (var_rowcount > 0) {
    
    DBI::dbAppendTable(conn = conn, 
                       name = "ramses_TC_group",
                       value = data.frame(cbind(
                         id = c(var_ids$id1, var_ids$id2),
                         grp = c(var_ids$id1, var_ids$id1),
                         lvl = c(var_lvl, var_lvl)
                       ), stringsAsFactors = F))
    
    DBI::dbExecute(
      conn, 
      paste("DELETE FROM", edge_table, 
            "WHERE id1 =", var_ids$id1,
            "AND id2 =", var_ids$id2, ";"))
    
    if(!silent) {
      progress$i <- progress$n - .nrow_sql_table(conn, edge_table)
      progress$print()
    }
    
    while(var_rowcount > 0) {  

      var_lvl <- var_lvl + 1
      
      .remove_db_tables(conn, "ramses_TC_CurIds")
      DBI::dbExecute(conn,
                     "CREATE TEMPORARY TABLE ramses_TC_CurIds(id INTEGER NOT NULL);")
      
      DBI::dbExecute(conn, paste(
        "INSERT INTO ramses_TC_CurIds",
        "SELECT T1.id2",
        "FROM ramses_TC_group AS G",
        " INNER JOIN", edge_table, "AS T1",
        " ON G.id = T1.id1",
        "WHERE lvl =", var_lvl - 1, ";"))
      
      DBI::dbExecute(conn, paste(
        "DELETE FROM", edge_table,
        "WHERE id1 IN(", 
        "  SELECT id",
        "  FROM ramses_TC_group",
        "  WHERE lvl =", var_lvl - 1,");"))
      
      var_rowcount <- DBI::dbExecute(conn, paste(
        "INSERT INTO ramses_TC_group",
        "SELECT DISTINCT id,", var_ids$id1, "AS grp,", var_lvl, "AS lvl",
        "FROM ramses_TC_CurIds AS C",
        "WHERE NOT EXISTS (",
        "   SELECT * FROM ramses_TC_group AS G",
        "   WHERE G.id = C.id);"))
      
      .remove_db_tables(conn, "ramses_TC_CurIds")
      DBI::dbExecute(conn,
                     "CREATE TEMPORARY TABLE ramses_TC_CurIds(id INTEGER NOT NULL);")
      
      DBI::dbExecute(conn, paste(
        "INSERT INTO ramses_TC_CurIds",
        "SELECT T1.id1",
        "FROM ramses_TC_group AS G",
        " INNER JOIN", edge_table, "AS T1",
        " ON G.id = T1.id2",
        "WHERE lvl =", var_lvl - 1, ";"))
      
      DBI::dbExecute(conn, paste(
        "DELETE FROM", edge_table,
        "WHERE id2 IN(", 
        "  SELECT id",
        "  FROM ramses_TC_group",
        "  WHERE lvl =", var_lvl - 1,");"))
      
      var_rowcount <- var_rowcount + DBI::dbExecute(conn, paste(
        "INSERT INTO ramses_TC_group",
        "SELECT DISTINCT id,", var_ids$id1, "AS grp,", var_lvl, "AS lvl",
        "FROM ramses_TC_CurIds AS C",
        "WHERE NOT EXISTS (",
        "   SELECT * FROM ramses_TC_group AS G",
        "   WHERE G.id = C.id);"))
      
    }
    
    var_ids <- tbl(conn, edge_table) %>% 
      dplyr::arrange(id1, id2) %>% 
      utils::head(.data, n = 1) %>% 
      dplyr::collect() %>% 
      data.frame()
    
    var_rowcount <- nrow(var_ids)
    
  }
  if(!silent) {
    progress$i <- progress$n
    progress$print()
  }
  
  .remove_db_tables(conn, "ramses_TC_CurIds")
  
  tbl(conn, "ramses_TC_group")
}


.prepare_example_drug_records <- function() {
  
  drug_rx <- Ramses::drug_prescriptions
  drug_admins <- Ramses::drug_administrations
  
  drug_rx$ab <- gsub("Vancomycin protocol", "Vancomycin", drug_rx$tr_DESC)
  drug_rx$ab <- as.character(AMR::as.ab(drug_rx$ab))
  drug_rx$drug_name <- AMR::ab_name(drug_rx$ab)
  ## recoding route of administration
  drug_rx <- mutate(
    drug_rx, 
    ATC_route = case_when(
      route %in% c(NULL) ~ "Implant", 
      route %in% c("NEB", "INHAL") ~ "Inhal", 
      route %in% c("TOP", "EYE", "EYEL", "EYER", "EYEB", 
                   "EAR", "EARL", "EARR", "EARB") ~ "Instill", 
      route %in% c("NASAL", "NOST", "NOSTL", "NOSTR", "NOSTB") ~ "N", 
      route %in% c("ORAL", "NAS", "PEG") ~ "O", 
      route %in% c("IV", "IVB", "IVI", "IMI", "IT", "IVT") ~ "P", 
      route %in% c("PR") ~ "R", 
      route %in% c("BUCC", "SB", "OROM", "SUBL") ~ "SL", 
      route %in% c("SC", "ID") ~ "TD", 
      route %in% c("PV") ~ "V", 
      TRUE ~ "NA_character_"
    ))
  
  ## compute_ddd
  drug_rx$ATC_code <- AMR::ab_atc(drug_rx$ab)
  drug_rx$ATC_group <- AMR::ab_atc_group1(drug_rx$ab)
  
  # prepare DDD extraction
  compound_strength_lookup <- data.frame(list(
    ab = c("AMC", "AMC", "TZP", "SMX"),
    route = c("oral", "oral", "oral", "oral"),
    dose = c(625, 1.2, 4.5, 480),
    units = c("mg", "g", "g", "mg"),
    strength = c(500, 1, 4, 400),
    basis_of_strength = c("AMX", "AMX", "PIP", "SMX")
  ), stringsAsFactors = F)
  
  drug_rx <- merge(drug_rx, compound_strength_lookup, all.x = T)
  drug_rx <- drug_rx %>% 
    mutate(strength = if_else(is.na(strength), dose, strength),  
           basis_of_strength = if_else(is.na(basis_of_strength),
                                       as.character(ab), basis_of_strength))
  
  drug_rx <- merge(drug_rx, reference_drug_frequency, by = "frequency", all.x = T)
  
  # computing the prescription DDD the reference DDD from the ATC
  drug_rx <- drug_rx %>% 
    mutate(DDD = compute_DDDs(
      ATC_code = AMR::ab_atc(basis_of_strength),
      ATC_administration = ATC_route,
      dose = strength * daily_frequency,
      unit = units),
      duration_days = if_else(
        daily_frequency == -1,
        "one-off", paste( difftime(
          prescription_end, prescription_start, units = "days"),
          "days"))) %>% 
    transmute(patient_id,
              prescription_id,
              prescription_text = paste0(
                drug_name, " ", route, " ", dose, units,
                " ",  duration_days),
              drug_id = ab,
              drug_name = drug_name,
              drug_display_name = drug_name,
              ATC_code,
              ATC_group, 
              ATC_route,
              authoring_date,
              prescription_start,
              prescription_end,
              prescription_status = "completed",
              prescription_context = "inpatient",
              dose,
              unit = units,
              route,
              frequency,
              daily_frequency,
              DDD)
  
  
  ## prepare_drug_admin
  drug_admins$ab <- gsub("Vancomycin protocol", "Vancomycin", drug_admins$tr_DESC)
  drug_admins$ab <- as.character(AMR::as.ab(drug_admins$ab))
  drug_admins$drug_name <- AMR::ab_name(drug_admins$ab)
  # recoding route of administration
  drug_admins <- mutate(
    drug_admins, 
    ATC_route = case_when(
      route %in% c(NULL) ~ "Implant", 
      route %in% c("NEB", "INHAL") ~ "Inhal", 
      route %in% c("TOP", "EYE", "EYEL", "EYER", "EYEB", 
                   "EAR", "EARL", "EARR", "EARB") ~ "Instill", 
      route %in% c("NASAL", "NOST", "NOSTL", "NOSTR", "NOSTB") ~ "N", 
      route %in% c("ORAL", "NAS", "PEG") ~ "O", 
      route %in% c("IV", "IVB", "IVI", "IMI", "IT", "IVT") ~ "P", 
      route %in% c("PR") ~ "R", 
      route %in% c("BUCC", "SB", "OROM", "SUBL") ~ "SL", 
      route %in% c("SC", "ID") ~ "TD", 
      route %in% c("PV") ~ "V", 
      TRUE ~ "NA_character_"
    ))
  drug_admins$ATC_code <- AMR::ab_atc(drug_admins$ab)
  drug_admins$ATC_group <- AMR::ab_atc_group1(drug_admins$ab)
  
  drug_admins <- merge(drug_admins, compound_strength_lookup, all.x = T)
  drug_admins <- drug_admins %>% 
    mutate(strength = if_else(is.na(strength), dose, strength),
           basis_of_strength = if_else(is.na(basis_of_strength),
                                       as.character(ab), basis_of_strength))
  
  drug_admins <- drug_admins %>% 
    mutate(DDD = compute_DDDs(
      ATC_code = AMR::ab_atc(basis_of_strength),
      ATC_administration = ATC_route,
      dose = dose,
      unit = units 
    ))
  
  drug_admins$administration_id <- drug_admins %>%
    dplyr::group_indices(
      patient_id,
      ab,
      route,
      dose,
      units,
      administration_date)
  drug_admins <- transmute(drug_admins,
      patient_id,
      administration_id = as.character(administration_id),
      prescription_id,
      administration_text = paste0(
          drug_name, " ", route, " ", dose, units),
      drug_id = ab,
      drug_name,
      drug_display_name = drug_name,
      ATC_code,
      ATC_group,
      ATC_route,
      dose,
      unit = units,
      route,
      administration_date,
      administration_status = "completed",
      DDD
    )
  
  return(list(
    drug_rx = drug_rx,
    drug_admins = drug_admins
  ))
  
}

.prepare_example_inpatient_records <- function() {
  
  ip_diagnoses <- Ramses::inpatient_diagnoses
  ip_diagnoses <- filter(ip_diagnoses, !is.na(.data$icd_code))
  
  ip_episodes <- Ramses::inpatient_episodes
  ip_episodes <- ip_episodes %>% 
    dplyr::filter(!is.na(spell_id)) %>% 
    dplyr::group_by(.data$spell_id) %>% 
    dplyr::mutate(last_episode_in_spell_indicator = if_else(
      episode_number == max(.data$episode_number),
      1, 2)) %>% 
    dplyr::ungroup()
  
  list(
    episodes = ip_episodes,
    diagnoses = ip_diagnoses,
    ward_movements = Ramses::inpatient_wards
  )
}
