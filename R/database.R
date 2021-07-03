


# Inpatient records -------------------------------------------------------


#' Load inpatient diagnosis records into the warehouse
#' 
#' @description Validate, then load records of clinical diagnoses into the 
#' warehouse. This function automatically generates derived ICD-10 look up 
#' tables for comorbidities, infection indications, and the \code{\link{ccs}}.
#' @param conn a database connection
#' @param diagnoses_data a data frame validated with 
#' \code{\link{validate_inpatient_diagnoses}()}
#' @param diagnoses_lookup a data frame containing an ICD-10 reference 
#' lookup table
#' @param overwrite if \code{TRUE} (the default), will overwrite any existing
#' \code{inpatient_diagnoses} database table
#' @seealso \code{\link{validate_inpatient_episodes}()}, 
#' \code{\link{map_infections_abx_indications}()},
#' \code{\link{map_charlson_comorbidities}()},
#' \code{\link{map_ICD10_CCS}()},
#' \code{\link{map_ICD10_CCSR}()}
#' @import methods
#' @export
load_inpatient_diagnoses <- function(conn, diagnoses_data,
                                     diagnoses_lookup, overwrite = FALSE) {
   
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
  diagnoses_data <- .format_str_time_sqlite.tbl_df(conn, diagnoses_data)

  load_errors <- try({
    dplyr::copy_to(
      dest = conn, name = "inpatient_diagnoses", 
      df = dplyr::tibble(diagnoses_data), overwrite = overwrite, temporary = FALSE,
      indexes = list("patient_id", "spell_id", "episode_number", "icd_code"))
    dplyr::copy_to(
      dest = conn, name = "reference_icd", 
      df = dplyr::tibble(diagnoses_lookup), overwrite = overwrite, temporary = FALSE,
      indexes = list("icd_code"))
    dplyr::copy_to(
      dest = conn, name = "reference_icd_comorbidity", 
      df = dplyr::tibble(reference_icd_comorbidity), overwrite = overwrite, temporary = FALSE,
      indexes = list("icd_code", "comorb", "comorb_group"))
    dplyr::copy_to(
      dest = conn, name = "reference_icd_infections", 
      df = dplyr::tibble(reference_icd_infections), overwrite = overwrite, temporary = FALSE,
      indexes = list("icd_code"))
    dplyr::copy_to(
      dest = conn, name = "reference_icd_ccs", 
      df = dplyr::tibble(reference_icd_ccs), overwrite = overwrite, temporary = FALSE,
      indexes = list("icd_code", "ccs_cat_code", "ccs_L1_code", "ccs_L2_code"))
    dplyr::copy_to(
      dest = conn, name = "reference_icd_ccsr", 
      df = dplyr::tibble(reference_icd_ccsr), overwrite = overwrite, temporary = FALSE,
      indexes = list("icd_code", "ccsr_body_system_code", "ccsr_cat_code"))
  })
  
  if ( is(load_errors, "try-error") ) {
    stop(load_errors)
  }
}

#' Load episodes of care records into the warehouse
#'
#' @param conn a database connection
#' @param patients_data a data frame validated with
#' \code{\link{validate_inpatient_episodes}()}
#' @param episodes_data a data frame validated with 
#' \code{\link{validate_inpatient_episodes}()}
#' @param wards_data a data frame validated with  
#' \code{\link{validate_inpatient_episodes}()}
#' @param overwrite if \code{TRUE} (the default), will overwrite any existing
#' \code{inpatient_episodes} database table
#' @export
load_inpatient_episodes <- function(conn, 
                                    patients_data,
                                    episodes_data, 
                                    wards_data = NULL, 
                                    overwrite = TRUE) {
  
  patients_data <- dplyr::arrange(patients_data, 
                                  patient_id)
  patients_data <- patients_data[
    , c(
      "patient_id",
      colnames(patients_data)[!colnames(patients_data) == "patient_id"]
    )
  ]

  patients_data <- .format_str_time_sqlite.tbl_df(conn, patients_data)

  episodes_data <- dplyr::arrange(episodes_data, 
                                  patient_id, episode_start)
  first_column_names_episodes <- .inpatient_episodes_variables()[["variable_name"]]
  first_column_names_episodes <- c(
    first_column_names_episodes[first_column_names_episodes %in% names(episodes_data)]
  )
  episodes_data <- arrange_variables(
    episodes_data, 
    first_column_names = first_column_names_episodes) 
  
  episodes_data <- .format_str_time_sqlite.tbl_df(conn, episodes_data)
  
  dplyr::copy_to(
    dest = conn, 
    name = "patients",
    df = dplyr::tibble(patients_data),
    temporary = FALSE,
    overwrite = overwrite,
    indexes = list("patient_id")
    )
  
  dplyr::copy_to(
    dest = conn, 
    name = "inpatient_episodes",
    df = dplyr::tibble(episodes_data),
    temporary = FALSE,
    overwrite = overwrite,
    indexes = list(
      "patient_id", 
      "spell_id",
      "admission_date",
      "discharge_date",
      "episode_number",
      "episode_start",
      "episode_end",
      "consultant_code",
      "main_specialty_code"
    ))
  .compute_bed_days(conn = conn)
  
  if(!is.null(wards_data)) {
    wards_data <- dplyr::arrange(wards_data,
                                 patient_id, ward_start)
    first_column_names_wards <- .inpatient_wards_variables()[["variable_name"]]
    first_column_names_wards <- c(
      first_column_names_wards[first_column_names_wards %in% names(wards_data)]
    )
    wards_data <- arrange_variables(
      wards_data, 
      first_column_names = first_column_names_wards)   
    
    wards_data <- .format_str_time_sqlite.tbl_df(conn, wards_data)

    load_errors <- try({
      dplyr::copy_to(
        dest = conn, 
        name = "inpatient_ward_movements",
        df = dplyr::tibble(wards_data),
        temporary = FALSE,
        overwrite = overwrite,
        indexes = list(
          "patient_id", 
          "spell_id",
          "ward_start",
          "ward_end"
        )
      )
    })
    
    if ( is(load_errors, "try-error") ) {
      stop(load_errors)
    }
  }
}



#' Load observations & investigations into the warehouse
#' 
#' @param conn a database connection
#' @param investigations_data a data frame validated with 
#' \code{\link{validate_investigations}()}
#' @param overwrite if \code{TRUE} (the default), will overwrite any existing
#' \code{inpatient_investigations} database table
#' @seealso \code{\link{validate_investigations}()}
#' @export
load_inpatient_investigations <- function(conn, investigations_data, overwrite = TRUE) {
  
  first_variables <- .inpatient_investigations_variables()$variable_name
  first_variables <- first_variables[first_variables %in% colnames(investigations_data)]
  
  investigations_data <- arrange_variables(
    data = investigations_data,
    first_column_names = first_variables)
  
  investigations_data <- .format_str_time_sqlite.tbl_df(conn, investigations_data)
  
  load_errors <- try({
    dplyr::copy_to(
      dest = conn, 
      name = "inpatient_investigations", 
      df = dplyr::tibble(investigations_data), 
      overwrite = overwrite, 
      temporary = FALSE,
      indexes = list("observation_code", 
                     "observation_code_system",
                     "patient_id", 
                     "observation_datetime",
                     "status"))
  })

  if ( is(load_errors, "try-error") ) {
    stop(load_errors)
  } 
}


#' Load microbiology cultures and susceptibilities into the warehouse
#' 
#' @param conn a database connection
#' @param specimens a data frame with one row per specimen sent
#' to laboratory and
#' validated with \code{\link{validate_microbiology}()}
#' @param isolates a data frame with one row per microorganism
#' isolated from the laboratory specimen and
#' validated with \code{\link{validate_microbiology}()}
#' @param susceptibilities a data frame with one row per susceptibility 
#' validated with \code{\link{validate_microbiology}()}
#' @param overwrite if \code{TRUE} (the default), will overwrite any existing
#' microbiology database table
#' @seealso \code{\link{validate_investigations}()}
#' @export
load_inpatient_microbiology <- function(conn, 
                                        specimens, 
                                        isolates, 
                                        susceptibilities, 
                                        overwrite = TRUE) {
  
  first_variables <- .inpatient_microbiology_variables()
  first_variables$specimens <- first_variables$specimens$variable_name
  first_variables$isolates <- first_variables$isolates$variable_name
  first_variables$susceptibilities <- first_variables$susceptibilities$variable_name
  
  first_variables$specimens <- first_variables$specimens[
    first_variables$specimens %in% colnames(specimens)]
  first_variables$isolates <- first_variables$isolates[
    first_variables$isolates %in% colnames(isolates)]
  first_variables$susceptibilities <- first_variables$susceptibilities[
    first_variables$susceptibilities %in% colnames(susceptibilities)]
  
  specimens <- arrange_variables(
    data = specimens,
    first_column_names = first_variables$specimens)
  isolates <- arrange_variables(
    data = isolates,
    first_column_names = first_variables$isolates)
  susceptibilities <- arrange_variables(
    data = susceptibilities,
    first_column_names = first_variables$susceptibilities)
  
  specimens <- .format_str_time_sqlite.tbl_df(conn, specimens)
  isolates <- .format_str_time_sqlite.tbl_df(conn, isolates)
  susceptibilities <- .format_str_time_sqlite.tbl_df(conn, susceptibilities)

  load_errors <- try({
    dplyr::copy_to(
      dest = conn,
      name = "microbiology_specimens", 
      df = dplyr::tibble(specimens), 
      overwrite = overwrite, 
      temporary = FALSE,
      indexes = list("patient_id", "specimen_id", "status",
                     "specimen_datetime", "specimen_type_code",
                     "specimen_type_name"))
    dplyr::copy_to(
      dest = conn,
      name = "microbiology_isolates", 
      df = dplyr::tibble(isolates), 
      overwrite = overwrite, 
      temporary = FALSE,
      indexes = list("patient_id", "isolate_id", 
                     "specimen_id", "organism_code"))
    dplyr::copy_to(
      dest = conn,
      name = "microbiology_susceptibilities", 
      df = dplyr::tibble(susceptibilities), 
      overwrite = overwrite, 
      temporary = FALSE,
      indexes = list("patient_id", "isolate_id", "specimen_id", 
                     "organism_code", "agent_code"))
  })
  
  if ( is(load_errors, "try-error") ) {
    stop(load_errors)
  }
}

# Medication records ------------------------------------------------------


#' Set parameters for building combination therapy identifiers and
#' therapy episode identifiers
#'
#' @description Set parameters controlling how prescriptions are 
#' linked into combinations and therapy episodes 
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
#' @seealso \code{\link[Ramses]{create_therapy_episodes}()}. For more details please consult 
#' \code{vignette("therapy-episodes", package = "Ramses")}
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
  
  argument_length <- vapply(list(max_continuation_gap,
                                 max_combination_authoring_gap,
                                 max_combination_start_gap), 
                            length,
                            FUN.VALUE = double(1)) != 1
  
  if(any(argument_length)) {
    stop(simpleError("Parameters must have length == 1"))
  }
  
  argument_negative <- vapply(list(max_continuation_gap,
                                   max_combination_authoring_gap,
                                   max_combination_start_gap), 
                              sign,
                              FUN.VALUE = double(1)) == -1
  
  if(any(argument_negative)) {
    stop(simpleError("Parameters must be >= 0"))
  }
  
  list(max_combination_authoring_gap = as.integer(max_combination_authoring_gap),
       max_combination_start_gap = as.integer(max_combination_start_gap),
       max_continuation_gap = as.integer(max_continuation_gap))
}



#' Load drug prescription records into the warehouse
#'
#' @param conn a database connection
#' @param prescriptions a data frame of prescriptions passing the 
#' \code{\link{validate_prescriptions}()} function. Note: if variables 
#' \code{therapy_id} or \code{combination_id} are provided, they will be preserved
#' as they are instead of being populated by Ramses
#' @param administrations a data frame of drug administrations passing the 
#' \code{\link{validate_administrations}()} function
#' @param overwrite if \code{TRUE} (the default), will overwrite existing 
#' medication tables
#' @param transitive_closure_controls parameters controlling (see 
#' \code{\link{transitive_closure_control}()})
#' @param silent if \code{TRUE}, the progress bar will be hidden. The default is 
#' \code{FALSE}.
#' @rdname load_medications
#' @seealso \code{\link[Ramses]{create_therapy_episodes}()}
#' @export
load_medications <- function(
  conn, prescriptions, administrations = NULL, overwrite = FALSE,
  transitive_closure_controls = transitive_closure_control(),
  silent = FALSE) {
  
  load_errors <- try({
    .load_prescriptions(
      conn = conn, 
      prescriptions = prescriptions, 
      overwrite = overwrite,
      transitive_closure_controls = transitive_closure_controls,
      silent = silent)
    if(!is.null(administrations)) {
      .load_administrations(
        conn = conn, 
        administrations = administrations, 
        overwrite = overwrite)
    }
  })
  
  if( is(load_errors, "try-error") ) {
    stop(load_errors)
  }
}

.load_prescriptions <- function(
  conn, prescriptions, overwrite, 
  transitive_closure_controls, silent) {
  
  create_therapy_id <- !exists("therapy_id", prescriptions)
  create_combination_id <- !exists("combination_id", prescriptions)
  
  prescriptions <- dplyr::arrange(prescriptions, patient_id, prescription_start)
  prescriptions$id <- seq_len(nrow(prescriptions))
  prescriptions <- .format_str_time_sqlite.tbl_df(conn, prescriptions)
  
  if (is(conn, "PqConnection")) {
    if( !is(prescriptions$authoring_date, "POSIXct") ){
      stop("prescriptions$authoring_date must be of class POSIXct")
    }
    if( !is(prescriptions$prescription_start, "POSIXct") ){
      stop("prescriptions$prescription_start must be of class POSIXct")
    }
    if( !is(prescriptions$prescription_end, "POSIXct") ){
      stop("prescriptions$prescription_end must be of class POSIXct")
    }
  }
  
  prescriptions$therapy_rank <- NA_integer_
  
  if(create_combination_id) prescriptions$combination_id <- NA_character_
  if(create_therapy_id) {
    if (is.character(prescriptions$prescription_id)) {
      prescriptions$therapy_id <- NA_character_
    } else {
      prescriptions$therapy_id <- NA_integer_
    }
  }
  
  first_column_names <- .drug_prescriptions_variables()[["variable_name"]]
  first_column_names <- c(
    "id",
    first_column_names[first_column_names %in% names(prescriptions)]
  )
  prescriptions <- arrange_variables(
    prescriptions, 
    first_column_names = first_column_names) 
  
  dplyr::copy_to(
    dest = conn,
    name = "drug_prescriptions",
    df = dplyr::tibble(prescriptions),
    temporary = FALSE,
    overwrite = overwrite,
    indexes = list(
      "id",
      "patient_id", 
      "prescription_id",
      "combination_id",
      "therapy_id",
      "drug_code",
      "antiinfective_type",
      "ATC_code",
      "ATC_route",
      "authoring_date",
      "prescription_start", 
      "prescription_end"
    )
  )
  
  if (create_therapy_id | create_combination_id) {
    create_therapy_episodes(
      conn,
      transitive_closure_controls = transitive_closure_controls,
      silent = silent
    )
  } else {
    .create_table_drug_therapy_episodes(conn = conn)
  }
  
  TRUE
}


#' Create table `drug_prescriptions_edges`
#'
#' @description Create or reoverwrite the database table 
#' `drug_prescriptions_edges` and populate by drawing edges between
#' prescriptions in the `drug_prescriptions` table based on 
#' patterns of overlap determined by `transitive_closure_controls`
#' @param conn a database connection
#' @param transitive_closure_controls parameters controlling the creation
#' of edges between prescriptions
#' @return NULL
#' @noRd
.create_table_drug_prescriptions_edges <- function(conn, 
                                                   transitive_closure_controls) {
  
  .remove_db_tables(conn, "drug_prescriptions_edges")
  
  if( is(conn, "SQLiteConnection") ) {
    statement_edges <- .read_sql_syntax("drug_prescriptions_edges_SQLite.sql")
  } else if ( is(conn, "PqConnection") ) {
    statement_edges <- .read_sql_syntax("drug_prescriptions_edges_PostgreSQL.sql")
  } else {
    .throw_error_method_not_implemented(".create_table_drug_prescriptions_edges()", class(conn))
  }
  
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
#' @param silent if \code{TRUE}, the progress bar will be hidden
#' @noRd
.create_therapy_id <- function(conn, silent) {
  
  edges_table <- dplyr::select(tbl(conn, "drug_prescriptions_edges"),
                               from_id, to_id)  
  
  if(!silent) message("Transitive closure of therapy episodes beginning...\n")
  therapy_grps <- .run_transitive_closure(edges_table)
  
  therapy_grps <- therapy_grps %>% 
    dplyr::left_join(
      dplyr::select(tbl(conn, "drug_prescriptions"), 
                    id, prescription_id, prescription_start, drug_name), 
      by = c("id" = "id")
    ) %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(therapy_rank = dplyr::row_number(
      paste0(prescription_start, drug_name))) %>% 
    dplyr::ungroup() %>% 
    dplyr::compute()
  
  th_ids <- therapy_grps %>% 
    dplyr::filter(therapy_rank == 1) %>% 
    dplyr::mutate(therapy_id = prescription_id) %>% 
    dplyr::select(grp, therapy_id) %>% 
    dplyr::compute()
  
  therapy_grps <- dplyr::left_join(
    therapy_grps,
    th_ids,  
    by = c("grp" = "grp")) %>% 
    dplyr::distinct(id, therapy_id, therapy_rank) %>% 
    dplyr::compute(name = "ramses_tc_therapy", temporary = FALSE)
  
  therapy_grps
}


#' Populate `drug_prescription.combination_id`
#'
#' @param conn a data source with a table `drug_prescriptions_edges`
#' already populated
#' @param silent if \code{TRUE}, the progress bar will be hidden
#' @noRd
.create_combination_id <- function(conn, silent) {
  
  edges_table <- tbl(conn, "drug_prescriptions_edges") %>% 
    dplyr::filter(edge_type == "combination") %>% 
    dplyr::select(from_id, to_id) 
  
  if(!silent) message("Transitive closure of therapy combinations beginning...\n")
  therapy_grps <- .run_transitive_closure(edges_table)
  
  therapy_grps <- therapy_grps %>% 
    dplyr::left_join(
      dplyr::select(tbl(conn, "drug_prescriptions"), 
                    id, prescription_id), 
      by = "id"
    ) %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(combination_id = min(prescription_id, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(id, combination_id) %>% 
    dplyr::compute(name = "ramses_tc_combination", temporary = FALSE)
  
  therapy_grps
}



#' Recreate and populate `drug_therapy_episodes`
#'
#' @param conn a data source with a table `drug_prescriptions_edges`
#' already populated
#' @noRd
.create_table_drug_therapy_episodes <- function(conn) {
  
  .remove_db_tables(conn = conn, "drug_therapy_episodes")
  
  forget <- tbl(conn, "drug_prescriptions") %>%
    dplyr::group_by(patient_id, therapy_id) %>% 
    dplyr::summarise(therapy_start = min(prescription_start, na.rm = TRUE),
                     therapy_end = max(prescription_end, na.rm = TRUE)) %>% 
    dplyr::compute(name = "drug_therapy_episodes", temporary = FALSE)
  
  dplyr::db_create_index(
    con = conn, 
    table = "drug_therapy_episodes",
    columns = "patient_id")
  dplyr::db_create_index(
    con = conn, 
    table = "drug_therapy_episodes",
    columns = "therapy_id")
}


#' Re-create therapy episodes and therapy combinations
#'
#' @description This function should be used to recreate therapy episodes
#' and therapy combinations, for example in order to try new 
#' \code{transitive_closure_controls} parameters.
#' 
#' @param conn a connection to a database containing a
#' \code{drug_prescriptions} table previously created with 
#' \code{\link{load_medications}()})
#' @param transitive_closure_controls parameters controlling (see 
#' \code{\link{transitive_closure_control}()})
#' @param silent if \code{TRUE}, the progress bar will be hidden. The default is 
#' \code{FALSE}.
#' 
#' @section Overview:
#' 
#' Electronic prescribing and administration systems do not usually 
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
#' @section Operations:
#' 
#' This function performs the following operations:
#' \enumerate{
#'    \item verify that a valid \code{drug_prescriptions} table exists
#'    \item delete any existing \code{drug_prescription_edges} and
#'    \code{drug_therapy_episodes} table
#'    \item recreate the \code{drug_prescription_edges} table
#'    \item overwrite \code{therapy_id} and \code{combination_id}
#'    fields in \code{drug_prescriptions} table
#'    \item recreate the \code{drug_therapy_episodes} table
#' }
#' @seealso For more details please consult 
#' \code{vignette("therapy-episodes", package = "Ramses")}
#' @rdname create_therapy_episodes
#' @export
create_therapy_episodes <- function(
  conn,
  transitive_closure_controls = transitive_closure_control(),
  silent = FALSE
) {
  if( !DBI::dbExistsTable(conn, "drug_prescriptions") ){
    stop("No `drug_prescriptions` table found on `conn`")
  }
  .remove_db_tables(conn = conn, c("ramses_tc_combination", 
                                   "ramses_tc_therapy",
                                   "drug_prescriptions_new"))
  
  .create_table_drug_prescriptions_edges(
    conn = conn, 
    transitive_closure_controls = transitive_closure_controls
  )
  
  therapy_episode_ids <- .create_therapy_id(conn = conn, silent = silent)
  therapy_combination_ids <- .create_combination_id(conn = conn, silent = silent)
  
  new_drug_prescriptions_tbl <- tbl(conn, "drug_prescriptions") %>% 
    dplyr::select(-therapy_id, -therapy_rank) %>%
    dplyr::left_join(therapy_episode_ids, by = "id") %>% 
    dplyr::mutate(
      therapy_id = dplyr::if_else(
        is.na(therapy_id) & 
          !prescription_status %in% c('cancelled', 'draft', 'entered-in-error'),
        prescription_id,
        therapy_id),
      therapy_rank = dplyr::if_else(
        is.na(therapy_id) & 
          is.na(therapy_rank) &
          !prescription_status %in% c('cancelled', 'draft', 'entered-in-error'),
        1L,
        therapy_rank
      )
    ) %>% 
    dplyr::compute()
  
  new_drug_prescriptions_tbl <- new_drug_prescriptions_tbl %>% 
    dplyr::select(-combination_id) %>%
    dplyr::left_join(therapy_combination_ids, by = "id") %>% 
    dplyr::compute()
  
  first_column_names <- .drug_prescriptions_variables()[["variable_name"]]
  first_column_names <- c(
    "id",
    first_column_names[first_column_names %in% colnames(new_drug_prescriptions_tbl)]
  )
  new_drug_prescriptions_tbl <- arrange_variables(
    new_drug_prescriptions_tbl, 
    first_column_names = first_column_names)  %>% 
    compute(name = "drug_prescriptions_new", temporary = FALSE)
  
  .remove_db_tables(conn = conn, c("ramses_tc_combination", 
                                   "ramses_tc_therapy",
                                   "drug_prescriptions"))
  DBI::dbExecute(
    conn = conn, 
    "ALTER TABLE drug_prescriptions_new RENAME TO drug_prescriptions;"
  )
  
  .create_table_drug_therapy_episodes(conn = conn)
}


.load_administrations <- function(
  conn, 
  administrations, 
  overwrite
) {
  
  administrations <- dplyr::arrange(administrations, 
                                    patient_id, administration_date)
  administrations$id <- seq_len(nrow(administrations))
  
  first_column_names <- .drug_administrations_variables()[["variable_name"]]
  first_column_names <- c(
    "id",
    first_column_names[first_column_names %in% names(administrations)]
  )
  administrations <- arrange_variables(
    administrations, 
    first_column_names = first_column_names
  ) 
  administrations <- .format_str_time_sqlite.tbl_df(conn, administrations)
  
  load_output <- try({
    dplyr::copy_to(
      dest = conn,
      name = "drug_administrations",
      df = dplyr::tibble(administrations),
      temporary = FALSE,
      overwrite = overwrite,
      indexes = list(
        "id",
        "patient_id", 
        "administration_id",
        "drug_code",
        "ATC_code",
        "ATC_route",
        "administration_date"
      )
    )
  })
  
  if(is(load_output, "try-error")) {
    stop(load_output)
  }
}



# Database management -----------------------------------------------------


#' Create or connect to a local SQLite database
#'
#' @description Create a local database in memory or on disk using \code{\link[RSQLite]{SQLite}()}. 
#' This is the ideal method to experiment on a small scale or for testing purposes.
#'
#' @details This function creates a database on disk at the desired path. The database and
#' its content will persist after it is disconnected.
#'     
#' @section Warning:     
#' This method does not provide any encryption or password protection. You should only use this
#' method with mock data unless you operate within a secure data enclave.
#'
#' @param file A file path to an existing or new database file with a \code{.sqlite} extension.
#' @return A database connection object of class \code{SQLiteConnection}.
#' @seealso The dbplyr website provides excellent guidance on how to connect to databases: 
#' \url{https://db.rstudio.com/getting-started/connect-to-database}
#' @importFrom DBI dbConnect dbDisconnect
#' @export
#'
#' @examples
#'     # Create database and load data
#'     con <- connect_local_database("ramses-db.sqlite")
#'     
#'     dplyr::copy_to(dest = con, df = reference_aware, name = "reference_aware", 
#'                    overwrite = FALSE, temporary = FALSE)      
#'     
#'     # Close connection to database
#'     DBI::dbDisconnect(con)
#'     
#'     # Connect to the database again and show data
#'     con <- connect_local_database("ramses-db.sqlite")
#'     dplyr::tbl(con, "reference_aware")
#'     DBI::dbDisconnect(con)
#'     file.remove("ramses-db.sqlite")
connect_local_database <- function(file) {
  if(!file.exists(file)){
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    .build_tally_table(con)
    message(paste0("SQLite database created in \n", con@dbname, 
                   "\nPlease do not use real patient data."))
  } else {
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    message(paste0("Connected to ", con@dbname, 
                   "\nPlease do not use real patient data."))
  }
  
  con
}



#' Create a mock database for training/demonstration purposes
#'
#' @description Create a local database on disk using 
#' \code{\link[RSQLite]{SQLite}()} and load synthetic data ready for analysis.
#' @details This function creates a database on disk at the desired path. 
#' The database and its content will persist after it is disconnected.
#'
#' @param file A file path to an existing or new database file with a
#'  ".sqlite" extension.
#' @param silent if \code{TRUE}, the progress bar will be hidden. The default is 
#' \code{FALSE}.
#' @return An object of class \code{SQLiteConnection}.
#' @seealso The dbplyr website provides excellent guidance on how to connect to databases: 
#' \url{https://db.rstudio.com/getting-started/connect-to-database}
#' @importFrom DBI dbConnect dbDisconnect
#' @export
create_mock_database <- function(file, 
                                 silent = FALSE) {
  
  stopifnot(is.logical(silent))
  if(!silent) {
    progress_bar <- progress::progress_bar$new(
      format = "  building mock db [:bar] :percent",
      total = 8)
    progress_bar$tick(0)
  }
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), file)
  
  .build_tally_table(mock_db)
  dplyr::copy_to(
    dest = mock_db,
    df = dplyr::filter(Ramses::reference_aware, version == "England" & year == "2019"),
    name = "reference_aware",
    temporary = FALSE,
    overwrite = TRUE
  )
  if(!silent) progress_bar$tick()
  
  drug_data <- .prepare_example_drug_records()
  inpatient_data <- .prepare_example_inpatient_records()
  icd10cm <- download_icd10cm(silent = TRUE)
  if(!silent) progress_bar$tick()
  
  load_medications(
    conn = mock_db, 
    prescriptions = drug_data$drug_rx,
    administrations = drug_data$drug_admins,
    overwrite = TRUE, 
    silent = TRUE
  )
  if(!silent) progress_bar$tick()
  load_inpatient_episodes(
    conn = mock_db, 
    patients_data = inpatient_data$patients,
    episodes_data = inpatient_data$episodes,
    wards_data = inpatient_data$ward_movements,
    overwrite = TRUE
  ) 
  if(!silent) progress_bar$tick()
  suppressWarnings(
    load_inpatient_diagnoses(
      conn = mock_db,
      diagnoses_data = inpatient_data$diagnoses,
      diagnoses_lookup = icd10cm,
      overwrite = TRUE
    )
  )
  if(!silent) progress_bar$tick()
  load_inpatient_investigations(
    conn = mock_db,
    investigations_data = inpatient_data$investigations,
    overwrite = TRUE
  )
  if(!silent) progress_bar$tick()
  load_inpatient_microbiology(
    conn = mock_db,
    specimens = inpatient_data$micro$specimens, 
    isolates = inpatient_data$micro$isolates,
    susceptibilities = inpatient_data$micro$susceptibilities,
    overwrite = TRUE
  )
  if(!silent) progress_bar$tick()
  bridge_tables(conn = mock_db, 
                overwrite = TRUE)
  if(!silent) progress_bar$tick()
  
  return(mock_db)
}

.build_tally_table <- function(conn) {
  # Build table to use in joins to create therapy tables
  dplyr::copy_to(conn, 
                 name = "ramses_tally",
                 df = data.frame(t = 0:20000),
                 temporary = FALSE,
                 overwrite = TRUE)
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
  statement <- lapply(statement, gsub, pattern = "--.*", replacement = "")
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
#' @param string character vector produced by \code{\link{readLines}()}
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
    if(DBI::dbExistsTable(conn = conn,
                          name = i)) {
      DBI::dbRemoveTable(conn = conn,
                         name = i)
    }
  }
  
  NULL
}

.create_ramses_tc_graphs <- function(conn){
  UseMethod(".create_ramses_tc_graphs")
}

.create_ramses_tc_graphs.SQLiteConnection <- function(conn){
  .remove_db_tables(conn, "ramses_tc_group")
  job <- .split_sql_batch(.read_sql_syntax("create_ramses_tc_group_SQLite.sql"))
  for(i in seq_along(job)) {
    DBI::dbExecute(conn, job[i])
  }
}

.create_ramses_tc_graphs.PqConnection <- function(conn){
  .remove_db_tables(conn, "ramses_tc_group")
  job <- .split_sql_batch(.read_sql_syntax("create_ramses_tc_group_SQLite.sql"))
  for(i in seq_along(job)) {
    DBI::dbExecute(conn, job[i])
  }
}

.nrow_sql_table <- function(conn, table){
  nrow <- tbl(conn, table) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::collect()
  
  nrow$n
}

 

#' Run transitive closure
#'
#' @param edge_tbl_dbi a table of class `tbl_dbi` containing two columns
#' named `from_id` and `to_id`
#' @return a connection to a non-temporary table of class `tbl_dbi`
#' under the name `ramses_tc_group`.
#' @keywords internal
#' @noRd
.run_transitive_closure <- function(edge_tbl_dbi) {
  local_edges <- dplyr::collect(edge_tbl_dbi)
  stopifnot( names(local_edges) == c("from_id", "to_id") )
  
  graph_list <- igraph::groups(
    igraph::components(
      igraph::graph_from_edgelist(
        as.matrix(local_edges),
        directed = TRUE
  )))
  
  graph_list <- graph_list[
    vapply(graph_list, length, FUN.VALUE = integer(1)) > 1
  ]
  
  rames_tc_groups_dt <- data.table::data.table(
    id = graph_list,
    grp = as.integer(names(graph_list))
  )
  rames_tc_groups_dt <- rames_tc_groups_dt[
    , list(id = as.integer(unlist(id))), 
    by = grp
  ]
  
  dplyr::copy_to(
    dest = edge_tbl_dbi$src$con,
    df = dplyr::tibble(rames_tc_groups_dt[, list(id, grp, lvl = NULL)]),
    name = "ramses_tc_group",
    overwrite = TRUE
  )
}


.prepare_example_drug_records <- function() {
  
  drug_rx <- Ramses::drug_prescriptions
  drug_admins <- Ramses::drug_administrations
  
  drug_rx$antiinfective_type <- "antibacterial"
  drug_admins$antiinfective_type <- "antibacterial"
  drug_rx$drug_code <- gsub("Vancomycin protocol", "Vancomycin", drug_rx$tr_DESC)
  drug_rx$drug_code <- as.character(AMR::as.ab(drug_rx$drug_code))
  drug_rx$drug_name <- AMR::ab_name(drug_rx$drug_code)
  drug_rx$drug_group <- AMR::ab_group(drug_rx$drug_code)
  ## recoding route of administration
  drug_rx <- dplyr::mutate(
    drug_rx, 
    ATC_route = dplyr::case_when(
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
  drug_rx$ATC_code <- AMR::ab_atc(drug_rx$drug_code)
  
  # prepare DDD extraction
  compound_strength_lookup <- data.frame(list(
    drug_code = c("AMC", "AMC", "TZP", "SMX"),
    route = c("oral", "oral", "oral", "oral"),
    dose = c(625, 1.2, 4.5, 480),
    units = c("mg", "g", "g", "mg"),
    strength = c(500, 1, 4, 400),
    basis_of_strength = c("AMX", "AMX", "PIP", "SMX")
  ), stringsAsFactors = F)
  
  drug_rx <- merge(drug_rx, compound_strength_lookup, all.x = T)
  drug_rx <- drug_rx %>% 
    dplyr::mutate(strength = dplyr::if_else(is.na(strength), dose, strength),  
           basis_of_strength = dplyr::if_else(is.na(basis_of_strength),
                                       as.character(drug_code), basis_of_strength))
  
  drug_rx <- merge(drug_rx, reference_drug_frequency, by = "frequency", all.x = T)
  
  # computing the prescription DDD the reference DDD from the ATC
  drug_rx <- drug_rx %>% 
    dplyr::mutate(DDD = compute_DDDs(
      ATC_code = AMR::ab_atc(basis_of_strength),
      ATC_administration = ATC_route,
      dose = strength * daily_frequency,
      unit = units, 
      silent = TRUE),
      duration_days = dplyr::if_else(
        daily_frequency == -1,
        "one-off", 
        dplyr::if_else(
          round(difftime(prescription_end, 
                         prescription_start, 
                         units = "days")) == 1,
          "1 day", 
          paste(round(difftime(prescription_end, 
                               prescription_start, 
                               units = "days")),
                "days")
          )
        )) %>% 
    dplyr::transmute(patient_id,
              prescription_id,
              prescription_text = paste0(
                drug_name, " ", route, " ", dose, units,
                " ",  duration_days),
              drug_code,
              drug_name = drug_name,
              drug_display_name = drug_name,
              drug_group,
              ATC_code,
              ATC_route,
              antiinfective_type,
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
  drug_admins$drug_code <- gsub("Vancomycin protocol", "Vancomycin", drug_admins$tr_DESC)
  drug_admins$drug_code <- as.character(AMR::as.ab(drug_admins$drug_code))
  drug_admins$drug_name <- AMR::ab_name(drug_admins$drug_code)
  drug_admins$drug_group <- AMR::ab_group(drug_admins$drug_code)
  
  # recoding route of administration
  drug_admins <- dplyr::mutate(
    drug_admins, 
    ATC_route = dplyr::case_when(
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
  drug_admins$ATC_code <- AMR::ab_atc(drug_admins$drug_code)
  
  drug_admins <- merge(drug_admins, compound_strength_lookup, all.x = T)
  drug_admins <- drug_admins %>% 
    dplyr::mutate(
      strength = dplyr::if_else(is.na(strength), dose, strength),
      basis_of_strength = dplyr::if_else(is.na(basis_of_strength),
                                  as.character(drug_code), basis_of_strength))
  
  drug_admins <- drug_admins %>% 
    dplyr::mutate(
      DDD = compute_DDDs(
      ATC_code = AMR::ab_atc(basis_of_strength),
      ATC_administration = ATC_route,
      dose = dose,
      unit = units,
      silent = TRUE
    ))
  
  if ( utils::packageVersion("dplyr") >= "1.0.0" ) {
    drug_admins <- drug_admins %>% 
      dplyr::group_by(
        patient_id,
        drug_code,
        route,
        dose,
        units,
        administration_date) %>% 
      dplyr::mutate(administration_id = dplyr::cur_group_id()) %>% 
      dplyr::ungroup()
  } else {
    drug_admins <- drug_admins %>% 
      dplyr::group_by(
        patient_id,
        drug_code,
        route,
        dose,
        units,
        administration_date) %>% 
      dplyr::mutate(administration_id = dplyr::group_indices()) %>% 
      dplyr::ungroup()
  }
  
  drug_admins <- dplyr::transmute(drug_admins,
      patient_id,
      administration_id = as.character(administration_id),
      prescription_id,
      administration_text = paste0(
          drug_name, " ", route, " ", dose, units),
      drug_code,
      drug_name,
      drug_display_name = drug_name,
      drug_group,
      antiinfective_type,
      ATC_code,
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
  
  ip_patients <- Ramses::patients
  ip_diagnoses <- Ramses::inpatient_diagnoses
  ip_diagnoses <- dplyr::filter(ip_diagnoses, !is.na(icd_code))
  
  ip_episodes <- Ramses::inpatient_episodes
  ip_episodes <- ip_episodes %>% 
    dplyr::filter(!is.na(spell_id)) %>% 
    dplyr::group_by(spell_id) %>% 
    dplyr::mutate(last_episode_in_spell_indicator = dplyr::if_else(
      episode_number == max(episode_number),
      1, 2)) %>% 
    dplyr::ungroup()
  
  micro <- list()
  micro$raw <- Ramses::inpatient_microbiology
  micro$raw <- micro$raw %>% 
    dplyr::mutate(
      organism_code = AMR::as.mo(dplyr::if_else(
        organism_display_name == "No growth",
        NA_character_,
        organism_display_name)),
      agent_code = AMR::as.ab(agent_display_name)
    ) %>% 
    dplyr::mutate(organism_name = AMR::mo_name(organism_code),
                  agent_name = AMR::ab_name(agent_code))
  micro$raw <- micro$raw %>% 
    dplyr::mutate(specimen_type_code = dplyr::case_when(
      specimen_type_display == "Blood Culture" ~ 
        "446131002", # Blood specimen obtained for blood culture
      specimen_type_display == "Faeces" ~ 
        "119339001", # Stool specimen
      specimen_type_display == "MRSA Screen" ~ 
        "697989009", # Anterior nares swab 
      specimen_type_display == "Urine" ~ 
        "122575003", # Urine specimen
      TRUE ~ NA_character_
    )) %>% 
    dplyr::left_join(
      dplyr::transmute(reference_specimen_type,
                       specimen_type_code = conceptId,
                       specimen_type_name = pt_term),
      by = "specimen_type_code"
    )
  micro$specimens <- micro$raw %>% 
    dplyr::transmute(specimen_id,
              patient_id,
              status = "available",
              specimen_datetime,
              specimen_type_code,
              specimen_type_name,
              specimen_type_display) %>% 
    dplyr::distinct() # Removing duplicates created by multiple isolates and susceptibility testing
  
  micro$isolates <- micro$raw %>% 
    dplyr::transmute(isolate_id,
              specimen_id,
              patient_id,
              organism_code,
              organism_name,
              organism_display_name,
              isolation_datetime) %>% 
    dplyr::distinct() # Removing duplicates created by susceptibility testing
  
  micro$susceptibilities <- micro$raw %>% 
    dplyr::filter(!is.na(organism_code)) %>%  # Remove no growth
    dplyr::transmute(isolate_id,
              specimen_id,
              patient_id,
              organism_code,
              organism_name,
              organism_display_name,
              agent_code,
              agent_name,
              agent_display_name,
              rsi_code,
              concept_code = NA_character_) %>% 
    dplyr::distinct()
  micro$raw <- NULL
  list(
    patients = ip_patients,
    episodes = ip_episodes,
    diagnoses = ip_diagnoses,
    ward_movements = Ramses::inpatient_wards,
    investigations = Ramses::inpatient_investigations,
    micro = micro
  )
}


#' Format datetime and time fields as character according to SQLite specification
#'
#' @description SQLite requires a specific format for datetime strings. This 
#' function transforms all POSIXct fields of a data frame into the correct
#' character format.
#' @param conn a database connection
#' @param data_frame a data frame to format
#' @return a formatted data frame
#' @noRd
#' @seealso https://www.sqlite.org/lang_datefunc.html#time_values
.format_str_time_sqlite.tbl_df <- function(conn, data_frame) {
  if ( is(conn, "SQLiteConnection" )) {
    for(i in which(vapply(data_frame, is, class2 = "POSIXct", FUN.VALUE = logical(1)))) {
      data_frame[[i]] <- .format_str_time_sqlite.POSIXct(data_frame[[i]])
    }
    for(i in which(vapply(data_frame, is, class2 = "Date", FUN.VALUE = logical(1)))) {
      data_frame[[i]] <- as.character(data_frame[[i]])
    }
  }
  
  data_frame
}

.format_str_time_sqlite.POSIXct <- function(x) {
  paste0(
    as.character(x, format = "%Y-%m-%d %H:%M:%S"),
    substr(as.character(x, format = "%z"), 0, 3),
    ":",
    substr(as.character(x, format = "%z"), 4, 5)
  )
}

.format_id_as_character <- function(x) {
  
  idvars <- c(
    "patient_id",
    "spell_id",
    "observation_id",
    "specimen_id",
    "isolate_id",
    "prescription_id",
    "combination_id",
    "therapy_id",
    "administration_id"
  )
  idvars <- idvars[idvars %in% colnames(x)]
  
  for (i in idvars) {
    x[[i]] <- as.character(x[[i]])
  }
  
  x
}

#' Collect SQL tibble with correct date-time specifications
#' 
#' @description This wrapper function for \link[dplyr]{collect} will convert
#' relevant character columns to \code{Date} and \code{POSIXct} type when collecting 
#' SQLite tables. This addresses the absence of date and time data types
#' in SQLite. Tables from other relational database systems are note affected.
#' @param tbl a `tbl_sql` object
#' @return a `tbl_df` object
#' @seealso \url{https://www.sqlite.org/datatype3.html#date_and_time_datatype}
#' @export
collect_ramses_tbl <- function(tbl) {
  if(is(tbl, "tbl_SQLiteConnection")) {
    tbl_df <- dplyr::collect(tbl)
    DATETIME_FIELDS <- c(
      "prescription_start",
      "prescription_end",
      "authoring_date",
      "administration_date",
      "therapy_start",
      "therapy_end",
      "admission_date",
      "discharge_date",
      "episode_start",
      "episode_end",
      "ward_start",
      "ward_end",
      "t_start",
      "t_end",
      "isolation_datetime", 
      "specimen_datetime",
      "start_time",
      "end_time"
    )
    DATETIME_FIELDS <- DATETIME_FIELDS[
      which(DATETIME_FIELDS %in% colnames(tbl_df))
      ]
    for(var in DATETIME_FIELDS){
      tbl_df[[var]] <- gsub("[:]([0-9]{2})$", "\\1", tbl_df[[var]])
      tbl_df[[var]] <- as.POSIXct(tbl_df[[var]], format = "%Y-%m-%d %H:%M:%S%z")
    }
    
    DATE_FIELDS <- c(
      "date_of_birth", 
      "date_of_death"
    )
    DATE_FIELDS <- DATE_FIELDS[
      which(DATE_FIELDS %in% colnames(tbl_df))
      ]
    for(var in DATE_FIELDS){
      tbl_df[[var]] <- as.Date(tbl_df[[var]])
    }
    
    return(tbl_df)
  } else {
    dplyr::collect(tbl)
  }
}


#' Create database bridge tables
#'
#' @description Create or re-create bridge tables to facilitate linking prescribing
#' events with spells or episodes of care. Bridge tables are used when computing
#' rates of prescribing per admission or per 1,000 bed-days.
#' @param conn a database connection
#' @param overwrite if \code{TRUE}, will overwrite any existing
#' database table. The default is \code{FALSE}
#' @param silent if \code{TRUE}, the progress bar will be hidden. The default is 
#' \code{FALSE}.
#' @details 
#' \describe{
#'    \item{\code{bridge_tables()}}{Generates all bridge tables.}
#'    \item{\code{bridge_episode_prescription_overlap()}}{Links prescriptions
#'    with inpatient episodes when they were administered. The resulting 
#'    table is the natural join of \code{inpatient_episodes} and 
#'    \code{drug_prescriptions} based on matching patient identifiers 
#'    and a time overlap between prescriptions and inpatient episodes.}
#'    \item{\code{bridge_episode_prescription_initiation()}}{Links prescriptions
#'    with inpatient episodes when they were authored. The resulting table 
#'    differs from {\code{bridge_episode_prescription_overlap}}: it links prescriptions 
#'    to the episode referencing the clinical team who prescribed them, rather
#'    that episodes during which the prescription was administered. The resulting
#'    table is the natural join of \code{inpatient_episodes} and 
#'    \code{drug_prescriptions} based on matching patient identifiers 
#'    and the prescription authoring date being comprised between the episode 
#'    start and end dates.}
#'    \item{\code{bridge_spell_therapy_overlap()}}{Links therapy episodes
#'    with inpatient spells during which they were administered. The resulting 
#'    table is the natural join of \code{inpatient_episodes} and 
#'    \code{drug_therapy_episodes} based on matching patient identifiers 
#'    and a time overlap between therapy episodes and hospital stays.}
#' } 
#' \code{inpatient_episodes}
#' @return \code{TRUE} if tables were successfully created
#' @name bridge_tables
#' @export
bridge_tables <- function(conn,
                          overwrite = FALSE,
                          silent = FALSE) {
  x <- vector()
  if( !silent ) {
    progress_bar <- progress::progress_bar$new(
      format = "  building bridge tables [:bar] :percent",
      total = 3)
    progress_bar$tick(0)
  }
  x[1] <- bridge_episode_prescription_overlap(conn, overwrite)
  if( !silent ) progress_bar$tick()
  x[2] <- bridge_episode_prescription_initiation(conn, overwrite)
  if( !silent ) progress_bar$tick()
  x[3] <- bridge_spell_therapy_overlap(conn, overwrite)
  if( !silent ) progress_bar$tick()
  
  return(all(x))
}

#' @export
#' @name bridge_tables
bridge_episode_prescription_overlap <- function(conn, 
                                                overwrite = FALSE) {
  stopifnot(is.logical(overwrite))
  stopifnot(is(conn, "SQLiteConnection") | is(conn, "PqConnection"))

  tblz_episodes <- tbl(conn, "inpatient_episodes")
  tblz_prescriptions <- tbl(conn, "drug_prescriptions")
  
  tblz_bridge_episode_prescriptions_overlap <- tblz_episodes %>% 
    dplyr::inner_join(tblz_prescriptions, 
                     by = "patient_id") %>% 
    dplyr::filter(
      dplyr::between(prescription_start, episode_start, episode_end) |
        dplyr::between(prescription_end, episode_start, episode_end) |
        dplyr::between(episode_start, prescription_start, prescription_end)
    )
  
  if( is(conn, "SQLiteConnection") ) {
    tblz_bridge_episode_prescriptions_overlap <- dplyr::mutate(
      tblz_bridge_episode_prescriptions_overlap,
      DOT = dplyr::sql(
        "(strftime('%s', min(prescription_end, episode_end)) -
        strftime('%s', max(prescription_start, episode_start)))/(3600.0*24.0)"
      ),
      DDD_prescribed = dplyr::sql(
        "(strftime('%s', min(prescription_end, episode_end)) -
        strftime('%s', max(prescription_start, episode_start)))
         / (3600.0*24.0) * DDD"
      ))
  } else if( is(conn, "PqConnection") ) {
    tblz_bridge_episode_prescriptions_overlap <- dplyr::mutate(
      tblz_bridge_episode_prescriptions_overlap,
      DOT = dplyr::sql(
        "EXTRACT(EPOCH FROM (
           LEAST(prescription_end::TIMESTAMP, episode_end::TIMESTAMP) -
           GREATEST(prescription_start::TIMESTAMP, episode_start::TIMESTAMP) ))
         / ( 3600.0 * 24.0 )"
      ),
      DDD_prescribed = dplyr::sql(
        "EXTRACT(EPOCH FROM (
           LEAST(prescription_end::TIMESTAMP, episode_end::TIMESTAMP) -
           GREATEST(prescription_start::TIMESTAMP, episode_start::TIMESTAMP) ))
         / ( 3600.0 * 24.0 ) * \"DDD\""
      ))
  } else {
    .throw_error_method_not_implemented("bridge_episode_prescription_overlap()",
                                        class(conn))
  }
  
  tblz_bridge_episode_prescriptions_overlap <- dplyr::transmute(
    tblz_bridge_episode_prescriptions_overlap,
    patient_id,
    spell_id,
    episode_number,
    prescription_id,
    DOT,
    DDD_prescribed
  )
  
  if (overwrite) {
    if(DBI::dbExistsTable(conn, "bridge_episode_prescription_overlap")) {
      DBI::dbRemoveTable(conn, "bridge_episode_prescription_overlap")
      }
  }
  
  dplyr::compute(tblz_bridge_episode_prescriptions_overlap, 
                 name = "bridge_episode_prescription_overlap", 
                 temporary = FALSE)
  
  return(TRUE)
}

#' @export
#' @name bridge_tables
bridge_episode_prescription_initiation <- function(conn, 
                                                   overwrite = FALSE) {
  
  stopifnot(is.logical(overwrite))
  stopifnot(is(conn, "SQLiteConnection") | is(conn, "PqConnection"))
  
  tblz_episodes <- tbl(conn, "inpatient_episodes")
  tblz_prescriptions <- tbl(conn, "drug_prescriptions")
  
  tblz_bridge_episode_prescription_initiation <- tblz_episodes %>% 
    dplyr::inner_join(tblz_prescriptions, 
                     by = "patient_id") %>% 
    dplyr::filter(
      dplyr::between(authoring_date, episode_start, episode_end) 
    )
  
  if( is(conn, "SQLiteConnection") ) {
    tblz_bridge_episode_prescription_initiation <- dplyr::mutate(
      tblz_bridge_episode_prescription_initiation,
      DOT = dplyr::sql(
        "(strftime('%s', prescription_end) -
        strftime('%s', prescription_start))
        / (3600.0 * 24.0)"
      ),
      DDD_prescribed = dplyr::sql(
        "(strftime('%s', prescription_end) -
        strftime('%s', prescription_start))
         / (3600.0 * 24.0) * DDD")
    )
  } else if( is(conn, "PqConnection") ) {
    tblz_bridge_episode_prescription_initiation <- dplyr::mutate(
      tblz_bridge_episode_prescription_initiation,
      DOT = dplyr::sql(
        "EXTRACT(EPOCH FROM (
           prescription_end::TIMESTAMP -
           prescription_start::TIMESTAMP ))
         / ( 3600.0 * 24.0 )"
      ),
      DDD_prescribed = dplyr::sql(
        "EXTRACT(EPOCH FROM (
           prescription_end::TIMESTAMP -
           prescription_start::TIMESTAMP ))
         / ( 3600.0 * 24.0 ) * \"DDD\"")
    )
  } else {
    .throw_error_method_not_implemented("bridge_episode_prescription_initiation()",
                                        class(conn))
  }
  
  tblz_bridge_episode_prescription_initiation <- dplyr::transmute(
    tblz_bridge_episode_prescription_initiation,
    patient_id,
    spell_id,
    episode_number,
    prescription_id,
    DOT,
    DDD_prescribed
  )
  
  if (overwrite) {
    if(DBI::dbExistsTable(conn, "bridge_episode_prescription_initiation")) {
      DBI::dbRemoveTable(conn, "bridge_episode_prescription_initiation")
    }
  }
  
  silence <- dplyr::compute(tblz_bridge_episode_prescription_initiation, 
                            name = "bridge_episode_prescription_initiation", 
                            temporary = FALSE)
  
  return(TRUE)
}

#' @name bridge_tables
#' @export
bridge_spell_therapy_overlap <- function(conn, 
                                         overwrite = FALSE) {
  stopifnot(is.logical(overwrite))
  stopifnot(is(conn, "SQLiteConnection") | is(conn, "PqConnection"))
  
  tblz_spells <- tbl(conn, "inpatient_episodes") %>% 
    dplyr::distinct(patient_id, spell_id, admission_date, discharge_date)
  tblz_therapies <- tbl(conn, "drug_therapy_episodes")
  
  tblz_bridge_spell_therapy_overlap <- tblz_spells %>% 
    dplyr::inner_join(tblz_therapies, 
                      by = "patient_id") %>% 
    dplyr::filter(
      dplyr::between(therapy_start, admission_date, discharge_date) |
        dplyr::between(therapy_end, admission_date, discharge_date) |
        dplyr::between(admission_date, therapy_start, therapy_end)
    )
  
  if( is(conn, "SQLiteConnection") ) {
    tblz_bridge_spell_therapy_overlap <- dplyr::mutate(
      tblz_bridge_spell_therapy_overlap,
      LOT = dplyr::sql(
        "(strftime('%s', min(therapy_end, discharge_date)) -
        strftime('%s', max(therapy_start, admission_date)))/(3600.0*24.0)"
      )
    )
  } else if( is(conn, "PqConnection") ) {
    tblz_bridge_spell_therapy_overlap <- dplyr::mutate(
      tblz_bridge_spell_therapy_overlap,
      LOT = dplyr::sql(
        "EXTRACT(EPOCH FROM (
           LEAST(therapy_end::TIMESTAMP, discharge_date::TIMESTAMP) -
           GREATEST(therapy_start::TIMESTAMP, admission_date::TIMESTAMP) ))
         / ( 3600.0 * 24.0 )"
      )
    )
  } else {
    .throw_error_method_not_implemented("bridge_spell_therapy_overlap()",
                                        class(conn))
  }
  
  tblz_bridge_spell_therapy_overlap <- dplyr::transmute(
    tblz_bridge_spell_therapy_overlap,
    patient_id,
    spell_id,
    therapy_id,
    LOT
  )
  
  if (overwrite) {
    if(DBI::dbExistsTable(conn, "bridge_spell_therapy_overlap")) {
      DBI::dbRemoveTable(conn, "bridge_spell_therapy_overlap")
    }
  }
  
  silence <- dplyr::compute(tblz_bridge_spell_therapy_overlap, 
                            name = "bridge_spell_therapy_overlap", 
                            temporary = FALSE)
  return(TRUE)
}

#' Compute episode bed days in inpatient_episodes table
#' 
#' @description compute the bed-days of every episodes in the inpatient_episodes 
#' table. Bed-days are not rounded. A patient admitted today at 10:00 AM and 
#' discharged tomorrow at 16:00 has 1.25 bed-days.
#' @param conn a database connection
#' @noRd
.compute_bed_days <- function(conn){
UseMethod(".compute_bed_days")
}

.compute_bed_days.SQLiteConnection <- function(conn) {
  if( !("ramses_bed_days" %in% 
        DBI::dbListFields(conn, "inpatient_episodes")) ) {
    DBI::dbExecute(conn = conn,
                   statement = "ALTER TABLE inpatient_episodes
                           ADD COLUMN ramses_bed_days real;")
  }
  DBI::dbExecute(
    conn = conn,
    statement = "
    UPDATE inpatient_episodes
    SET ramses_bed_days = ( strftime('%s', episode_end) - 
    strftime('%s', episode_start) ) / ( 24.0 * 3600.0 );"
  )
}

.compute_bed_days.PqConnection <- function(conn) {
  if( !("ramses_bed_days" %in% 
        DBI::dbListFields(conn, "inpatient_episodes")) ) {
    DBI::dbExecute(conn = conn,
                   statement = "ALTER TABLE inpatient_episodes
                           ADD COLUMN ramses_bed_days real;")
  }
  DBI::dbExecute(
    conn = conn,
    statement = "
    UPDATE inpatient_episodes
    SET ramses_bed_days = EXTRACT(EPOCH FROM ( 
    episode_end::TIMESTAMP - episode_start::TIMESTAMP )) 
    / ( 24.0 * 3600.0 );"
  )
}