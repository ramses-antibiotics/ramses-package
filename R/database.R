


# Inpatient records -------------------------------------------------------


#' Load inpatient diagnosis records into the warehouse
#' 
#' @description Load records of clinical diagnoses into the 
#' warehouse along with an ICD-10 reference table (\code{`reference_icd`}). 
#' In addition, this function automatically generates derived ICD-10 look up 
#' tables for comorbidities (\code{`reference_icd_comorbidity`}), 
#' infection indications, (\code{`reference_icd_infections`}), and the \code{\link{ccs}}
#' (\code{`reference_icd_ccs`} and \code{`reference_icd_ccsr`} ).
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
#' @importFrom methods is
#' @export
load_inpatient_diagnoses <- function(conn, diagnoses_data,
                                     diagnoses_lookup, overwrite = FALSE) {
   
  if (!validate_inpatient_diagnoses(diagnoses_data, diagnoses_lookup)) {
    stop(simpleError("`diagnoses_data` and `diagnoses_lookup` must pass `validate_inpatient_diagnoses()`"))
  }
  
  reference_icd_infections <- map_infections_abx_indications(
    df = dplyr::select(diagnoses_lookup, "icd_code"), 
    icd_column = "icd_code") %>% 
    na.omit()
  
  reference_icd_comorbidity <- map_charlson_comorbidities(
    df = dplyr::select(diagnoses_lookup, "icd_code"), 
    icd_column = "icd_code") %>% 
    na.omit()
  
  reference_icd_ccsr <- map_ICD10_CCSR(
    df = dplyr::select(diagnoses_lookup, "icd_code"), 
    icd_column = "icd_code") %>% 
    na.omit()
  
  reference_icd_ccs <- map_ICD10_CCS(
    df = dplyr::select(diagnoses_lookup, "icd_code"), 
    icd_column = "icd_code") %>% 
    na.omit()
  
  diagnoses_data <- arrange_variables(
    data = diagnoses_data,
    first_column_names = c(
      "patient_id", 
      "encounter_id", 
      "episode_number",
      "icd_code",
      "diagnosis_position"
    ))

  load_errors <- try({
    dplyr::copy_to(
      dest = conn, name = "inpatient_diagnoses", 
      df = dplyr::tibble(diagnoses_data), overwrite = overwrite, temporary = FALSE,
      indexes = list("patient_id", "encounter_id", "episode_number", "icd_code"))
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
                                  .data$patient_id)
  patients_data <- patients_data[
    , c(
      "patient_id",
      colnames(patients_data)[!colnames(patients_data) == "patient_id"]
    )
  ]

  episodes_data <- episodes_data %>% 
    dplyr::mutate(ramses_bed_days = NA_real_) %>% 
    dplyr::arrange(.data$patient_id, .data$episode_start)
  
  first_column_names_episodes <- .inpatient_episodes_variables()[["variable_name"]]
  first_column_names_episodes <- c(
    first_column_names_episodes[first_column_names_episodes %in% names(episodes_data)]
  )
  episodes_data <- arrange_variables(
    episodes_data, 
    first_column_names = first_column_names_episodes) 
  
  dplyr::copy_to(
    dest = conn, 
    name = "patients",
    df = dplyr::tibble(patients_data),
    temporary = FALSE,
    overwrite = overwrite,
    indexes = list("patient_id")
    )
  
  if ("sex" %in% colnames(patients_data)) {
    dplyr::copy_to(
      dest = conn, 
      name = "dimension_sex",
      df = dplyr::arrange(dplyr::distinct(patients_data, .data$sex), .data$sex),
      temporary = FALSE,
      overwrite = overwrite
    )
  }
  
  dplyr::copy_to(
    dest = conn, 
    name = "inpatient_episodes",
    df = dplyr::tibble(episodes_data),
    temporary = FALSE,
    overwrite = overwrite,
    indexes = list(
      "patient_id", 
      "encounter_id",
      "admission_date",
      "discharge_date",
      "episode_number",
      "episode_start",
      "episode_end",
      "consultant_code",
      "main_specialty_code"
    ))
  .compute_bed_days(conn = conn)
  .update_dimension_date(
    conn = conn, 
    date_min = min(episodes_data$episode_start, na.rm = TRUE), 
    date_max = max(episodes_data$episode_end, na.rm = TRUE)
  )
  
  if(!is.null(wards_data)) {
    wards_data <- dplyr::arrange(wards_data,
                                 .data$patient_id, .data$ward_start)
    first_column_names_wards <- .inpatient_wards_variables()[["variable_name"]]
    first_column_names_wards <- c(
      "id",
      first_column_names_wards[first_column_names_wards %in% names(wards_data)]
    )
    wards_data$id <- seq_len(nrow(wards_data))
    wards_data <- arrange_variables(
      wards_data, 
      first_column_names = first_column_names_wards)   
    
    load_errors <- try({
      dplyr::copy_to(
        dest = conn, 
        name = "inpatient_ward_movements",
        df = dplyr::tibble(wards_data),
        temporary = FALSE,
        overwrite = overwrite
      )
      .create_sql_primary_key(
        conn = conn,
        table = "inpatient_ward_movements",
        field = "id"
      )
      .create_sql_index(
        conn = conn,
        table = "inpatient_ward_movements",
        fields = c(
          "patient_id", 
          "encounter_id",
          "ward_code",
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
  
  load_errors <- try({
    dplyr::copy_to(
      dest = conn, 
      name = "inpatient_investigations", 
      df = dplyr::tibble(investigations_data), 
      overwrite = overwrite, 
      temporary = FALSE)
    
    .create_sql_primary_key(
      conn = conn,
      table = "inpatient_investigations",
      field = "observation_id"
    )

    DBI::dbExecute(
      conn,
      statement = paste(
        "create index idx_inpatient_investigations_codes_num_not_null on inpatient_investigations",
        "(patient_id, observation_code_system, observation_code, (observation_value_numeric IS NOT NULL))",
        "where (NOT (observation_value_numeric IS NULL));"
      )
    )
    
    .update_dimension_date(
      conn = conn, 
      date_min = min(investigations_data$observation_datetime, na.rm = TRUE), 
      date_max = max(investigations_data$observation_datetime, na.rm = TRUE)
    )
    
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
    
    .update_dimension_date(
      conn = conn, 
      date_min = pmin(
        min(specimens$specimen_datetime, na.rm = TRUE),
        min(isolates$isolation_datetime, na.rm = TRUE)
      ), 
      date_max = pmax(
        max(specimens$specimen_datetime, na.rm = TRUE),
        max(isolates$isolation_datetime, na.rm = TRUE)
      )
    )
    
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
#' @importFrom rlang .data
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
  
  prescriptions <- dplyr::arrange(prescriptions, .data$patient_id, .data$prescription_start)
  prescriptions$id <- seq_len(nrow(prescriptions))

  if( !is(prescriptions$authoring_date, "POSIXct") ){
    stop("prescriptions$authoring_date must be of class POSIXct")
  }
  if( !is(prescriptions$prescription_start, "POSIXct") ){
    stop("prescriptions$prescription_start must be of class POSIXct")
  }
  if( !is(prescriptions$prescription_end, "POSIXct") ){
    stop("prescriptions$prescription_end must be of class POSIXct")
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
    overwrite = overwrite
  )
  
  .update_dimension_date(
    conn = conn, 
    date_min =  pmin(
      min(prescriptions$authoring_date, na.rm = TRUE), 
      min(prescriptions$prescription_end, na.rm = TRUE),
      min(prescriptions$prescription_start, na.rm = TRUE),
      na.rm = TRUE
    ),
    date_max = pmax(
      max(prescriptions$prescription_end, na.rm = TRUE),
      max(prescriptions$prescription_start, na.rm = TRUE),
      na.rm = TRUE
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
  
  .create_sql_primary_key(
    conn = conn,
    table = "drug_prescriptions",
    field = "prescription_id"
  )
  
  .create_sql_index(
    conn = conn,
    table = "drug_prescriptions",
    fields = c(
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
  
  if ( is(conn, "PqConnection") || is(conn, "duckdb_connection") ) {
    .remove_db_tables(conn, "drug_prescriptions_edges")
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
                               "from_id", "to_id")  
  
  if(!silent) message("Transitive closure of therapy episodes beginning...\n")
  therapy_grps <- .run_transitive_closure(edges_table)
  
  therapy_grps <- therapy_grps %>% 
    dplyr::left_join(
      dplyr::select(tbl(conn, "drug_prescriptions"), 
                    "id", "prescription_id", "prescription_start", "drug_name"), 
      by = c("id" = "id")
    ) %>% 
    dplyr::group_by(.data$grp) %>% 
    dplyr::mutate(therapy_rank = dplyr::row_number(
      paste0(.data$prescription_start, .data$drug_name))) %>% 
    dplyr::ungroup() %>% 
    dplyr::compute()
  
  th_ids <- therapy_grps %>% 
    dplyr::filter(.data$therapy_rank == 1) %>% 
    dplyr::mutate(therapy_id = .data$prescription_id) %>% 
    dplyr::select("grp", "therapy_id") %>% 
    dplyr::compute()
  
  therapy_grps <- dplyr::left_join(
    therapy_grps,
    th_ids,  
    by = c("grp" = "grp")) %>% 
    dplyr::distinct(.data$id, .data$therapy_id, .data$therapy_rank) %>% 
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
    dplyr::filter(.data$edge_type == "combination") %>% 
    dplyr::select("from_id", "to_id") 
  
  if(!silent) message("Transitive closure of therapy combinations beginning...\n")
  therapy_grps <- .run_transitive_closure(edges_table)
  
  therapy_grps <- therapy_grps %>% 
    dplyr::left_join(
      dplyr::select(tbl(conn, "drug_prescriptions"), 
                    "id", "prescription_id"), 
      by = "id"
    ) %>% 
    dplyr::group_by(.data$grp) %>% 
    dplyr::mutate(combination_id = min(.data$prescription_id, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(.data$id, .data$combination_id) %>% 
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
  
  if (is(conn, "duckdb_connection") || is(conn, "PqConnection")) {
    drug_therapy_episodes <- tbl(conn, "drug_prescriptions") %>%
      dplyr::collect() %>% 
      dplyr::filter(!is.na(.data$therapy_id)) %>% 
      dplyr::group_by(.data$patient_id, .data$therapy_id, .data$antiinfective_type) %>% 
      dplyr::summarise(therapy_start = min(.data$prescription_start, na.rm = TRUE),
                       therapy_end = max(.data$prescription_end, na.rm = TRUE))
    dplyr::copy_to(
      dest = conn,
      df = drug_therapy_episodes,
      name = "drug_therapy_episodes",
      temporary = FALSE
    )
  } else {
    .throw_error_method_not_implemented(".create_table_drug_therapy_episodes()", 
                                        class_name = class(conn))
  }

  .create_sql_primary_key(
    conn = conn,
    table = "drug_therapy_episodes",
    field = "therapy_id"
  )
  .create_sql_index(
    conn = conn,
    table = "drug_therapy_episodes",
    fields = c(
      "patient_id",
      "therapy_start",
      "therapy_end",
      "antiinfective_type"
    )
  )
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
#' When loading medication records, \code{\link{load_medications}}() first examines all 
#' prescriptions authored in a given hospital admission to ascertain such 
#' links between prescriptions. This involves looking at patterns of 
#' overlap and succession of prescriptions, and using a graph theory method 
#' known as 'transitive closure'.
#' 
#' Ramses links prescriptions together if:
#' \itemize{
#'   \item{prescriptions share the same \code{antiinfective_type}: 
#'   antibacterials are linked with antibacterials, antifungals with 
#'   antifungals, etc.}
#'   \item{\code{prescription_status} is *not* \code{'cancelled'},  
#'   \code{'draft'}, \code{'entered-in-error'}, or \code{'unknown'}.}
#' }
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
    dplyr::select(-tidyselect::all_of(c("therapy_id", "therapy_rank"))) %>%
    dplyr::left_join(therapy_episode_ids, by = "id") %>% 
    dplyr::mutate(
      therapy_rank = dplyr::if_else(
        is.na(.data$therapy_id) & 
          is.na(.data$therapy_rank) &
          !.data$prescription_status %in% c('unknown', 'cancelled', 'draft', 'entered-in-error'),
        1L,
        .data$therapy_rank
      ),
      therapy_id = dplyr::if_else(
        is.na(.data$therapy_id) & 
          !.data$prescription_status %in% c('unknown', 'cancelled', 'draft', 'entered-in-error'),
        .data$prescription_id,
        .data$therapy_id)
    ) %>% 
    dplyr::select(-tidyselect::all_of("combination_id")) %>%
    dplyr::left_join(therapy_combination_ids, by = "id")
  
  first_column_names <- .drug_prescriptions_variables()[["variable_name"]]
  first_column_names <- c(
    "id",
    first_column_names[first_column_names %in% colnames(new_drug_prescriptions_tbl)]
  )
  
  new_drug_prescriptions_tbl <- arrange_variables(
    new_drug_prescriptions_tbl, 
    first_column_names = first_column_names)  %>% 
    dplyr::compute()
  
  .remove_db_tables(conn = conn, table_names = "drug_prescriptions")
  
  dplyr::compute(new_drug_prescriptions_tbl,
                 name = "drug_prescriptions",
                 temporary = FALSE)
  
  .remove_db_tables(conn = conn, c("ramses_tc_combination", 
                                   "ramses_tc_therapy",
                                   "drug_prescriptions_new"))
  
  .create_table_drug_therapy_episodes(conn = conn)
}


.load_administrations <- function(
  conn, 
  administrations, 
  overwrite
) {
  
  administrations <- dplyr::arrange(administrations, 
                                    .data$patient_id, .data$administration_date)
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

  load_output <- try({
    dplyr::copy_to(
      dest = conn,
      name = "drug_administrations",
      df = dplyr::tibble(administrations),
      temporary = FALSE,
      overwrite = overwrite
    )
    
    .create_sql_primary_key(
      conn = conn,
      table = "drug_administrations",
      field = "administration_id"
    )
    
    .create_sql_index(
      conn = conn,
      table = "drug_administrations",
      fields = c(
        "id",
        "patient_id", 
        "administration_id",
        "drug_code",
        "ATC_code",
        "ATC_route",
        "administration_date"
      )
    )
    
    .update_dimension_date(
      conn = conn, 
      date_min = min(administrations$administration_date, na.rm = TRUE),
      date_max = max(administrations$administration_date, na.rm = TRUE)
    )
  })
  
  if(is(load_output, "try-error")) {
    stop(load_output)
  }
}


# Database management -----------------------------------------------------


#' Connect to (or create) a local DuckDB database
#'
#' @description Create a local database in memory or on disk using 
#' \code{\link[duckdb]{duckdb}()}. This is the ideal method to experiment 
#' on a small scale. DuckDB is a relational database similar to SQLite, 
#' with full support for date and datetime data.
#'
#' @details This function creates a database on disk at the desired path. 
#' The database and its content will persist after it is disconnected.
#'     
#' @section Warning:     
#' This method does not provide any encryption or password protection. You should only use this
#' method with mock data unless you operate within a secure data enclave.
#'
#' @param file A file path to an existing or new database file with a
#'  ".duckdb" extension.
#' @param timezone A string for the time zone in which to return data to 
#' R from the database. By default, it is set to \code{\link{Sys.timezone}()}.
#' @return A database connection object of class \code{duckdb_connection}.
#' @seealso The \code{duckdb} website provides excellent guidance on how to
#' connect to databases: 
#' \url{https://duckdb.org/docs/api/r}
#' @export
#'
#' @examples
#'     # Create database and load data
#'     con <- connect_local_database("ramses-db.duckdb")
#'     
#'     dplyr::copy_to(dest = con, df = reference_aware, name = "reference_aware", 
#'                    overwrite = FALSE, temporary = FALSE)      
#'     
#'     # Close connection to database
#'     DBI::dbDisconnect(con, shutdown=TRUE)
#'     
#'     # Connect to the database again and show data
#'     con <- connect_local_database("ramses-db.duckdb")
#'     dplyr::tbl(con, "reference_aware")
#'     DBI::dbDisconnect(con, shutdown=TRUE)
#'     file.remove("ramses-db.duckdb")
connect_local_database <- function(file, timezone = Sys.timezone()) {
  if(!file.exists(file)){
    con <- DBI::dbConnect(duckdb::duckdb(), 
                          dbdir = file, 
                          timezone_out = timezone,
                          tz_out_convert = "with")
    .build_tally_table(con)
    message(paste0("DuckDB database created in \n", con@driver@dbdir,
                   "\nPlease do not use real patient data."))
  } else {
    con <- DBI::dbConnect(duckdb::duckdb(), 
                          dbdir = file, 
                          timezone_out = Sys.timezone())
    message(paste0("Connected to local DuckDB database ", con@driver@dbdir,
                   "\nPlease do not use real patient data."))
  }
  
  con
}


#' Create a mock database for training/demonstration purposes
#'
#' @description Create a local database on disk using 
#' \code{\link[duckdb]{duckdb}()} and load it with synthetic data ready for analysis.
#' @details This function creates a database on disk at the desired path. 
#' The database and its content will persist after it is disconnected.
#'
#' @param file A file path to an existing or new database file with a
#'  ".duckdb" extension.
#' @param timezone A string for the time zone in which to return data to 
#' R from the database. By default, it is set to \code{\link{Sys.timezone}()}.
#' @param silent if \code{TRUE}, the progress bar will be hidden. The default is 
#' \code{FALSE}.
#' @return An object of class \code{duckdb_connection}.
#' @export
create_mock_database <- function(file, 
                                 timezone = Sys.timezone(),
                                 silent = FALSE) {
  
  stopifnot(is.logical(silent))
  if(!silent) {
    progress_bar <- progress::progress_bar$new(
      format = "  building mock db [:bar] :percent",
      total = 8)
    progress_bar$tick(0)
  }
  mock_db <- DBI::dbConnect(duckdb::duckdb(), 
                            dbdir = file, 
                            timezone_out = timezone,
                            tz_out_convert = "with")

  .build_tally_table(mock_db)
  dplyr::copy_to(
    dest = mock_db,
    df = dplyr::filter(Ramses::reference_aware, 
                       .data$version == "England" & .data$year == "2019"),
    name = "reference_aware",
    temporary = FALSE,
    overwrite = TRUE
  )
  if(!silent) progress_bar$tick()
  
  load_medications(
    conn = mock_db, 
    prescriptions = .ramses_mock_dataset$drug_rx,
    administrations = .ramses_mock_dataset$drug_admins,
    overwrite = TRUE, 
    silent = TRUE
  )
  if(!silent) progress_bar$tick()
  load_inpatient_episodes(
    conn = mock_db, 
    patients_data = .ramses_mock_dataset$patients,
    episodes_data = .ramses_mock_dataset$episodes,
    wards_data = Ramses::inpatient_wards,
    overwrite = TRUE
  ) 
  if(!silent) progress_bar$tick()
  suppressWarnings(
    load_inpatient_diagnoses(
      conn = mock_db,
      diagnoses_data = .ramses_mock_dataset$diagnoses,
      diagnoses_lookup = .ramses_mock_dataset$icd10cm_2020,
      overwrite = TRUE
    )
  )
  if(!silent) progress_bar$tick()
  load_inpatient_investigations(
    conn = mock_db,
    investigations_data = Ramses::inpatient_investigations,
    overwrite = TRUE
  )
  if(!silent) progress_bar$tick()
  load_inpatient_microbiology(
    conn = mock_db,
    specimens = .ramses_mock_dataset$micro$specimens, 
    isolates = .ramses_mock_dataset$micro$isolates,
    susceptibilities = .ramses_mock_dataset$micro$susceptibilities,
    overwrite = TRUE
  )
  if(!silent) progress_bar$tick()
  bridge_tables(conn = mock_db, 
                overwrite = TRUE)
  if(!silent) progress_bar$tick()
  
  return(mock_db)
}


.create_dimension_date <- function(conn) {
  # Build table to use as dimension for dates if it does not already exist
  
  if (!DBI::dbExistsTable(conn, "dimension_date")) {
    if (is(conn, "duckdb_connection")) {
      # STORED generated columns are not yet implemented in DuckDB
      statement <- gsub(
        pattern = "STORED", 
        replacement = "VIRTUAL", 
        x = .read_sql_syntax("date_dimension_PostgreSQL.sql")
      )
    } else if (is(conn, "PqConnection")) {
      statement <- .read_sql_syntax("date_dimension_PostgreSQL.sql")
    } else {
      .throw_error_method_not_implemented(".create_dimension_date()", class_name = class(conn))
    }
    
    load_errors <- try({
      DBI::dbExecute(conn, statement)
    })
    
    if ( is(load_errors, "try-error") ) {
      warning(load_errors)
      warning("The `dimension_date` table did not load successfully")
    }
  }
}

.create_dimension_age <- function(conn, overwrite = FALSE) {
  # Build table to use as dimension for age - do not overwrite by default
  stopifnot(is.logical(overwrite))
  
  if (!DBI::dbExistsTable(conn, "dimension_age") | overwrite) {
    dim_age <- dplyr::copy_to(
      dest = conn, 
      df = dplyr::tibble(age = 0L:150L),
      name = "dimension_age",
      overwrite = overwrite,
      temporary = FALSE
    )
  }
}

.update_dimension_date <- function(conn, date_min, date_max) {
  # Add dates to `reference_dimension_date` if they are not already loaded
  
  .create_dimension_date(conn)
  
  date_df <- .compute_date_dimensions(date_min, date_max) %>% 
    dplyr::anti_join(
      dplyr::tbl(conn, "dimension_date"),
      by = "date", 
      copy = TRUE
    )
  
  DBI::dbAppendTable(
    conn = conn, 
    name = "dimension_date", 
    value = date_df
  )
}


.build_tally_table <- function(conn) {
  # Build table to use in joins to create therapy tables
  if ( !DBI::dbExistsTable(conn, "ramses_tally") ) {
    DBI::dbExecute(
      conn = conn, 
      statement = "CREATE TABLE ramses_tally(t INTEGER PRIMARY KEY);"
    )
    DBI::dbWriteTable(
      conn = conn,
      name = "ramses_tally",
      value = data.frame(t = 0L:50000L), 
      append = TRUE
    )
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
  statement <- lapply(statement, gsub, pattern = "--.*", replacement = "")
  # concatenate on single line
  statement <- paste(statement, collapse = " ")
  
  statement
}

#' Get table field data type
#'
#' @param conn a database connection
#' @param table a character table name
#' @param field a character field name
#'
#' @return a variable type
#' @noRd
.sql_data_type <- function(conn, table, field = NULL) {
  stopifnot(length(table)==1)
  stopifnot(is.character(table))
  stopifnot(is.character(field) | is.null(field))
  if (!DBI::dbExistsTable(conn, table)) {
    if (table %in% c("drug_prescriptions", "drug_therapy_episodes") ) {
      help_file <- "load_medications"
    } else {
      help_file <- "load_inpatient_episodes"
    }
    stop(
      "The Ramses database must contain a valid `", table, "` table.\n",
      "Please consult ?", help_file," for help.",
      call. = FALSE
    )
  }
  x <- tbl(conn, table)
  if (!is.null(field)) {
    x <- dplyr::select(x, !!field)
  }
  x <- utils::head(x, 1) %>% 
    dplyr::collect() %>% 
    vapply(class, FUN.VALUE = character(1))
  
  x
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

#' Create primary key
#'
#' @param conn a database connection
#' @param table a character table name
#' @param field a character field name
#' @noRd
.create_sql_primary_key <- function(conn, table, field, override_index_name = NULL) {
  
  if ( is(conn, "duckdb_connection") & !is.null(override_index_name) ) {
      DBI::dbExecute(
        conn,
        statement = paste0(
          "CREATE UNIQUE INDEX ", override_index_name,
          " ON ", table, " (", field, ");"
        )
      )
    } else if ( is(conn, "duckdb_connection") & is.null(override_index_name) ) {
      DBI::dbExecute(
        conn,
        statement = paste0(
          "CREATE UNIQUE INDEX ", table, "_pk",
          " ON ", table, " (", field, ");"
        ))
    } else if( is(conn, "PqConnection") & !is.null(override_index_name) ) {
    DBI::dbExecute(
      conn,
      statement = paste0(
        "alter table ", table,
        " add constraint ", override_index_name,
        " primary key (", field,");"
      )
    )
    } else if( is(conn, "PqConnection") & is.null(override_index_name) ) {
      DBI::dbExecute(
      conn,
      statement = paste0(
        "alter table ", table,
        " add constraint ", table, "_pk ",
        "primary key (\"", field,"\");"
      ))
    } else {
      .throw_error_method_not_implemented(".create_sql_primary_key()", class_name = class(conn))
    }
}


#' Create individual indexes
#'
#' @param conn a database connection
#' @param table a character table name
#' @param fields a character vector of field names
#' @noRd
.create_sql_index <- function(conn, table, fields, override_index_name = NULL) {
  
  if(!is.null(override_index_name)) {
    DBI::dbExecute(
      conn,
      paste0("create index ", override_index_name,  
             " on ", table, " (", fields, ");")
    )
  } else {
    for (i in fields) {
      DBI::dbExecute(
        conn,
        paste0("create index idx_", table, "_", i, 
               " on ", table, " (\"", i, "\");")
      )
    }
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
  
  id <- grp <- NULL
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


.format_id_as_character <- function(x) {
  
  idvars <- c(
    "patient_id",
    "encounter_id",
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


#' Interface for SQL generate_series() function
#'
#' @param x a \code{tbl_sql} object (remote table)
#' @param start_dt character string of name of a timestamp or date field
#' @param end_dt character string of name of a timestamp or date field
#'
#' @return \code{x} with an additional variate \code{date}
#' @noRd
.sql_generate_date_series <- function(x, start_dt, end_dt) {
  stopifnot(is.character(start_dt) & length(start_dt) == 1)
  stopifnot(is.character(end_dt) & length(end_dt) == 1)
  
  start_dt <- dbplyr::sql_quote(start_dt, '"')
  end_dt <- dbplyr::sql_quote(end_dt, '"')
  
  if (is(x$src$con, "duckdb_connection")) {
    tbl_expanded <- dplyr::mutate(
      x,
      date = dplyr::sql(paste0("unlist(generate_series(", start_dt, "::DATE, ", end_dt, "::DATE, INTERVAL '1 day'))::DATE"))
    )
  } else if (is(x$src$con, "PqConnection")) {
    tbl_expanded <- dplyr::mutate(
      x,
      date = dplyr::sql(paste0("generate_series(", start_dt, "::DATE, ", end_dt, "::DATE, INTERVAL '1 day')::DATE"))
    )
  } else {
    .throw_error_method_not_implemented(".sql_generate_date_series")
  }
  
  
  tbl_expanded <- dplyr::mutate(
    tbl_expanded,
    date_weight = dplyr::sql(paste0(
      "CASE WHEN ", start_dt, "::date = date ", 
      "THEN 3600.0*24.0 - EXTRACT(epoch from CAST(", start_dt, "::TIMESTAMP as TIME))", 
      "WHEN ", end_dt, "::date = date ",
      "THEN EXTRACT(epoch FROM CAST(", end_dt,"::TIMESTAMP as TIME)) ",
      "ELSE 3600*24 END / 3600.0 / 24.0"
    ))
  )
  
  tbl_expanded
}


#' Compute episode bed days in inpatient_episodes table
#' 
#' @description compute the bed-days of every episodes in the inpatient_episodes 
#' table. Bed-days are not rounded. A patient admitted today at 10:00 AM and 
#' discharged tomorrow at 16:00 has 1.25 bed-days.
#' @param conn a database connection
#' @noRd
.compute_bed_days <- function(conn){
  
  if ( !is(conn, "PqConnection") & !is(conn, "duckdb_connection") ) {
    .throw_error_method_not_implemented(function_name = ".compute_bed_days()", 
                                        class_name =  class(conn))
  }
  
  if( !("ramses_bed_days" %in% 
        DBI::dbListFields(conn, "inpatient_episodes")) ) {
    stop("`ramses_bed_days` is not found in table `inpatient_episodes`")
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
