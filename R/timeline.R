

#' Display an HTML antimicrobial therapy timeline
#'
#' @param x an object of class \code{Patient}, \code{Encounter}, 
#' or \code{TherapyEpisode} 
#' @param date1 (optional, only used for \code{x} of class \code{Patient}) 
#' a \code{Date} or \code{POSIXct} window minimum to 
#' focus the timeline on by default
#' @param date2 (optional, only used for \code{x} of class \code{Patient}) 
#' a \code{Date} or \code{POSIXct} window maximum to 
#' focus the timeline on by default
#' @param load_timevis_dependencies if \code{TRUE}, jQuery and Bootstrap will
#' be loaded (default is \code{FALSE}). See \link[timevis]{timevis} for detail.
#' @param ... other arguments (not in use)
#' @return a timevis object
#' @importFrom dplyr tbl
#' @importFrom magrittr %>%
#' @include objects.R
#' @rdname therapy_timeline
#' @export
setGeneric("therapy_timeline", 
           function(x, ...) standardGeneric("therapy_timeline"),
           signature = "x")

#' @rdname therapy_timeline
#' @export
setMethod(
  "therapy_timeline",
  c(x = "Patient"),
  function(x, 
           date1 = NULL, date2 = NULL,
           load_timevis_dependencies = FALSE) {
    
    stopifnot( is.null(date1) | is(date1, "Date") | is(date1, "POSIXct") )
    stopifnot( is.null(date2) | is(date2, "Date") | is(date2, "POSIXct") )
    input_patient <- x
    if( nrow(collect(input_patient)) == 0 ) {
      stop("Patient not found in the database")
    }
    
    if(!DBI::dbExistsTable(input_patient@conn, "drug_prescriptions")) {
      stop("The Ramses database must contain a valid `drug_prescriptions` table.\n",
           "Please consult ?load_medications for help.",
           call. = FALSE)
    }
    
    # Retrieve prescription records  
    medication_requests_inclusion <- tbl(input_patient@conn, "drug_prescriptions") %>%
      dplyr::filter(.data$patient_id == !!input_patient@id & 
                      !.data$prescription_status %in% c("cancelled", "draft", "in-error")) %>%
      dplyr::left_join(tbl(input_patient@conn, "drug_therapy_episodes"), 
                       by = c("patient_id", "therapy_id")) %>%
      dplyr::arrange(.data$patient_id, .data$prescription_start) %>% 
      dplyr::collect()
    
    # Calculate the date range the timeline should display by default
    
    if( !is.null(date1) && !is.null(date2) ) {
      date1 <- as.POSIXct(date1)
      date2 <- as.POSIXct(date2)
      date_window <- dplyr::filter(
        medication_requests_inclusion, 
        dplyr::between(.data$therapy_start, date1, date2) |
          dplyr::between(.data$therapy_end, date1, date2) |
          (.data$therapy_start <= date1 & date1 <= .data$therapy_end)) %>%
        dplyr::summarise(left_date = min(.data$therapy_start, na.rm = T), 
                         right_date = max(.data$therapy_end, na.rm = T))
    } else if( !is.null(date2) ) {
      date2 <- as.POSIXct(date2)
      date_window <- dplyr::filter(
        medication_requests_inclusion, 
        .data$therapy_start <= date2 ) %>%
        dplyr::summarise(left_date = min(.data$therapy_start, na.rm = T), 
                         right_date = max(.data$therapy_end, na.rm = T)) 
    } else if( !is.null(date1) ) {
      date1 <- as.POSIXct(date1)
      date_window <- dplyr::filter(
        medication_requests_inclusion, 
        .data$therapy_end >= date1 ) %>%
        dplyr::summarise(left_date = min(.data$therapy_start, na.rm = T), 
                         right_date = max(.data$therapy_end, na.rm = T)) 
    } else {
      date_window <- dplyr::summarise(
        medication_requests_inclusion, 
        left_date = min(.data$therapy_start, na.rm = T), 
        right_date = max(.data$therapy_end, na.rm = T)) 
    }
    
    .therapy_timeline_create(
      medication_requests_df = medication_requests_inclusion,
      input_patient = input_patient,
      date_window = date_window,
      load_timevis_dependencies = load_timevis_dependencies
    )
  })

#' @rdname therapy_timeline
#' @export
setMethod(
  "therapy_timeline",
  c(x = "Encounter"),
  function(x, load_timevis_dependencies = FALSE) {
    
    input_encounter <- x
    if (length(input_encounter@id) > 1) {
      input_encounter <- Encounter(x@conn, x@id[1])
      warning("`x` contains multiple encounters.\n",
              "Only the first therapy episode will be used.", call. = FALSE)
    }
    if( nrow(collect(input_encounter)) == 0 ) {
      stop("Encounter not found in the database", call. = FALSE)
    }
    
    input_patient <- Patient(input_encounter)
    if( nrow(collect(input_patient)) == 0 ) {
      stop("Patient not found in the database", call. = FALSE)
    }
    
    date_window <- collect(input_encounter) %>% 
      dplyr::distinct(left_date = .data$admission_date,
                      right_date = .data$discharge_date)
    
    if(!DBI::dbExistsTable(input_patient@conn, "drug_prescriptions")) {
      stop("The Ramses database must contain a valid `drug_prescriptions` table.\n",
           "Please consult ?load_medications for help.",
           call. = FALSE)
    }
    
    # Retrieve prescription records  
    medication_requests_inclusion <- tbl(input_patient@conn, "drug_prescriptions") %>%
      dplyr::filter(.data$patient_id == !!input_patient@id & 
                      !.data$prescription_status %in% c("cancelled", "draft", "in-error")) %>%
      dplyr::left_join(tbl(input_patient@conn, "drug_therapy_episodes"), 
                       by = c("patient_id", "therapy_id")) %>%
      dplyr::arrange(.data$patient_id, .data$prescription_start) %>% 
      dplyr::collect()
    
    .therapy_timeline_create(
      medication_requests_df = medication_requests_inclusion,
      input_patient = input_patient,
      date_window = date_window,
      load_timevis_dependencies = load_timevis_dependencies
    )
  })

#' @rdname therapy_timeline
#' @export
setMethod(
  "therapy_timeline",
  c(x = "TherapyEpisode"),
  function(x, load_timevis_dependencies = FALSE) {

    input_therapy_episode <- x
    if (length(input_therapy_episode@id) > 1) {
      input_therapy_episode <- TherapyEpisode(x@conn, x@id[1])
      warning("`x` contains multiple therapy episodes.\n",
              "Only the first therapy episode will be used.", call. = FALSE)
    }
    if( nrow(collect(input_therapy_episode)) == 0 ) {
      stop("TherapyEpisode not found in the database", call. = FALSE)
    }
    
    input_patient <- Patient(input_therapy_episode)
    if( nrow(collect(input_patient)) == 0 ) {
      stop("Patient not found in the database", call. = FALSE)
    }
    
    date_window <- collect(input_therapy_episode) %>% 
      dplyr::transmute(left_date = .data$therapy_start,
                       right_date = .data$therapy_end)
    
    # Retrieve prescription records  
    medication_requests_inclusion <- tbl(input_patient@conn, "drug_prescriptions") %>%
      dplyr::filter(.data$patient_id == !!input_patient@id & 
                      !.data$prescription_status %in% c("cancelled", "draft", "in-error")) %>%
      dplyr::left_join(tbl(input_patient@conn, "drug_therapy_episodes"), 
                       by = c("patient_id", "therapy_id")) %>%
      dplyr::arrange(.data$patient_id, .data$prescription_start) %>% 
      collect()
    
    .therapy_timeline_create(
      medication_requests_df = medication_requests_inclusion,
      input_patient = input_patient,
      date_window = date_window,
      load_timevis_dependencies = load_timevis_dependencies
    )
  })

#' @rdname therapy_timeline
#' @export
setMethod(
  "therapy_timeline",
  c(x = "MedicationRequest"),
  function(x, load_timevis_dependencies = FALSE) {
    
    input_therapy_episode <- TherapyEpisode(x)

    if( nrow(collect(input_therapy_episode)) == 0 ) {
      stop("TherapyEpisode not found in the database", call. = FALSE)
    }
    
    input_patient <- Patient(input_therapy_episode)
    if( nrow(collect(input_patient)) == 0 ) {
      stop("Patient not found in the database", call. = FALSE)
    }
    
    date_window <- collect(input_therapy_episode) %>% 
      dplyr::transmute(left_date = .data$therapy_start,
                       right_date = .data$therapy_end)
    
    # Retrieve prescription records  
    medication_requests_inclusion <- tbl(input_patient@conn, "drug_prescriptions") %>%
      dplyr::filter(.data$patient_id == !!input_patient@id & 
                      !.data$prescription_status %in% c("cancelled", "draft", "in-error")) %>%
      dplyr::left_join(tbl(input_patient@conn, "drug_therapy_episodes"), 
                       by = c("patient_id", "therapy_id")) %>%
      dplyr::arrange(.data$patient_id, .data$prescription_start) %>% 
      collect()
    
    .therapy_timeline_create(
      medication_requests_df = medication_requests_inclusion,
      input_patient = input_patient,
      date_window = date_window,
      load_timevis_dependencies = load_timevis_dependencies
    )
  })


#' Extract a data frame of drug data for display in therapy timeline
#' @param medication_requests_df a data frame of medication requests to include
#' @param input_patient a `Patient` object
#' @param date_window a singe-row data frame with `left_date` and `right_date`
#' fields 
#' @param load_timevis_dependencies a boolean indicating whether jQuery 
#' and Bootstrap should be loaded (default is \code{FALSE}). See \link[timevis]{timevis}
#' for detail
#' @return a `htmlwidget` `timevis` therapy timeline
#' @noRd
.therapy_timeline_create <- function(medication_requests_df,
                                     input_patient, 
                                     date_window,
                                     load_timevis_dependencies) {
  
  requireNamespace("timevis", quietly = TRUE)
  
  timeline_admissions <- .therapy_timeline_get_admissions(input_patient)
  
  timeline_diags <- .therapy_timeline_get_diagnoses(input_patient)
  
  timeline_drugs <- .therapy_timeline_get_drugs(medication_requests_df)
  
  timeline_death <- NULL # timeline_Rx_getdeath(input_patient)
  
  timeline_CC <- NULL # timeline_Rx_getCC(input_patient)
  
  timeline_redflags <- NULL # tbl(conn, "APC_Investigations") %>%
  #   dplyr::filter(patient_id == input_patient & (
  #     (inv == 'BPSYS' & value_num <= 90 ) |
  #       (inv == 'HR' & value_num > 130) |
  #       (inv == 'RESP' & value_num >= 25) |
  #       (inv == 'FLUOTURO' & value_num < 500)  |
  #       (inv == 'FLUOTUR' & value_num < 500) |
  #       (inv == 'UVOL' & value_num < 500) |
  #       (inv == 'SEWSSCORE' & value_num >= 4)
  #   )) %>% dplyr::collect() %>% 
  #   dplyr::transmute(id = NA, 
  #             title = paste0(inv, ": ", value, ", ", start_time),
  #             start = start_time,
  #             type = "point",
  #             group = 1,
  #             subgroup = "red-flag",
  #             className = "sepsis-redflag"
  #   )
  
  timeline_micro <- .therapy_timeline_get_micro(input_patient)
  
  timeline_groups <- data.frame(
    id = 0:3,
    content = c(
      "Admissions",
      "Clinical findings",
      "Diagnoses", 
      "Rx"
    ),
    stackSubgroups = c(FALSE, FALSE, FALSE, FALSE),
    # subgroupOrder = c("TherapyRank", NA),#, "Diagnoses", "Cultures"
    stringsAsFactors = F
  )
  
  timevis::timevis(
    dplyr::bind_rows(
      list(timeline_drugs$timeline_Rx, 
           timeline_drugs$timeline_Rx_request,
           timeline_admissions, 
           timeline_diags,
           timeline_redflags, 
           timeline_CC,
           timeline_micro, 
           timeline_death)
    ),
    groups = timeline_groups,
    options = list(stackSubgroups = FALSE,
                   stack = FALSE,
                   showCurrentTime = F,
                   tooltip = list(followMouse = T)),
    loadDependencies = load_timevis_dependencies) %>% 
    timevis::setWindow(start = date_window$left_date,
                       end = date_window$right_date) %>% 
    return()
}

#' Extract a data frame of drug data for display in therapy timeline
#' @param medication_requests_df a data frame of medication requests to include
#' @return a list of data frames
#' @noRd
.therapy_timeline_get_drugs <- function(medication_requests_df) {

  timeline_Rx <- medication_requests_df %>%
    dplyr::filter(!is.na(.data$prescription_start)) %>%
    dplyr::arrange(.data$therapy_rank) %>%
    dplyr::mutate(duration = as.numeric(.data$prescription_end - .data$prescription_start, unit = "secs")) %>% 
    dplyr::transmute(
      id = paste0("MedicationRequest:", .data$prescription_id, ":Rx"), 
      content = .data$prescription_text,
      title = paste0(
        .data$prescription_start, ", ",
        dplyr::if_else(
          .data$daily_frequency < 0, "",
          dplyr::if_else(.data$duration < 24*3600,
                         paste(trunc(.data$duration/3600), "hours, "),
                         paste(trunc(.data$duration/3600/24), "days, "))
        ),
        "Therapy episode: ", .data$therapy_id, ", position: ", .data$therapy_rank
      ), 
      start = .data$prescription_start,
      end = .data$prescription_end,
      type = dplyr::if_else(.data$daily_frequency < 0, 'point', 'range'),
      group = 3,
      subgroup = as.character(.data$therapy_rank),
      className = dplyr::case_when(
        .data$daily_frequency < 0 ~ "prescription-OOF",
        .data$prescription_context == "discharge" ~ "prescription-REGT",
        TRUE ~ "prescription-REG"
      )
    ) %>%
    as.data.frame()
  
  # This extracts data for the time of request 
  # (displaying a vertical bar before the drug administrations start)
  timeline_Rx_request <- medication_requests_df %>%
    dplyr::filter(!is.na(.data$prescription_start)) %>%
    dplyr::arrange(.data$therapy_rank) %>%
    dplyr::transmute(
      id = paste0("MedicationRequest:", .data$prescription_id, ":request"), 
      title = paste0("Time requested: ", .data$authoring_date),
      start = .data$authoring_date,
      type =  'point' ,
      group = 3,
      subgroup = as.character(.data$therapy_rank),
      className = "prescription-request"
    ) %>% 
    as.data.frame()
  
  list(
    timeline_Rx = timeline_Rx,
    timeline_Rx_request = timeline_Rx_request
  )
}


#' Extract a data frame of microbiology results for display in therapy timeline
#' @param input_patient a `Patient` object
#' @return a data frame
#' @noRd
.therapy_timeline_get_micro <- function(input_patient) {
  
  stopifnot(is(input_patient, "Patient"))
  tables_missing <- !vapply(
    X = c(
      "microbiology_specimens",
      "microbiology_isolates"
    ), 
    FUN = DBI::dbExistsTable, 
    conn = input_patient@conn, 
    FUN.VALUE = logical(1L))
  if (any(tables_missing)) {
    warning(
      "The following tables are missing from the Ramses database:\n",
      paste(paste0("`", names(tables_missing)[tables_missing], "`"), collapse = ", "), "\n",
      "Please consult ?load_microbiology for help.",
      call. = FALSE
    )
    return(data.frame())
  }
  
  micro <- tbl(input_patient@conn, "microbiology_specimens") %>%
    dplyr::filter(.data$patient_id == !!input_patient@id) %>% 
    dplyr::left_join(
      tbl(input_patient@conn, "microbiology_isolates"),
      by = c("patient_id", "specimen_id")
    ) %>% 
    dplyr::collect()
  
  if(nrow(micro) == 0){
    return(data.frame())
  } else {
    timeline_micro_reports <- micro %>%
      dplyr::mutate(className = dplyr::if_else(is.na(.data$organism_code),
                                               "micro-report-no-growth", 
                                               "micro-report")) %>% 
      dplyr::mutate(isolation_datetime = dplyr::if_else(
        is.na(.data$isolation_datetime),
        .data$specimen_datetime,
        .data$isolation_datetime
      )) %>% 
      dplyr::group_by(.data$specimen_id, .data$className) %>% 
      dplyr::summarise(
        id = paste0("Specimen ID:",
                    paste(unique(.data$specimen_id), sep = "-"), 
                    ":report"),
        start = min(.data$isolation_datetime, na.rm = TRUE),
        title = paste0(
          paste(unique(.data$specimen_type_display), collapse = ", "), "\n",
          "Organisms: ", 
          paste(unique(.data$organism_display_name), collapse = ", "), "\n",
          "Sample received: ", unique(as.Date(.data$specimen_datetime))
          ),
        type = "point",
        group = 1,
        subgroup = "micro"
      ) %>% 
      dplyr::ungroup()
    
    # TODO: reintroduce the micro-requests when we have a date of reporting
    # timeline_micro_requests <- micro %>%
    #   dplyr::group_by(specimen_id) %>%
    #   dplyr::summarise(
    #     id = unique(paste0("Specimen ID:", specimen_id, ":request")),
    #     start = mean(specimen_datetime, na.rm = T),
    #     type = "point",
    #     group = 1,
    #     subgroup = "micro",
    #     className = "micro-request"
    #   )
    # timeline_data <- dplyr::bind_rows(list(
    #   timeline_micro_requests,
    #   timeline_micro_reports))
    
    return(timeline_micro_reports)
  }
}
  

#' Extract a data frame of infection diagnoses for display in therapy timeline
#' @param input_patient a `Patient` object
#' @return a data frame
#' @noRd
.therapy_timeline_get_diagnoses <- function(input_patient) {
  
  stopifnot(is(input_patient, "Patient"))
  
  tables_missing <- !vapply(
    X = c(
      "inpatient_diagnoses",
      "reference_icd",
      "reference_icd_infections"
    ), 
    FUN = DBI::dbExistsTable, 
    conn = input_patient@conn, 
    FUN.VALUE = logical(1L))
  if (any(tables_missing)) {
    warning(
      "The following tables are missing from the Ramses database:\n",
      paste(paste0("`", names(tables_missing)[tables_missing], "`"), collapse = ", "), "\n",
      "Please consult ?load_inpatient_diagnoses for help.",
      call. = FALSE
    )
    return(data.frame())
  }
  
  # Taking ICD labels from the most recent ICD edition
  icd_lu <- tbl(input_patient@conn, "reference_icd") %>%
    dplyr::select("icd_code",
                  "icd_display",
                  "icd_description",
                  "category_description")
  
  # Consolidating diagnoses across episodes of care
  diag <- tbl(input_patient@conn, "inpatient_diagnoses") %>%
    dplyr::filter(.data$patient_id == !!input_patient@id) %>% 
    dplyr::inner_join(tbl(input_patient@conn, "reference_icd_infections"), 
                      by = c("icd_code")) %>%
    dplyr::left_join(
      dplyr::select(
        tbl(input_patient@conn, "inpatient_episodes"),
        "patient_id", 
        "encounter_id", 
        "episode_number",
        "episode_start",
        "episode_end"
      ),
      by = c("patient_id", "encounter_id", "episode_number")
    )
  
  if (
    all(c("diagnosis_start", "diagnosis_end") %in% colnames(diag))
  ) {
    diag <- diag %>% 
      dplyr::mutate(
        tl_start = dplyr::if_else(is.na(.data$diagnosis_start), 
                                  .data$episode_start,
                                  .data$diagnosis_start),
        tl_end = dplyr::if_else(is.na(.data$diagnosis_end), 
                                .data$episode_end,
                                .data$diagnosis_end)
      )
  } else {
    diag <- diag %>% 
      dplyr::mutate(
        tl_start = .data$episode_start,
        tl_end = .data$episode_end
      )
  }
  diag <- diag %>% 
    dplyr::filter( !( is.na(.data$tl_start) | is.na(.data$tl_end) ) ) %>% 
    dplyr::collect()
  
  # If no infection diagnosis is present, return empty data frame.
  # It will get ignore when feeding into timevis
  if(nrow(diag) == 0){
    return(data.frame())
  } 

  # combining diagnosis episodes that resume <24h after ending
  diag <- diag %>%
    dplyr::arrange(.data$patient_id, .data$icd_code, .data$tl_start) %>% 
    dplyr::group_by(.data$patient_id, .data$icd_code) %>% 
    dplyr::mutate(diagnosis_episode_end = dplyr::lag(.data$tl_end, 
                                                     n = 1, 
                                                     order_by = .data$tl_start)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      prim_diag = dplyr::if_else(.data$diagnosis_position == 1, 1, 0)
    ) %>% 
    dplyr::group_by(.data$patient_id, .data$icd_code, .data$prim_diag) %>% 
    dplyr::mutate(
      grp = cumsum(dplyr::case_when(
        # First diagnosis episode
        is.na(.data$diagnosis_episode_end) ~ 0L,
        # Diagnosis episode closely following identical one
        abs(as.numeric(.data$diagnosis_episode_end - .data$tl_start, units = "hours")) < 24 ~ 0L,
        # Independent (new) diagnosis episode
        TRUE ~ 1L
      ))
    ) %>% 
    # dplyr::mutate(grp = max(.data$grp)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(
      .data$patient_id, 
      .data$icd_code, 
      .data$grp, 
      .data$prim_diag, 
      .data$infection_group1_code,
      .data$infection_group1_label,
      .data$infection_group2_code,
      .data$infection_group2_label
    ) %>%
    dplyr::summarise(
      start_time = min(.data$tl_start, na.rm = TRUE),
      end_time = max(.data$tl_end, na.rm = TRUE)
    ) 
  
  diag <- dplyr::right_join(icd_lu, diag, 
                            by = "icd_code", copy = TRUE) %>% 
    dplyr::collect() %>% 
    data.table() %>% 
    unique() %>%
    dplyr::mutate(icd_text = paste0(
      dplyr::if_else(.data$prim_diag == 1, "<strong>", ""), 
      .data$icd_display, " &ndash; ", .data$icd_description,
      dplyr::if_else(.data$prim_diag == 1, "</strong>", "")))
  
  diag <- dplyr::transmute(
    diag,
    id = NA, 
    content = .data$icd_text, 
    title = paste0(.data$icd_description, dplyr::if_else(.data$prim_diag == 1, " (PRIMARY DIAGNOSIS)", "")),
    start = .data$start_time,  
    end = .data$end_time,
    group =  2,
    subgroup = .data$infection_group1_code,
    type = 'range',
    style = "background-color: #97e2f8",#paste0("background-color: #97e2f8", graph_colour), # TODO why not move this to CSS!
    className = "") %>% 
    unique() %>%
    as.data.frame()
  
  return(diag)
}



#' Extract a data frame of inpatient admissions for display in therapy timeline
#' @param input_patient a `Patient` object
#' @return a data frame
#' @noRd
.therapy_timeline_get_admissions <- function(input_patient) {
  
  stopifnot(is(input_patient, "Patient"))
  tables_missing <- !vapply(
    X = c(
      "inpatient_episodes"
    ), 
    FUN = DBI::dbExistsTable, 
    conn = input_patient@conn, 
    FUN.VALUE = logical(1L))
  if (any(tables_missing)) {
    warning(
      "The following tables are missing from the Ramses database:\n",
      paste(paste0("`", names(tables_missing)[tables_missing], "`"), collapse = ", "), "\n",
      "Please consult ?load_inpatient_diagnoses for help.",
      call. = FALSE
    )
    return(data.frame())
  }
  
  tbl(input_patient@conn, "inpatient_episodes") %>%
    dplyr::filter(.data$patient_id == !!input_patient@id) %>% 
    dplyr::filter(!is.na(.data$discharge_date)) %>%
    dplyr::distinct(.data$admission_method, .data$admission_date, .data$discharge_date) %>%
    na.omit() %>% 
    dplyr::collect() %>% 
    dplyr::transmute(
      id = NA_character_, 
      content =  dplyr::case_when(
        .data$admission_method == 2 ~ "Emergency admission",
        .data$admission_method == 1 ~ "Elective admission",
        TRUE ~ "Other/Transfer"),
      start = .data$admission_date, 
      end = .data$discharge_date,
      type = 'background',
      className = dplyr::if_else(
        .data$admission_method == 2,
        'admission-emergency', 
        'admission-elective')
    ) %>%
    as.data.frame()
}

