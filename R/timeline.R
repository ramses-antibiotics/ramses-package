

#' Display an HTML antimicrobial therapy timeline
#'
#' @param x an object of class \code{Patient} or \code{TherapyEpisode}
#' @param date1 (optional, only used for \code{x} of class \code{Patient}) a \code{Date} or \code{POSIXct} window minimum to 
#' focus the timeline on by default
#' @param date2 (optional, only used for \code{x} of class \code{Patient}) a \code{Date} or \code{POSIXct} window maximum to 
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
    
    # Retrieve prescription records  
    medication_requests_inclusion <- tbl(input_patient@conn, "drug_prescriptions") %>%
      dplyr::filter(patient_id == !!input_patient@id & 
                      !prescription_status %in% c("cancelled", "draft", "in-error")) %>%
      dplyr::left_join(tbl(input_patient@conn, "drug_therapy_episodes"), 
                       by = c("patient_id", "therapy_id")) %>%
      dplyr::arrange(patient_id, prescription_start) %>% 
      dplyr::collect()
    
    # Calculate the date range the timeline should display by default
    
    if( !is.null(date1) && !is.null(date2) ) {
      date1 <- as.POSIXct(date1)
      date2 <- as.POSIXct(date2)
      date_window <- dplyr::filter(
        medication_requests_inclusion, 
        dplyr::between(therapy_start, date1, date2) |
          dplyr::between(therapy_end, date1, date2) |
          (therapy_start <= date1 & date1 <= therapy_end)) %>%
        dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                         right_date = max(therapy_end, na.rm = T))
    } else if( !is.null(date2) ) {
      date2 <- as.POSIXct(date2)
      date_window <- dplyr::filter(
        medication_requests_inclusion, 
        therapy_start <= date2 ) %>%
        dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                         right_date = max(therapy_end, na.rm = T)) 
    } else if( !is.null(date1) ) {
      date1 <- as.POSIXct(date1)
      date_window <- dplyr::filter(
        medication_requests_inclusion, 
        therapy_end >= date1 ) %>%
        dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                         right_date = max(therapy_end, na.rm = T)) 
    } else {
      date_window <- dplyr::summarise(
        medication_requests_inclusion, 
        left_date = min(therapy_start, na.rm = T), 
        right_date = max(therapy_end, na.rm = T)) 
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
    
    input_patient <- Patient(input_therapy_episode@conn, 
                             collect(input_therapy_episode)$patient_id)
    if( nrow(collect(input_patient)) == 0 ) {
      stop("Patient not found in the database", call. = FALSE)
    }
    
    date_window <- collect(input_therapy_episode) %>% 
      dplyr::transmute(left_date = therapy_start,
                       right_date = therapy_end)
    
    # Retrieve prescription records  
    medication_requests_inclusion <- tbl(input_patient@conn, "drug_prescriptions") %>%
      dplyr::filter(patient_id == !!input_patient@id & 
                      !prescription_status %in% c("cancelled", "draft", "in-error")) %>%
      dplyr::left_join(tbl(input_patient@conn, "drug_therapy_episodes"), 
                       by = c("patient_id", "therapy_id")) %>%
      dplyr::arrange(patient_id, prescription_start) %>% 
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
    id = 3:0,
    content = c("Rx", "Diagnoses", "Clinical findings",
                "Admissions"# spacer at top of timeline swimming lanes
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
    dplyr::filter(!is.na(prescription_start)) %>%
    dplyr::arrange(therapy_rank) %>%
    dplyr::mutate(duration = as.numeric(prescription_end - prescription_start, unit = "secs")) %>% 
    dplyr::transmute(id = paste0("MedicationRequest:", prescription_id, ":Rx"), 
                     content = prescription_text,
                     title = paste0(prescription_start, ", ",
                                    ifelse(daily_frequency < 0, "",
                                           ifelse(duration < 24*3600,
                                                  paste(trunc(duration/3600), "hours, "),
                                                  paste(trunc(duration/3600/24), "days, "))),
                                    "Therapy episode: ", therapy_id, ", position: ", therapy_rank), 
                     start = prescription_start,
                     end = prescription_end,
                     type = dplyr::if_else(daily_frequency < 0, 'point', 'range'),
                     group = 3,
                     subgroup = as.character(therapy_rank),
                     className = dplyr::case_when(
                       daily_frequency < 0 ~ "prescription-OOF",
                       prescription_context == "discharge" ~ "prescription-REGT",
                       TRUE ~ "prescription-REG"
                     )) %>% as.data.frame()
  
  # This extracts data for the time of request 
  # (displaying a vertical bar before the drug administrations start)
  timeline_Rx_request <- medication_requests_df %>%
    dplyr::filter(!is.na(prescription_start)) %>%
    dplyr::arrange(therapy_rank) %>%
    dplyr::transmute(id = paste0("MedicationRequest:", prescription_id, ":request"), 
                     title = paste0("Time requested: ", authoring_date),
                     start = authoring_date,
                     type =  'point' ,
                     group = 3,
                     subgroup = as.character(therapy_rank),
                     className = "prescription-request") %>% as.data.frame()
  
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
  
  micro <- tbl(input_patient@conn, "microbiology_specimens") %>%
    dplyr::filter(patient_id == !!input_patient@id) %>% 
    dplyr::left_join(
      tbl(input_patient@conn, "microbiology_isolates"),
      by = c("patient_id", "specimen_id")
    ) %>% 
    dplyr::collect()
  
  if(nrow(micro) == 0){
    return(data.frame())
  } else {
    timeline_micro_reports <- micro %>%
      dplyr::mutate(className = dplyr::if_else(is.na(organism_code),
                                               "micro-report-no-growth", 
                                               "micro-report")) %>% 
      dplyr::group_by(specimen_id, className) %>% 
      dplyr::summarise(
        id = paste0("Specimen ID:",
                    paste(unique(specimen_id), sep = "-"), 
                    ":report"),
        start = min(isolation_datetime, na.rm = T),
        title = paste0(
          paste(unique(specimen_type_display), collapse = ", "), "\n",
          "Organisms: ", 
          paste(unique(organism_display_name), collapse = ", "), "\n",
          "Sample received: ", unique(as.Date(specimen_datetime))
          ),
        type = "point",
        group = 1,
        subgroup = "micro"
      )
    
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
  
  # Taking ICD labels from the most recent ICD edition
  icd_lu <- tbl(input_patient@conn, "reference_icd") %>%
    dplyr::select(icd_code,
                  icd_display,
                  icd_description,
                  category_description)
  
  # Consolidating diagnoses across episodes of care
  diag <- tbl(input_patient@conn, "inpatient_diagnoses") %>%
    dplyr::filter(patient_id == !!input_patient@id) %>% 
    dplyr::inner_join(tbl(input_patient@conn, "reference_icd_infections"), 
               by = c("icd_code")) %>%
    dplyr::collect()
  
  # If no infection diagnosis is present, return empty data frame.
  # It will get ignore when feeding into timevis
  if(nrow(diag) == 0){
    return(data.frame())
  } 

  # combining episodes together for diagnoses that span several episodes
  diag <- diag %>%
    dplyr::group_by(.data$patient_id, .data$icd_code) %>% 
    dplyr::mutate(diagnosis_episode_end = dplyr::lag(.data$episode_end, 
                                                     n = 1, 
                                                     order_by = .data$episode_start)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      prim_diag = dplyr::if_else(.data$diagnosis_position == 1, 1, 0),
      grp = dplyr::if_else(
        abs(as.numeric(.data$diagnosis_episode_end - .data$episode_start, units = "secs")) <= (6 * 3600),
        NA_integer_,
        dplyr::row_number(.data$episode_start)
      )
    ) %>% 
    dplyr::group_by(.data$patient_id, .data$icd_code, .data$prim_diag) %>% 
    dplyr::mutate(grp = max(.data$grp)) %>% 
    dplyr::select(-.data$diagnosis_episode_end) %>%
    dplyr::group_by(patient_id, icd_code, grp, prim_diag, 
                    infection_group1_code,
                    infection_group1_label,
                    infection_group2_code,
                    infection_group2_label) %>%
    dplyr::summarise(start_time = min(episode_start, na.rm = TRUE),
                     end_time = max(episode_end, na.rm = TRUE)) 
  
  diag <- dplyr::right_join(icd_lu, diag, 
                            by = "icd_code", copy = TRUE) %>% 
    dplyr::collect() %>% 
    data.table() %>% unique() %>%
    dplyr::mutate(icd_text = paste0(
      dplyr::if_else(prim_diag == 1, "<strong>", ""), 
      icd_display, " &ndash; ", icd_description,
      dplyr::if_else(prim_diag == 1, "</strong>", "")))
  
  diag <- dplyr::transmute(
    diag,
    id = NA, 
    content = icd_text, 
    title = paste0(icd_description, ifelse(prim_diag == 1, " (PRIMARY DIAGNOSIS)", "")),
    start = start_time,  
    end = end_time,
    group =  2,
    subgroup = infection_group1_code,
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
  
  tbl(input_patient@conn, "inpatient_episodes") %>%
    dplyr::filter(patient_id == !!input_patient@id) %>% 
    dplyr::filter(!is.na(discharge_date)) %>%
    dplyr::distinct(admission_method, admission_date, discharge_date) %>%
    na.omit() %>% 
    dplyr::collect() %>% 
    dplyr::transmute(
      id = NA_character_, 
      content =  dplyr::case_when(
        admission_method == 2 ~ "Emergency admission",
        admission_method == 1 ~ "Elective admission",
        TRUE ~ "Other/Transfer"),
      start = admission_date, 
      end = discharge_date,
      # group =  2,
      # subgroup = 1,
      type = 'background',
      className = dplyr::if_else(
        admission_method == 2,
        'admission-emergency', 
        'admission-elective')
    ) %>%
    as.data.frame()
}

