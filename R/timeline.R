

#' Build an antiinfective timeline visualisation
#'
#' @param conn a database connection
#' @param patient_identifier a length-one character vector for a patient ID
#' @param date1 (optional) a \code{Date} or \code{POSIXct} window minimum to 
#' focus the timeline on by default
#' @param date2 (optional) a \code{Date} or \code{POSIXct} window maximum to 
#' focus the timeline on by default
#' @param load_timevis_dependencies a boolean indicating whether jQuery 
#' and Bootstrap should be loaded (default is \code{FALSE}). See \link[timevis]{timevis}
#' for detail
#' @return a timevis object
#' @importFrom dplyr tbl
#' @importFrom magrittr %>%
#' @export
therapy_timeline <- function(conn, patient_identifier, 
                             date1 = NULL, date2 = NULL,
                             load_timevis_dependencies = FALSE){
  
  requireNamespace("timevis", quietly = TRUE)
  stopifnot( is.null(date1) | is(date1, "Date") | is(date1, "POSIXct") )
  stopifnot( is.null(date2) | is(date2, "Date") | is(date2, "POSIXct") )
  
  # Retrieve inpatient records
  base <- tbl(conn, "inpatient_episodes") %>%
    dplyr::filter(patient_id == patient_identifier) %>% 
    collect_ramses_tbl()
    
  if (nrow(base) == 0) {
    stop("Patient was not found.")
  }
  
  # Retrieve prescription records  
  nodes <- tbl(conn, "drug_prescriptions") %>%
    dplyr::filter(patient_id == patient_identifier & 
             !prescription_status %in% c("cancelled", "draft")) %>%
    dplyr::left_join(tbl(conn, "drug_therapy_episodes"), 
                     by = c("patient_id", "therapy_id")) %>%
    dplyr::arrange(patient_id, prescription_start) %>% 
    collect_ramses_tbl()
   
  # Calculate the date range the timeline should display by default

  if( !is.null(date1) && !is.null(date2) ) {
    date1 <- as.POSIXct(date1)
    date2 <- as.POSIXct(date2)
    date_window <- dplyr::filter(
      nodes, 
      dplyr::between(therapy_start, date1, date2) |
        dplyr::between(therapy_end, date1, date2) |
        (therapy_start <= date1 & date1 <= therapy_end)) %>%
      dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                       right_date = max(therapy_end, na.rm = T))
  } else if( !is.null(date2) ) {
    date2 <- as.POSIXct(date2)
    date_window <- dplyr::filter(
      nodes, 
      therapy_start <= date2 ) %>%
      dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                       right_date = max(therapy_end, na.rm = T)) 
  } else if( !is.null(date1) ) {
    date1 <- as.POSIXct(date1)
    date_window <- dplyr::filter(
      nodes, 
      therapy_end >= date1 ) %>%
      dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                       right_date = max(therapy_end, na.rm = T)) 
  } else {
    date_window <- dplyr::summarise(
      nodes, 
      left_date = min(therapy_start, na.rm = T), 
      right_date = max(therapy_end, na.rm = T)) 
  } 
  
  # extract all data to feed into timevis
  timeline_diags <- .therapy_timeline_get_diagnoses(
    conn = conn, 
    patient_identifier = patient_identifier)
  
  timeline_Rx <- nodes %>%
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
              type = ifelse(daily_frequency < 0, 'point', 'range'),
              group = 3,
              subgroup = as.character(therapy_rank),
              className = dplyr::case_when(
                daily_frequency < 0 ~ "prescription-OOF",
                prescription_context == "discharge" ~ "prescription-REGT",
                TRUE ~ "prescription-REG"
              )) %>% as.data.frame()
  
  # This extracts data for the time of request 
  # (displaying a vertical bar before the drug administrations start)
  timeline_Rx_request <- nodes %>%
    dplyr::filter(!is.na(prescription_start)) %>%
    dplyr::arrange(therapy_rank) %>%
    dplyr::transmute(id = paste0("MedicationRequest:", prescription_id, ":request"), 
              title = paste0("Time requested: ", authoring_date),
              start = authoring_date,
              type =  'point' ,
              group = 3,
              subgroup = as.character(therapy_rank),
              className = "prescription-request") %>% as.data.frame()
  
  timeline_adms <- base %>% 
    dplyr::filter(!is.na(discharge_date)) %>%
    dplyr::select(admission_method, admission_date, discharge_date) %>%
    na.omit() %>% unique() %>% 
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
  
  timeline_micro <- .therapy_timeline_get_micro(conn = conn,
                                                patient_identifier = patient_identifier)
  
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
      list(timeline_Rx, timeline_Rx_request,
           timeline_adms, timeline_diags,
           timeline_redflags, timeline_CC,
           timeline_micro, timeline_death)
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



#' Extract a data frame of microbiology results for display in therapy timeline
#' @param conn a database connection
#' @param patient_identifier a patient identifier
#' @return a data frame
#' @noRd
.therapy_timeline_get_micro <- function(conn,
                                        patient_identifier) {
  
  micro <- tbl(conn, "microbiology_specimens") %>%
    dplyr::filter(patient_id == patient_identifier) %>% 
    dplyr::left_join(
      tbl(conn, "microbiology_isolates"),
      by = c("patient_id", "specimen_id")
    ) %>% 
    collect_ramses_tbl()
  
  
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
}}
  


#' Extract a data frame of infection diagnoses for display in therapy timeline
#' @param conn a database connection
#' @param patient_identifier a patient identifier
#' @return a data frame
#' @noRd
.therapy_timeline_get_diagnoses <- function(conn,
                                            patient_identifier) {
    
  
  # Taking ICD labels from the most recent ICD edition
  icd_lu <- tbl(conn, "reference_icd") %>%
    dplyr::select(icd_code,
                  icd_display,
                  icd_description,
                  category_description)
  
  # Consolidating diagnoses across episodes of care
  diag <- tbl(conn, "inpatient_diagnoses") %>%
    dplyr::filter(patient_id == patient_identifier) %>% 
    dplyr::inner_join(tbl(conn, "reference_icd_infections"), 
               by = c("icd_code")) %>%
    dplyr::compute()
  
  # If no infection diagnosis is present, return empty data frame.
  # It will get ignore when feeding into timevis
  if(nrow(dplyr::collect(diag)) == 0){
    return(data.frame())
  } 
  
  if( is(diag, "tbl_SQLiteConnection") ) {
    # combining episodes together for diagnoses that span several episodes
    diag <- diag %>%
      dplyr::mutate(
        prim_diag = dplyr::if_else(diagnosis_position == 1, 1, 0),
        grp = dplyr::sql("
         CASE WHEN ABS(strftime('%s', LAG([episode_end], 1, 0)
                       OVER(PARTITION BY patient_id, icd_code
                            ORDER BY [episode_start]))
                       - strftime('%s', [episode_start])) <= (6 * 3600) 
         THEN NULL
         ELSE ROW_NUMBER() OVER(ORDER BY [episode_start]) END")) %>% 
      dplyr::mutate(grp = dplyr::sql(
        "MAX(grp) 
         OVER(PARTITION BY patient_id, icd_code, prim_diag
         ORDER BY patient_id, [episode_start])"
      )) 
  } else if( is(diag, "tbl_PqConnection") ) {
    # combining episodes together for diagnoses that span several episodes
    diag <- diag %>%
      dplyr::mutate(diagnosis_episode_end = dplyr::sql("
      LAG(episode_end, 1)
      OVER(PARTITION BY patient_id, icd_code
           ORDER BY episode_start)")) %>% 
      dplyr::mutate(
        prim_diag = dplyr::if_else(diagnosis_position == 1, 1, 0),
        grp = dplyr::sql("
         CASE WHEN ABS( EXTRACT(EPOCH FROM( 
               diagnosis_episode_end::TIMESTAMP
                - episode_start::TIMESTAMP ))) <= (6 * 3600) 
         THEN NULL
         ELSE ROW_NUMBER() OVER(ORDER BY episode_start) END")) %>% 
      dplyr::mutate(grp = dplyr::sql(
        "MAX(grp) 
         OVER(PARTITION BY patient_id, icd_code, prim_diag
              ORDER BY patient_id, episode_start)"
      )) %>% 
      dplyr::select(-diagnosis_episode_end)
  } else {
    .throw_error_method_not_implemented(".therapy_timeline_get_diagnoses()", class(diag))
  }
  
  diag <- diag %>%
    dplyr::group_by(patient_id, icd_code, grp, prim_diag, 
                    infection_group1_code,
                    infection_group1_label,
                    infection_group2_code,
                    infection_group2_label) %>%
    dplyr::summarise(start_time = min(episode_start, na.rm = TRUE),
                     end_time = max(episode_end, na.rm = TRUE)) %>%
    dplyr::compute()
  
  diag <- diag %>%
    dplyr::left_join(icd_lu, by = c('icd_code')) %>% 
    collect_ramses_tbl() %>% 
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


