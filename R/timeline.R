

# TODO
# ramses_ui_data_filter_prescription_status <- function(){
#   # keep "cancelled" and "draft" out
#   c("active", "on-hold", "completed", "entered-in-error", "stopped", "unknown")
# }


# TODO rename, document and test function!
#' Build an antiinfective timeline visualisation
#'
#' @param conn a database connection
#' @param patient_identifier a length-one character vector for a patient ID
#' @param date1 (optional) a date window minimum to focus the timeline on by default
#' @param date2 (optional) a date window maximum to focus the timeline on by default
#' @param load_timevis_dependencies a boolean indicating whether jQuery 
#' and Boostrap should be loaded (default is FALSE). See \link[timevis]{timevis}
#' for detail
#' @return a timevis object
#' @importFrom dplyr tbl
#' @importFrom magrittr %>%
#' @importFrom lubridate as_datetime
#' @export
therapy_timeline <- function(conn, patient_identifier, 
                             date1 = NA, date2 = NA,
                             load_timevis_dependencies = FALSE){
  
  requireNamespace("timevis", quietly = TRUE)
  
  # Retrieve inpatient records
  base <- tbl(conn, "inpatient_episodes") %>%
    dplyr::filter(patient_id == patient_identifier) %>%
    .sqlite_date_collect()
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
    .sqlite_date_collect() 
   
  # Calculate the date range the timeline should display by default
  date1 <- lubridate::as_datetime(date1)
  date2 <- lubridate::as_datetime(date2)
  
  if( !is.na(date1) && !is.na(date2) ) {
    date_window <- dplyr::filter(
      nodes, 
      dplyr::between(therapy_start, date1, date2) |
        dplyr::between(therapy_end, date1, date2) |
        (therapy_start <= date1 & date1 <= therapy_end)) %>%
      dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                       right_date = max(therapy_end, na.rm = T))
  } else if( !is.na(date2) ) {
    date_window <- dplyr::filter(
      nodes, 
      therapy_start <= date2 ) %>%
      dplyr::summarise(left_date = min(therapy_start, na.rm = T), 
                       right_date = max(therapy_end, na.rm = T)) 
  } else if( !is.na(date1) ) {
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
  timeline_diags <- NULL #TODO timeline_Rx_getdiag(input_patient)
  
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
    dplyr::transmute(id = NA, content =  dplyr::case_when(
      admission_method == 2 ~ "Emergency admission",
      admission_method == 1 ~ "Elective admission",
      TRUE ~ "Other/Transfer"),
      start = admission_date, end = discharge_date,
      # group =  2,
      # subgroup = 1,
      type = 'background',
      className = ifelse(admission_method == 2,
                         'admission-emergency', 
                         'admission-elective')) %>%
    as.data.frame()
  
  timeline_death <- NULL # timeline_Rx_getdeath(input_patient)
  
  timeline_CC <- NULL # timeline_Rx_getCC(input_patient)
  
  timeline_redflags <- NULL # tbl(conn, "APC_Investigations") %>%
  #   filter(patient_id == input_patient & (
  #     (inv == 'BPSYS' & value_num <= 90 ) |
  #       (inv == 'HR' & value_num > 130) |
  #       (inv == 'RESP' & value_num >= 25) |
  #       (inv == 'FLUOTURO' & value_num < 500)  |
  #       (inv == 'FLUOTUR' & value_num < 500) |
  #       (inv == 'UVOL' & value_num < 500) |
  #       (inv == 'SEWSSCORE' & value_num >= 4)
  #   )) %>% collect() %>% 
  #   transmute(id = NA, 
  #             title = paste0(inv, ": ", value, ", ", start_time),
  #             start = start_time,
  #             type = "point",
  #             group = 1,
  #             subgroup = "red-flag",
  #             className = "sepsis-redflag"
  #   )
  
  timeline_micro <- NULL # timeline_Rx_getmicro(input_patient)
  
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
