
#' Simulated episodes of inpatient care
#' 
#' @description This simulated activity data consists of records of finished 
#' consultant episodes for 200 patients, detailing all dates and times of 
#' admission, dischage, activity dates, and ICD-10 clinical diagnoses codes 
#' (for infections only).
#' 
#' This data was simulated to reproduce relevant characteristics of English 
#' NHS Admitted Patient Care datasets. More information can be found on the 
#' \href{https://www.datadictionary.nhs.uk/data_dictionary/messages/cds_v6-2/data_sets/cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds_fr.asp?shownav=1}{NHS data dictionary website}.
#' @format A data frame of records of finished consultant episodes.
#' @docType data
#' @name inpatient_episodes
#' @source Simulated data
"inpatient_episodes"

#' Simulated inpatient clinical diagnoses
#' 
#' @description Dataset of simulated infection clinical diagnoses by 
#' finished consultant episodes listed in \code{\link{inpatient_episodes}},
#' detailing all episode start and end times, episode number, and up to
#' 5 clinical diagnoses coded according to the International 
#' Classification of Diseases, Tenth Revision (ICD-10). 
#' 
#' This data was simulated to reproduce a set of combinations of infection 
#' diagnoses in an English NHS hospital. For each episode, a maximum of 5
#' diagnoses are simulated. Non-infection diagnoses are not simulated, an
#' NA value is recorded instead. 
#'  
#' @format A data frame of records of clinical diagnoses.
#' @docType data
#' @name inpatient_diagnoses
#' @source Simulated data
"inpatient_diagnoses"

#' Simulated antimicrobial prescriptions
#' 
#' @description A set of drug prescriptions records simulated 
#' based on commonly prescribed drugs for clinical diagnoses 
#' recorded in inpatient care episode records. 
#'   
#' @format A data frame of records of drug prescriptions.
#' @docType data
#' @name drug_prescriptions
#' @source Simulated data
"drug_prescriptions"

#' Simulated antimicrobial drug administrations
#' 
#' @description A set of records of antimicrobials administered
#' during inpatient care episodes, simulated based on drugs commonly 
#' prescribed in hospitals. 
#'   
#' @format A data frame of records of drug administrations.
#' @docType data
#' @name drug_administrations
#' @source Simulated data
"drug_administrations"