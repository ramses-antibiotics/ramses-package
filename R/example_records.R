

#' Simulated patients
#' 
#' @description Set of simulated demographics records of 202 patients 
#' included in \code{\link[Ramses]{inpatient_episodes}}.
#' @format A data frame of patient demographics.
#' @docType data
#' @name patients
#' @source Simulated data
"patients"

#' Simulated episodes of inpatient care
#' 
#' @description Set of simulated records of finished consultant episodes for
#' 202 patients, detailing all dates and times of admission, discharge, 
#' episode start and end, clinician, and clinical specialty.
#' 
#' This data was simulated to reproduce relevant characteristics of English 
#' NHS Admitted Patient Care datasets. More information can be found on the 
#' \href{https://datadictionary.nhs.uk/data_sets/cds_v6-2/cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds.html#dataset_cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds}{NHS data dictionary website}.
#' @format A data frame of records of finished consultant episodes.
#' @docType data
#' @name inpatient_episodes
#' @source Simulated data
"inpatient_episodes"

#' Simulated hospital ward movements
#' 
#' @description Set of simulated records of ward movements during 
#' hospitalisation encounters included in \code{\link[Ramses]{inpatient_episodes}}.
#' 
#' This data was simulated to reproduce relevant characteristics of English 
#' NHS Admitted Patient Care datasets. More information can be found on the 
#' \href{https://datadictionary.nhs.uk/data_sets/cds_v6-2/cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds.html#dataset_cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds}{NHS data dictionary website}.
#' @format A data frame of records of ward movements
#' @docType data
#' @name inpatient_wards
#' @source Simulated data
"inpatient_wards"

#' Simulated inpatient clinical diagnoses
#' 
#' @description Set of simulated clinical diagnoses of infection for every
#' row in \code{\link[Ramses]{inpatient_episodes}},
#' detailing all episode start and end times, episode number, and up to
#' 5 clinical diagnoses coded according to the International 
#' Classification of Diseases, Tenth Revision (ICD-10). 
#' 
#' This data was simulated to reproduce a set of combinations of infection 
#' diagnoses in an English NHS hospital. For each episode, a maximum of 5
#' diagnoses are simulated. Non-infection diagnoses are not simulated, an
#' NA value is recorded instead. 
#'  
#' This data was simulated to reproduce relevant characteristics of English 
#' NHS Admitted Patient Care datasets. More information can be found on the 
#' \href{https://datadictionary.nhs.uk/data_sets/cds_v6-2/cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds.html#dataset_cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds}{NHS data dictionary website}.
#' @format A data frame of records of clinical diagnoses.
#' @docType data
#' @name inpatient_diagnoses
#' @source Simulated data
"inpatient_diagnoses"

#' Simulated antimicrobial drug prescriptions
#' 
#' @description A set of simulated antibiotic prescriptions records simulated 
#' based on commonly prescribed drugs for clinical diagnoses 
#' recorded in \code{\link[Ramses]{inpatient_episodes}}. 
#'   
#' @format A data frame of records of drug prescriptions.
#' @docType data
#' @name drug_prescriptions
#' @source Simulated data
"drug_prescriptions"

#' Simulated antimicrobial drug administrations
#' 
#' @description A set of simulated records of administrations of antibiotics 
#' during inpatient care episodes, simulated based on drugs commonly 
#' prescribed in hospitals. 
#'   
#' @format A data frame of records of drug administrations.
#' @docType data
#' @name drug_administrations
#' @source Simulated data
"drug_administrations"


#' Simulated clinical investigation results
#' 
#' @description A set of simulated records of observations (for example: respiration rate,
#' blood pressure) and investigations (for example: blood cell counts) for 
#' two patients included in \code{\link[Ramses]{inpatient_episodes}}.
#'   
#' @format A data frame of records of observations and investigations 
#' passing \code{\link[Ramses]{validate_investigations}()}.
#' @docType data
#' @name inpatient_investigations
#' @source Simulated data
"inpatient_investigations"


#' Simulated microbial culture and susceptibility results
#' 
#' @description A set of simulated records of microbial samples sent 
#' for culture and susceptibility testing for one patient included
#' in \code{\link[Ramses]{inpatient_episodes}}.
#' 
#' @format A data frame
#' @seealso \link[Ramses]{validate_microbiology}()
#' @docType data
#' @name inpatient_microbiology
#' @source Simulated data
"inpatient_microbiology"
