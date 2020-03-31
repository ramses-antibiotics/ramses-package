
validate_variable_no_missing <- function(data, vectorname){
  
  validation_result <- TRUE
  
  if(!exists(vectorname, data)) {
    warning(simpleWarning(paste0("`data` must include a `", vectorname, "` variable.")))
    validation_result <- FALSE
  } else {
    
    if(any(is.na(data[, vectorname]))) {
      warning(simpleWarning(paste0("Some `", vectorname, "` is missing.")))
      validation_result <- FALSE
    }
    
    if(any(na.omit(as.character(data[, vectorname]) == ""))) {
      warning(simpleWarning(paste0("Some `", vectorname, "` == \"\"")))
      validation_result <- FALSE
    }
    
  } 
  
  validation_result
}

#' Validate inpatient episode records
#'
#' @description This function performs a series of checks for mandatory and
#' optional requirements on episodes of care records. The data definitions 
#' closely follow those of the  
#' \href{https://www.datadictionary.nhs.uk/data_dictionary/messages/cds_v6-2/data_sets/cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds_fr.asp}{English NHS Admitted Patient Care Commissioning Datasets}.
#' @param data data frame containing one row per episode of care
#' @section Mandatory variables:
#' \describe{
#'   \item{\code{patient_id}}{a patient identifier with no missing value}
#'   \item{\code{spell_id}}{a hospital spell identifier with no missing value}
#'   \item{\code{admission_method}}{a non-missing character code: \itemize{
#'        \item \code{"1"} elective admission
#'        \item \code{"2"} emergency admission
#'        \item \code{"3"} transfer/other admission
#'     }
#'   \emph{Note:} \code{"1"} and \code{"2"} corresponds to the first character 
#'   of the \href{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/add/admission_method_de.asp}{NHS admission method value set}; \code{"3"} corresponds to 
#'   the remaining values starting with \code{3} or \code{8}.}
#'   \item{\code{admission_date}}{a \code{POSIXct} timestamp for 
#'   the hospital admission. Must not be missing.}
#'   \item{\code{discharge_date}}{a \code{POSIXct} timestamp for 
#'   the hospital discharge. Must not be missing.}
#'   \item{\code{episode_number}}{a strictly positive integer indicating the
#'   number of the episode within an admission. Must not be missing.}
#'   \item{\code{last_episode_in_spell_indicator}}{a character indicating whether
#'   the patient is discharged at the end of the episode: \itemize{
#'        \item \code{"1"} the episode is the last episode in the spell
#'        \item \code{"2"} the episode is \strong{not} the last episode in the spell
#'    }
#'    Must not be missing.}
#'   \item{\code{episode_start}}{a \code{POSIXct} timestamp for 
#'   the hospital start. Must not be missing.}
#'   \item{\code{episode_end}}{a \code{POSIXct} timestamp for 
#'   the hospital end Must not be missing.}
#'   \item{\code{consultant_code}}{a code uniquely identifying the medical 
#'   professional responsible for the episode of care. Must not be missing.}
#'   \item{\code{care_professional_main_specialty_code}}{a code identifying
#'   the main specialty of the medical professional responsible for the 
#'   episode of care. Must not be missing.}
#' }
#' @section Optional variables:
#' \describe{
#'   \item{\code{activity_treatment_function_code}}{0}
#'   \item{\code{local_sub-specialty_code}}{0}
#'   \item{\code{patient_forename}}{the patient's forename}
#'   \item{\code{patient_surname}}{the patient's surname}
#'   \item{\code{date_of_birth}}{a \code{Date} for the birth date}
#'   \item{\code{date_of_death}}{a missing value or a \code{Date} of death}
#'   \item{\code{patient_sex}}{the following values are valid: \itemize{
#'        \item \code{"male"}  
#'        \item \code{"female"}  
#'        \item \code{"other"}
#'        \item \code{"unknown"}
#'    }
#'   Must not be missing.}
#'   \item{\code{age_on_admission}}{age in years at start of hospital spell}
#'   \item{\code{ethnic_category_UK}}{Reserved for UK users for \code{Ramses} to compute
#'   the empirical glomerular filtration rate (eGFR). The following codes are valid:
#'   
#'   White \itemize{
#'       \item \code{"A"} British
#'       \item \code{"B"} Irish
#'       \item \code{"C"}	Any other White background
#'   }
#'   Mixed
#'   \itemize{
#'       \item \code{"D"} White and Black Caribbean
#'       \item \code{"E"} White and Black African
#'       \item \code{"F"} White and Asian
#'       \item \code{"G"} Any other mixed background
#'   }
#' 
#'   Asian or Asian British
#'   \itemize{
#'       \item \code{"H"} Indian
#'       \item \code{"J"} Pakistani
#'       \item \code{"K"} Bangladeshi
#'       \item \code{"L"} Any other Asian background
#'   }
#'
#'   Black or Black British
#'   \itemize{
#'       \item \code{"M"} Caribbean
#'       \item \code{"N"} African
#'       \item \code{"P"} Any other Black background
#'   }
#'
#'   Other Ethnic Groups
#'   \itemize{
#'       \item \code{"R"} Chinese
#'       \item \code{"S"} Any other ethnic group
#'   }
#'   
#'   Not stated \itemize{
#'       \item \code{"Z"} Not stated
#'   }}
#' }
#' @return A logical value indicating success
#' @export
validate_inpatient_episodes <- function(data) {
  
  
  ip_schema <- system.file("Schema", "inpatient_episodes_variables.csv", 
                           package = "Ramses")
  ip_schema <- utils::read.csv(ip_schema, stringsAsFactors = FALSE)
  
  variable_exists <- ip_schema[ip_schema$must_exist, "variable_name"]

  not_exist <- !sapply(variable_exists, exists, where = data)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", variable_exists[not_exist], "`"), collapse = ", "))
      )
    )
  }
  
  variable_exists_non_missing <- ip_schema[ip_schema$must_be_nonmissing, 
                                           "variable_name"]
  variable_exists_non_missing <- variable_exists_non_missing[
    variable_exists_non_missing %in% colnames(data)]
  
  missing_data <- suppressWarnings(
    !sapply(variable_exists_non_missing, 
            FUN = validate_variable_no_missing,
            data = data)
  )
  
  if( any(missing_data) ){
    stop(
      simpleError(paste(
        "Some variables must not contain missing data:",
        paste(paste0("`", 
                     variable_exists_non_missing[missing_data],
                     "`"), collapse = ", "))
      )
    )
  }
  
  validation_result <- validate_inpatient_spells(data)
  validation_result <- append(validate_inpatient_episode_dates(data), validation_result)
 
  !any(!validation_result)
}

#' Validate the consistency of admission and discharge dates
#'
#' @param data a data frame object
#' @importFrom data.table data.table
#' @return A logical value indicating success
validate_inpatient_spells <- function(data) {
  
  validation_result <- TRUE
  
  if (any(data$admission_date > data$discharge_date)) {
    warning(simpleWarning("Some admission dates are posterior to discharge dates."))
    validation_result <- FALSE
  }
  
  spells <- data[, c(
    "patient_id", "spell_id",
    "admission_date", "discharge_date"
  )]
  
  spells <- unique(data.table::data.table(spells))
  
  data_cross_prod <- merge(
    spells, spells, 
    by = "patient_id",
    all = T,
    allow.cartesian = T
  )
  
  data_cross_prod <- data_cross_prod[spell_id.x != spell_id.y]

  data_cross_prod <- data_cross_prod[
    data.table::between(admission_date.x, admission_date.y, discharge_date.y, incbounds = F) |
      data.table::between(discharge_date.x, admission_date.y, discharge_date.y, incbounds = F) 
    ]
  
  if (nrow(data_cross_prod) > 0) {
    warning(simpleWarning("Hospital spells must not overlap."))
    warning(simpleWarning(
      .print_and_capture(
        utils::head(data.table::setorderv(data_cross_prod, 
                                          c("patient_id", "admission_date.x"))))
    ))
    validation_result <- FALSE
  }
  
  validation_result
}


#' Validate the consistency of inpatient episode dates
#'
#' @param data a data frame object
#' @importFrom data.table data.table := 
#' @return A logical value indicating success
validate_inpatient_episode_dates <- function(data) {
  
  validation_result <- TRUE
  if (any(data$episode_start > data$episode_end)) {
    warning(simpleWarning("Some episode start dates are posterior to end dates."))
    validation_result <- FALSE
  }
  
  if (any(!(data$episode_start >= data$admission_date & data$episode_start <= data$discharge_date))) {
    warning(simpleWarning("Some episodes fall outside hospitalisation dates."))
    validation_result <- FALSE
  }
  
  if (any(!(data$episode_end >= data$admission_date & data$episode_end <= data$discharge_date))) {
    warning(simpleWarning("Some episodes fall outside hospitalisation dates."))
    validation_result <- FALSE
  }
  
  BD_spell <-  BD_episode <-  nextepistart <-  NULL
  
  episodes <- data.table::as.data.table(data)
  
  bed_day_matching <- episodes[,
    list(BD_episode = sum(difftime(episode_end, episode_start,  units = "hours"))), 
    by = list(patient_id, spell_id, admission_date, discharge_date)
    ]
  bed_day_matching[, BD_spell := difftime(discharge_date, admission_date, units = "hours")]
  
  if (nrow(bed_day_matching[BD_episode != BD_spell]) > 0) {
    warning(simpleWarning(paste(
      "Total bed days calculated from episode of cares does not",
      "match admission duration. Bed days may be incorrect.")))
  }
  
  rm(bed_day_matching)
  
  episodes <- episodes[, list(patient_id, spell_id,
                              episode_start, episode_end)]
  
  episodes <- data.table::setorderv(
    episodes, c("patient_id", "spell_id", "episode_start"))
  
  episodes[ , `:=`(nextepistart =  data.table::shift(episode_start, type = "lead")), 
            by = list(patient_id, spell_id)]
  
  if (nrow(episodes[!is.na(nextepistart) & nextepistart != episode_end]) > 0) {
    warning(simpleWarning(paste(
      "Some hospital spells have gaps between episodes.",
      "Bed days may be underestimated.")))
  }

  episodes[ , nextepistart := NULL]
  
  data_cross_prod <- merge(#data.table::merge.data.table(
    episodes, episodes, 
    by = c("patient_id"),
    all = T,
    allow.cartesian = T
  )
  
  data_cross_prod <- data_cross_prod[
    !(spell_id.x == spell_id.y & 
        episode_start.x == episode_start.y & 
        episode_end.x == episode_end.y)
    ]

  data_cross_prod <- data_cross_prod[
    data.table::between(episode_start.x, episode_start.y, episode_end.y, incbounds = F) |
      data.table::between(episode_end.x, episode_start.y, episode_end.y, incbounds = F) 
    ]
  
  if (nrow(data_cross_prod) > 0) {
    warning(simpleWarning("Hospital spells must not overlap."))
    utils::head(data.table::setorderv(
      data_cross_prod, c("patient_id", "episode_start.x"))
      )
    validation_result <- FALSE
    
  }
  
  validation_result
}


#' Validate inpatient diagnosis records
#' 
#' @description Validate contraints on diagnosis records, namely that the minimum 
#' variables are present, and that all `icd_code` values can be looked up in an 
#' ICD-10 reference table
#' @param diagnoses_data a data frame containing clinical diagnoses, with, at minimum, 
#' variables `patient_id`, `spell_id`, `episode_number`, `icd_code`, `diagnosis_position`
#' @param diagnoses_lookup a data frame containing an ICD-10 reference look up table
#'
#' @return A logical value indicating success
#' @export
#' @importFrom data.table data.table :=
#' @examples
#' lookup_icd <- dplyr::distinct(Ramses::inpatient_diagnoses, icd_code)
#' lookup_icd <- dplyr::filter(lookup_icd, !is.na(icd_code))
#' lookup_icd$icd_label <- "ICD-10 code label text"
#' validate_inpatient_diagnoses(Ramses::inpatient_diagnoses, lookup_icd)
validate_inpatient_diagnoses <- function(diagnoses_data, diagnoses_lookup) {

  exists_non_missing <- list(
    "patient_id", 
    "spell_id", 
    "episode_number",
    "icd_code",
    "diagnosis_position"
  )
 
  validation_result <- !any(
    !sapply(exists_non_missing,
            FUN = validate_variable_no_missing,
            data = diagnoses_data)
  )
  
  validation_result <- validation_result & !any(
    !sapply(list("icd_code", "icd_description", 
                 "icd_display", "category_code", 
                 "category_description"),
            FUN = validate_variable_no_missing,
            data = diagnoses_lookup)
  )
  
  if (validation_result) {
    diagnoses_data <- data.table::data.table(diagnoses_data)
    diagnoses_data <- unique(diagnoses_data[, list(icd_code)])
    diagnoses_lookup <- data.table::data.table(diagnoses_lookup)[, list(icd_code)]
    diagnoses_lookup[, missing := FALSE]
    
    diagnoses_data <- merge(diagnoses_data, diagnoses_lookup, by = "icd_code", all.x = T)
    
    if (any(is.na(diagnoses_data$missing))) {
      warning(
        simpleWarning("some `icd_code` values in `diagnoses_data` do not match any `icd_code` in `diagnoses_lookup`")
      )
    }
  }
  
  validation_result
}

#' Validate medication prescription records
#'
#' @description This function performs a series of checks for mandatory and
#' optional requirements on prescriptions data.
#' @param data a data frame containing one row per prescription
#' @section Mandatory fields:
#' These fields are required in order to pass the validation:
#'   \describe{
#'      \item{\code{patient_id}}{a patient identifier with no missing value}
#'      \item{\code{prescription_id}}{a prescription identifier with no missing value}
#'      \item{\code{prescription_text}}{a character string summarising the prescription 
#'      (to be displayed in user interfaces, eg: \code{'Amoxicillin Oral 500mg BDS'})}
#'      \item{\code{drug_id}}{identifier of the drug (from a dictionary such as SNOMED-CT or
#'       from \code{\link{AMR}{as.ab}()})}
#'      \item{\code{drug_name}}{preferred name of the drug in the drug dictionary}
#'      \item{\code{drug_display_name}}{drug name to display in reports and user interfaces
#'      (can be the same as \code{drug_name})}
#'      \item{\code{ATC_code}}{the ATC code, see \code{\link{AMR}{ab_atc}()}}
#'      \item{\code{ATC_group}}{the ATC group, see \code{\link{AMR}{ab_group1}()}}
#'      \item{\code{ATC_route}}{route of administration as defined in the ATC ("O" = oral; 
#'      "P" = parenteral; "R" = rectal; "V" = vaginal)}
#'      \item{\code{authoring_date}}{timestamp for when the prescription was issued}
#'      \item{\code{prescription_start}}{timestamp for the prescription start}
#'      \item{\code{prescription_end}}{timestamp for the prescription end (mandated except
#'      for one-off prescriptions with \code{daily_frequency} == -1, )}
#'      \item{\code{prescription_context}}{either \code{'inpatient'}, \code{'opat'}, or 
#'      \code{'discharge'}}
#'      \item{\code{prescription_status}}{one value from the following 
#'      \href{https://hl7.org/fhir/R4/valueset-medicationrequest-status.html}{FHIR R4}
#'      reference set:
#'      \itemize{ 
#'         \item \code{`active`} the prescription is 'actionable', but not all actions 
#'         that are implied by it have occurred yet.
#'         \item \code{`on-hold`} actions implied by the prescription are to be 
#'         temporarily halted, but are expected to continue later. 
#'         \item \code{`cancelled`} the prescription has been withdrawn before any 
#'         administrations have occurred.
#'         \item \code{`completed`} all actions that are implied by the prescription 
#'         have occurred.
#'         \item \code{`entered-in-error`} some of the actions that are implied by the 
#'         medication request may have occurred. For example, the medication may have 
#'         been dispensed and the patient may have taken some of the medication. 
#'         Clinical decision support systems should take this status into account.
#'         \item \code{`stopped`} actions implied by the prescription are to be 
#'         permanently halted, before all of the administrations occurred. This should 
#'         not be used if the original order was entered in error.
#'         \item \code{`draft`} the prescription is not yet 'actionable', e.g. it is a 
#'         work in progress, requires sign-off, verification or needs to be run through 
#'         decision support process.
#'         \item \code{`unknown`} the authoring/source system does not know which of the 
#'         status values currently applies for this observation. \emph{Note:} This 
#'         concept is not to be used for 'other' - one of the listed statuses is presumed 
#'         to apply, but the authoring/source system does not know which.         
#'      }}
#'      \item{\code{dose}}{a numeric vector of dosage quantities}
#'      \item{\code{unit}}{a character vector of dosage units}
#'      \item{\code{route}}{the route of administration value natively assigned by system}
#'      \item{\code{daily_frequency}}{a numeric value indicating the number of times the drug 
#'      is to be administered per day. The following values are considered valid:
#'      \itemize{ 
#'         \item -1 for a single one-off administration
#'         \item -9 for 'as required' (\emph{Pro Re Nata}) prescriptions
#'      }}}
#' @section Optional fields:
#' \describe{
#'   \item{\code{combination_id}}{system-issued identifiers for drugs 
#'        prescribed as a bundle to treat the same indication either 
#'        simultaneously (eg clarithromycin and amoxiclav) or consecutively 
#'        (eg doxicycline 200mg followed by 100mg). Unless provided, 
#'        such identifiers will be created by \code{Ramses} using 
#'        transitive closure.}
#'   \item{\code{DDD}}{the number of prescribed defined daily doses, 
#'        see \code{\link{compute_DDDs}()}}
#'   \item{\code{...}}{any other field, as desired, can be loaded into the database}
#' }
#' @return NULL if the `data` passes the validation. The function will trigger errors
#' for mandatory requirements and warnings for optional requirements.
#' @export
validate_prescriptions <- function(data) {
  
  # TODO: prescription id is unique
  # TODO: Indication data should be loaded separately, see \code{\link{load_indications}()}
  # TODO: check uniqueness
  # TODO: check for lonely OOF
  # TODO: validate prescriptions and administrations together... potentially?
  
  drug_prescriptions_variables <- .drug_prescriptions_variables()
  
  variable_exists <- drug_prescriptions_variables[["variable_name"]]
  variable_exists <- variable_exists[which(
    drug_prescriptions_variables[["must_exist"]]
  )]
  
  variable_exists_non_missing <- 
    drug_prescriptions_variables[["variable_name"]]
  variable_exists_non_missing <- variable_exists_non_missing[which(
    drug_prescriptions_variables[["must_be_nonmissing"]]
  )]
  
  not_exist <- !sapply(variable_exists, exists, where = data)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", variable_exists[not_exist], "`"), collapse = ", "))
        )
    )
  }
  
  missing_data <- suppressWarnings(
    !sapply(variable_exists_non_missing, 
            FUN = validate_variable_no_missing,
            data = data)
  )
  
  if( any(missing_data) ){
    stop(
      simpleError(paste(
        "Some variables must not contain missing data:",
        paste(paste0("`", 
                     variable_exists_non_missing[missing_data],
                     "`"), collapse = ", "))
      )
    )
  }
  
  invalid_status <- !data$prescription_status %in% c(
    "active", "on-hold", "cancelled", "completed", 
    "entered-in-error", "stopped", "draft", "unknown"
  )
  
  if( any(invalid_status) ) {
    stop(
      simpleError(
        '`prescription_status` must be one of: "active", "on-hold", "cancelled", "completed", 
    "entered-in-error", "stopped", "draft", or "unknown"'
      )
    )
  }
  
  if( any(is.na(data$prescription_end) & data$daily_frequency != -1) ) {
    stop(
      simpleError(paste(
        "`prescription_end` must contain valid data", 
        "except for one-off prescriptions\n",
        .print_and_capture(utils::head(
          dplyr::select(dplyr::filter(
            data, is.na(prescription_end) & daily_frequency != -1),
            patient_id, daily_frequency, prescription_end)))
    )))
  }
  
  if( !all(data$daily_frequency == -999 | 
           between(data$daily_frequency, 0, 48)) ){
    stop(
      simpleError(paste(
        "Prescription `daily_frequency` must be between",
        "0 and 48, or -999 for PRN 'as required' and one-off prescriptions"
        )))
  }
  
  duplicates <- data %>% 
    dplyr::group_by(patient_id, drug_id, dose, route, prescription_start) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(n > 1)
  duplicates <- merge(data, duplicates)
  
  if( nrow(duplicates) > 0 ) {
    warning(
      simpleWarning("There may be some duplicate records")
    )
    warning(simpleWarning(
      .print_and_capture(utils::head(
        select(dplyr::arrange(duplicates, 
                              patient_id, drug_id, prescription_start),
               patient_id, prescription_id, 
               prescription_text, prescription_start))
    )))
  }
  
  NULL

}



#' Validate medication administration records
#' 
#' @description This function performs a series of checks for mandatory and
#' optional requirements on drug administrations data.
#' @param data a data frame containing one row per drug administration
#' @section Mandatory fields:
#' The following fields are required in order to pass the validation:
#' \describe{
#'      \item{\code{patient_id}}{a patient identifier with no missing value}
#'      \item{\code{prescription_id}}{a prescription identifier with no missing value}
#'      \item{\code{administration_id}}{an administration identifier with no missing value}
#'      \item{\code{administration_text}}{a character string summarising the drug to administer 
#'      (to be displayed in user interfaces, eg: \code{'Amoxicillin Oral 500mg'})}
#'      \item{\code{drug_id}}{identifier of the drug (from a dictionary such as SNOMED-CT or
#'       from \code{\link{AMR}{as.ab}()})}
#'      \item{\code{drug_name}}{preferred name of the drug in the drug dictionary}
#'      \item{\code{drug_display_name}}{drug name to display in reports and user interfaces
#'      (can be the same as \code{drug_name})}
#'      \item{\code{ATC_code}}{the ATC code, see \code{\link{AMR}{ab_atc}()}}
#'      \item{\code{ATC_group}}{the ATC group, see \code{\link{AMR}{ab_group1}()}}
#'      \item{\code{ATC_route}}{route of administration as defined in the ATC ("O" = oral; 
#'      "P" = parenteral; "R" = rectal; "V" = vaginal)}
#'      \item{\code{dose}}{a numeric vector of dosage quantities}
#'      \item{\code{unit}}{a character vector of dosage units}
#'      \item{\code{route}}{the route of administration value natively assigned by system}
#'      \item{\code{administration_date}}{timestamp of the drug administration}
#'      \item{\code{administration_status}}{one value from the following 
#'      \href{https://hl7.org/fhir/R4/valueset-medicationrequest-status.html}{FHIR R4}
#'      reference set:
#'      \itemize{ 
#'         \item \code{`in-progess`} the administration has started but has not yet completed.
#'         \item \code{`not-done`} the administration was terminated prior to any impact on 
#'         the subject (though preparatory actions may have been taken). 
#'         \item \code{`on-hold`} actions implied by the administration have been 
#'         temporarily halted, but are expected to continue later.
#'         \item \code{`completed`} all actions that are implied by the administration 
#'         have occurred.
#'         \item \code{`entered-in-error`} the administration was entered in error and 
#'         therefore nullified. 
#'         \item \code{`stopped`} actions implied by the administration have been permanently 
#'         halted, before all of them occurred.
#'         \item \code{`unknown`} the authoring/source system does not know which of the 
#'         status values currently applies for this request. \emph{Note:} This concept 
#'         is not to be used for 'other' - one of the listed statuses is presumed 
#'         to apply, but the authoring/source system does not know which.         
#' }}}
#' @section Optional fields:
#' \describe{
#'      \item{\code{DDD}}{the number of defined daily doses (as administered),
#'       see \code{\link{compute_DDDs}()}}
#'      \item{\code{...}}{any other field, as desired, can be loaded into the database.}}
#' @return NULL if the `data` passes the validation. The function will trigger errors
#' for mandatory requirements and warnings for optional requirements.
#' @export
validate_administrations <- function(data) {
  
  drug_administrations_variables <- .drug_administrations_variables()
  
  variable_exists <- drug_administrations_variables[["variable_name"]]
  variable_exists <- variable_exists[which(
    drug_administrations_variables[["must_exist"]]
  )]
  
  not_exist <- !sapply(variable_exists, exists, where = data)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", variable_exists[not_exist], "`"), collapse = ", "))
      )
    )
  }
  
  variable_exists_non_missing <- 
    drug_administrations_variables[["variable_name"]]
  variable_exists_non_missing <- variable_exists_non_missing[which(
    drug_administrations_variables[["must_be_nonmissing"]]
  )]
  
  missing_data <- suppressWarnings(
    !sapply(variable_exists_non_missing, 
            FUN = validate_variable_no_missing,
            data = data)
  )
  
  if( any(missing_data) ){
    stop(
      simpleError(paste(
        "Some variables must not contain missing data:",
        paste(paste0("`", 
                     variable_exists_non_missing[missing_data],
                     "`"), collapse = ", "))
      )
    )
  }
  
  invalid_status <- !data$administration_status %in% c(
    "in-progress", "not-done", "on-hold", "completed", 
    "entered-in-error", "stopped", "unknown"
  )
  
  if( any(invalid_status) ) {
    stop(
      simpleError(
        '`prescription_status` must be one of: "in-progress", "not-done", 
        "on-hold", "completed", "entered-in-error", "stopped", or "unknown"'
      )
    )
  }
  
  
  duplicates <- data %>% 
    dplyr::group_by(patient_id, drug_id, dose, route, administration_date) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(n > 1)
  duplicates <- merge(data, duplicates)
  
  if( nrow(duplicates) > 0 ) {
    warning(
      simpleWarning("There may be some duplicate records")
    )
    warning(simpleWarning(
      .print_and_capture(utils::head(
        select(dplyr::arrange(duplicates, 
                              patient_id, drug_id, administration_date),
               patient_id, prescription_id, administration_id,
               administration_text, administration_date))
      )))
  }
  
  NULL
}

arrange_variables <- function(data, first_column_names) {
  other_names <- colnames(data)[!colnames(data) %in% first_column_names]
  data[, c(first_column_names, other_names)]
}

