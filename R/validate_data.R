

#' Verify uniqueness constraints
#' 
#' @description Verifies that values in vectors `col_names` in data frame
#' `data` are unique
#' @param data  a data frame
#' @param col_names a character vector of column names. Column names which do
#' not exist in `data` will be dropped.
#' @return TRUE if the validation is passed.
#' @noRd
.validate_values_unique <- function(data, col_names) {
  
  col_names <- col_names[col_names %in% colnames(data)]
  
  if( length(col_names) == 0 ){
    return(TRUE)
  }
  
  must_be_unique <- subset(data, select = col_names)
  duplicate_results <- sapply(
    must_be_unique,
    function(X) {
      any(duplicated(X))
    }
  )
  
  if( any(duplicate_results) ){
    stop(
      simpleError(paste(
        "The following variables must have unique values:",
        paste(paste0("`", 
                     must_be_unique[duplicate_results],
                     "`"), collapse = ", "))
      )
    )
  }
  
  validation_result <- !all(duplicate_results)
  
  validation_result
}


#' Validate unit codes
#'
#' @param unit_codes a character vector of UCUM unit names to pass
#' \code{\link[units]{as_units}()}. `NA` and `""` values will be dropped.
#'
#' @return TRUE if the validation is passed.
#' @noRd
.validate_UCUM_codes <- function(unit_codes) {
  stopifnot(is.character(unit_codes))
  unit_codes <- na.omit(unit_codes)
  unit_codes <- unit_codes[unit_codes != ""]
  
  units_validate <- list()
  units_validate$validation <- lapply(
    unit_codes,
    function(X){
      try(units::as_units(X), silent = T)
    })
  units_validate$class <- lapply(
    units_validate$validation,
    class
  )
  units_validate$errors <- which(units_validate$class == "try-error")
  
  if( any(units_validate$class != "units") ){
    stop(paste(
      units_validate$validation[units_validate$errors],
      sep = "\n"
    ))
  }
  
  return(TRUE)
}


#' Verify variables exist in the data
#'
#' @param data a data frame
#' @param vectorname a character vector of variables which must exist.
#' @param action whether to throw `"warning"` or `"error"`. Default is "warning"
#'
#' @return TRUE if validation is passed
#' @noRd
.validate_variable_exist <- function(
  data,
  vectorname,
  action = "warning"
) {
  stopifnot(action %in% c("warning", "error"))
  stopifnot(is.character(vectorname))
  if( length(vectorname) == 0 ) {
    return(TRUE)
  }
  
  not_exist <- !sapply(vectorname, exists, where = data)
  if( any(not_exist) & action == "warning"){
    warning(
      simpleWarning(paste(
        "The following variables must exist:",
        paste(paste0("`", vectorname[not_exist], "`"), collapse = ", "))
      )
    )
  } else if( any(not_exist) & action == "error"){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", vectorname[not_exist], "`"), collapse = ", "))
      )
    )
  } else {
    return(TRUE)
  }
}

#' Verify no missing or "" value
#'
#' @param data a data frame
#' @param vectorname a character vector of variables which must not be missing.
#' Variables that are not present in `data` will be dropped.
#' @param action whether to throw `"warning"` or `"error"`. Default is "warning"
#'
#' @return TRUE if validation is passed
#' @noRd
.validate_variable_no_missing <- function(data, 
                                          vectorname, 
                                          action = "warning") {
  
  stopifnot(action %in% c("warning", "error"))
  stopifnot(is.character(vectorname))
  vectorname <- vectorname[vectorname %in% colnames(data)]
  if( length(vectorname) == 0 ) {
    return(TRUE)
  }
  
  missing_data <- sapply(
    vectorname, 
    function(var, data) {
      any(is.na(data[, var]))
    },
    data = data
  )
  
  empty_data <- sapply(
    vectorname, 
    function(var, data) {
      any(as.character(na.omit(data[, var])) == "")
    },
    data = data
  )
  
  if( any(missing_data) & action == "warning"){
    warning(
      paste(
        "The following variables contain missing data:",
        paste(paste0("`", 
                     vectorname[missing_data],
                     "`"), collapse = ", ")
      ))
  }
  
  if( any(missing_data) & action == "error"){
    stop(
      paste(
        "The following variables must not contain missing data:",
        paste(paste0("`", 
                     vectorname[missing_data],
                     "`"), collapse = ", ")
      ))
  }
  
  
  if( any(empty_data) & action == "warning" ){
    warning(
      paste(
        "The following variables contain \"\" values:",
        paste(paste0("`", 
                     vectorname[empty_data],
                     "`"), collapse = ", ")
      ))
  }
  
  if( any(empty_data) & action == "error" ){
    stop(
      paste(
        "The following variables must not equal \"\":",
        paste(paste0("`", 
                     vectorname[empty_data],
                     "`"), collapse = ", ")
      ))
  }
  
  !any(empty_data, missing_data)
}



#' Validate inpatient episode and ward movement records
#'
#' @description This function performs a series of checks for mandatory and
#' optional requirements on episodes of care records. The data definitions 
#' closely follow those of the  
#' \href{https://www.datadictionary.nhs.uk/data_dictionary/messages/cds_v6-2/data_sets/cds_v6-2_type_130_-_admitted_patient_care_-_finished_general_episode_cds_fr.asp}{English NHS Admitted Patient Care Commissioning Datasets}.
#' @param episodes data frame containing one row per episode of care
#' @param wards (optional) data frame containing one row per ward stay. 
#'   Default is `NULL`.
#' @section Episode mandatory variables:
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
#' @section Episode optional variables:
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
validate_inpatient_episodes <- function(episodes,
                                        wards = NULL) {
  
  episode_schema <- .inpatient_episodes_variables()
  
  variable_exists <- episode_schema[episode_schema$must_exist, "variable_name"]
  not_exist <- !sapply(variable_exists, exists, where = episodes)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", variable_exists[not_exist], "`"), collapse = ", "))
      )
    )
  }
  
  variable_exists_non_missing <- episode_schema[
    episode_schema$must_be_nonmissing, 
    "variable_name"]
  no_missing_data <- .validate_variable_no_missing(
    data = episodes,
    vectorname = variable_exists_non_missing,
    action = "error"
  )
  
  validation_result <- validate_inpatient_spells(episodes)
  validation_result <- append(
    validate_inpatient_episode_dates(data = episodes,
                                     type = "episodes"), 
    validation_result)
  
  if(!is.null(wards)) {
    validation_result <- append(
      .validate_inpatient_wards(episodes, wards), 
      validation_result)
  }
 
  !any(!validation_result)
}


.validate_inpatient_wards <- function(episodes, wards) {
  
  ward_schema <- .inpatient_wards_variables()
  
  variable_exists <- ward_schema[ward_schema$must_exist, "variable_name"]
  not_exist <- !sapply(variable_exists, exists, where = wards)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", variable_exists[not_exist], "`"), collapse = ", "))
      )
    )
  }
  
  variable_exists_non_missing <- ward_schema[
    ward_schema$must_be_nonmissing, 
    "variable_name"]
  no_missing_data <- .validate_variable_no_missing(
    data = wards,
    vectorname = variable_exists_non_missing,
    action = "error"
  )
  
  wards <- merge(
    wards, 
    distinct(episodes, 
             patient_id, 
             spell_id, 
             admission_date, 
             discharge_date), 
    all.x = TRUE)
  
  validate_inpatient_episode_dates(data = wards,
                                   type = "wards")
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
#' @param type a string indicating the type of dates to validate:
#' either `"wards"` for ward stays or `"episodes"` for inpatient episodes.
#' @importFrom data.table data.table := 
#' @return A logical value indicating success
validate_inpatient_episode_dates <- function(data, type = "episodes") {
  
  if(type == "episodes") {
    data$start <- data[["episode_start"]]
    data$end <- data[["episode_end"]]
  } else if(type == "wards") {
    data$start <- data[["ward_start"]]
    data$end <- data[["ward_end"]]
  } else {
    stop("`type` must be 'episodes' or 'wards'.")
  }
  
  validation_result <- TRUE
  if (any(data$start > data$end)) {
    warning(simpleWarning(paste0("Some `", type, "` start dates are posterior to end dates.")))
    validation_result <- FALSE
  }
  
  if (any(!(data$start >= data$admission_date & data$start <= data$discharge_date))) {
    warning(simpleWarning(paste0("Some `", type, "` fall outside hospitalisation dates.")))
    validation_result <- FALSE
  }
  
  if (any(!(data$end >= data$admission_date & data$end <= data$discharge_date))) {
    warning(simpleWarning(paste0("Some `", type, "` fall outside hospitalisation dates.")))
    validation_result <- FALSE
  }
  
  BD_spell <-  BD_episode <-  nextepistart <-  NULL
  
  episodes <- data.table::as.data.table(data)
  
  bed_day_matching <- episodes[,
    list(BD_episode = sum(difftime(end, start, units = "secs"))), 
    by = list(patient_id, spell_id, admission_date, discharge_date)
    ]
  bed_day_matching[, BD_spell := difftime(discharge_date, admission_date, units = "secs")]
  
  if (nrow(bed_day_matching[abs(BD_episode - BD_spell) > 5]) > 0) {
    warning(simpleWarning(paste0(
      "Total bed days calculated from `", type,"` does not",
      "match admission duration. Bed days may be incorrect.")))
  }
  
  rm(bed_day_matching)
  
  episodes <- episodes[, list(patient_id, spell_id, start, end)]
  
  episodes <- data.table::setorderv(
    episodes, c("patient_id", "spell_id", "start"))
  
  episodes[ , `:=`(nextepistart =  data.table::shift(start, type = "lead")), 
            by = list(patient_id, spell_id)]
  
  if (nrow(episodes[!is.na(nextepistart) & nextepistart != end]) > 0) {
    warning(simpleWarning(paste0(
      "Some hospital spells have gaps between `", type,"`.",
      "Bed days may be underestimated.")))
  }

  episodes[ , nextepistart := NULL]
  
  data_cross_prod <- merge(
    episodes, episodes, 
    by = c("patient_id"),
    all = T,
    allow.cartesian = T
  )
  
  data_cross_prod <- data_cross_prod[
    !(spell_id.x == spell_id.y & 
        start.x == start.y & 
        end.x == end.y)
    ]

  data_cross_prod <- data_cross_prod[
    data.table::between(start.x, start.y, end.y, incbounds = F) |
      data.table::between(end.x, start.y, end.y, incbounds = F) 
    ]
  
  if (nrow(data_cross_prod) > 0) {
    warning(simpleWarning(paste0("Hospital `", type,"` must not overlap.")))
    utils::head(data.table::setorderv(
      data_cross_prod, c("patient_id", "start.x"))
      )
    validation_result <- FALSE
    
  }
  
  validation_result
}


#' Validate inpatient diagnosis records
#' 
#' @description Validate constraints on diagnosis records, namely that the 
#' minimum variables are present, and that all \code{icd_code} values can be 
#' looked up in an ICD-10 reference table
#' @param diagnoses_data a data frame containing clinical diagnoses, with, 
#' at minimum, variables \code{patient_id}, \code{spell_id}, 
#' \code{episode_number}, \code{icd_code}, \code{diagnosis_position}
#' @param diagnoses_lookup a data frame containing an ICD-10 reference look up 
#' table with, at minimum, variables `icd_description`, `icd_display`, 
#' `category_code`, `category_description`
#'
#' @return A logical value indicating success
#' @export
#' @importFrom data.table data.table :=
#' @examples
#' data_icd <- dplyr::filter(Ramses::inpatient_diagnoses, !is.na(icd_code))
#' lookup_icd <- dplyr::distinct(data_icd, icd_code)
#' lookup_icd$icd_display <- lookup_icd$icd_code
#' lookup_icd$icd_description <- "ICD-10 code label text"
#' lookup_icd$category_code <- substr(lookup_icd$icd_code, 0, 3)
#' lookup_icd$category_description <- "ICD-10 category label text"
#' validate_inpatient_diagnoses(data_icd, lookup_icd)
validate_inpatient_diagnoses <- function(diagnoses_data, diagnoses_lookup) {

  diagnoses_data_schema <- .inpatient_diagnoses_data_variables()
  diagnoses_lookup_schema <- .inpatient_diagnoses_lookup_variables()
  
  data_var_exists <- diagnoses_data_schema[
    diagnoses_data_schema[["must_exist"]],
    "variable_name"
  ]
  not_exist <- !sapply(data_var_exists, exists, where = diagnoses_data)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", data_var_exists[not_exist], "`"), collapse = ", "))
      )
    )
  }
  
  lkup_var_exists <- diagnoses_lookup_schema[
    diagnoses_lookup_schema[["must_exist"]],
    "variable_name"
  ]
  not_exist <- !sapply(lkup_var_exists, exists, where = diagnoses_lookup)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", lkup_var_exists[not_exist], "`"), collapse = ", "))
      )
    )
  }

  validation_result <- .validate_variable_no_missing(
    data = diagnoses_data,
    vectorname = diagnoses_data_schema[
      diagnoses_data_schema[["must_be_nonmissing"]],
      "variable_name"
    ],
    action = "warning"
  )
  
  validation_result <- validation_result & 
    .validate_variable_no_missing(
    data = diagnoses_lookup,
    vectorname = diagnoses_lookup_schema[
      diagnoses_lookup_schema[["must_be_nonmissing"]],
      "variable_name"
    ],
    action = "error"
  )
  
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
#'      \item{\code{drug_id}}{identifier of the drug (from a dictionary such as SNOMED CT or
#'       from \code{\link{AMR}{as.ab}()})}
#'      \item{\code{drug_name}}{preferred name of the drug in the drug dictionary}
#'      \item{\code{drug_display_name}}{drug name to display in reports and user interfaces
#'      (can be the same as \code{drug_name})}
#'      \item{\code{antiinfective_type}}{type of antiinfective ("antibacterial", "antifungal",
#'      "antiviral", or "antiparasitic")}
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
#'      }.
#'      \emph{Note that prescriptions marked as \code{"on-hold"}, \code{"cancelled"}, 
#'      \code{"draft"}, \code{"entered-in-error"}, or \code{"unknown"} will not 
#'      count towards antibiotic consumption estimates.}}
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
  
  variable_exists <- drug_prescriptions_variables[
    drug_prescriptions_variables[["must_exist"]],
    "variable_name"
  ]
  variable_exists_non_missing <- drug_prescriptions_variables[
    drug_prescriptions_variables[["must_be_nonmissing"]],
    "variable_name"
  ]

  not_exist <- !sapply(variable_exists, exists, where = data)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", variable_exists[not_exist], "`"), collapse = ", "))
        )
    )
  }
  
  missing_data <- .validate_variable_no_missing(
    data = data,
    vectorname = variable_exists_non_missing,
    action = "error"
  )
 
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
  
  if( any(
    !data$antiinfective_type %in% c(
      "antibacterial",
      "antifungal",
      "antiviral",
      "antiparasitic"
    )
  )) {
    stop(paste(
      '`antiinfective_type` must be one of: "antibacterial", "antifungal",',
      '"antiviral", or "antiparasitic"'))
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
#'      \item{\code{drug_id}}{identifier of the drug (from a dictionary such as SNOMED CT or
#'       from \code{\link{AMR}{as.ab}()})}
#'      \item{\code{drug_name}}{preferred name of the drug in the drug dictionary}
#'      \item{\code{drug_display_name}}{drug name to display in reports and user interfaces
#'      (can be the same as \code{drug_name})}
#'      \item{\code{antiinfective_type}}{type of antiinfective ("antibacterial", "antifungal",
#'      "antiviral", or "antiparasitic")}
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
  
  variable_exists <- drug_administrations_variables[
    drug_administrations_variables[["must_exist"]],
    "variable_name"
  ]
  
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
  
  missing_data <- .validate_variable_no_missing(
    data = data,
    vectorname = variable_exists_non_missing,
    action = "error"
  )
  
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
  
  if( any(
    !data$antiinfective_type %in% c(
      "antibacterial",
      "antifungal",
      "antiviral",
      "antiparasitic"
    )
  )) {
    stop(paste(
      '`antiinfective_type` must be one of: "antibacterial", "antifungal",',
      '"antiviral", or "antiparasitic"'))
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

#' Validate microbial isolates & susceptibility records
#'
#' @param specimens a data frame with one row per specimen sent
#' to laboratory (see details)
#' @param isolates a data frame with one row per microorganism
#' isolated from the laboratory specimen (see details)
#' @param susceptibilities a data frame with one row per susceptibility
#' (see details)
#' 
#' @section \code{specimens} data frame:
#' \emph{The following fields are mandatory:}
#' \describe{
#'    \item{\code{specimen_id}}{a unique specimen identifier with no missing value}
#'    \item{\code{patient_id}}{a patient identifier with no missing value}
#'    \item{\code{status}}{one value from the following 
#'    \href{https://www.hl7.org/fhir/valueset-specimen-status.html}{FHIR R4}
#'      reference set: \itemize{
#'        \item \code{"available"}: The physical specimen is present and 
#'        in good condition.
#'        \item \code{"unavailable"}: There is no physical specimen because it is either 
#'        lost, destroyed or consumed.
#'        \item \code{"unsatisfactory"}: The specimen cannot be used because of a
#'         quality issue such as a broken container, contamination, or too old.	
#'        \item \code{"entered-in-error"}: The specimen was entered in error and 
#'        therefore nullified.
#'      }}
#'    \item{\code{specimen_datetime}}{datetime when specimen was sampled or 
#'    received for processing.}
#'    \item{\code{specimen_type_code}}{character vector of descendants of 
#'    the SNOMED CT concept \code{123038009 | Specimen (specimen) |}. Admissible
#'    values are listed in \link[Ramses]{reference_specimen_type}}
#'    \item{\code{specimen_type_name}}{character vector of the SNOMED CT
#'    preferred terms for \code{specimen_type_code}}
#'    \item{\code{specimen_type_display}}{character vector of custom specimen
#'    types for display in user interfaces}
#' }
#' 
#' \emph{The following fields are optional:}
#' \describe{
#'    \item{\code{spell_id}}{a hospital spell identifier (if the specimen was 
#'    sampled during admission)}
#'    \item{\code{test_display}}{free text description of test requested for 
#'    display in user interfaces. For instance: "Mycobacteria culture" or 
#'    "Microbial culture, anaerobic, initial isolation". Coded concepts
#'    should be stored in other custom columns.}
#'    \item{\code{reason_display}}{free text reason for a procedure for display 
#'    in user interfaces. For instance, "Suspected urinary tract infection", or
#'    "Surgical site microbiological sample". Coded concepts should be stored in other 
#'    custom columns.}
#' }
#'    
#' @section \code{isolates} data frame:
#' 
#' \emph{The following fields are mandatory:}
#' \describe{
#'    \item{\code{organism_id}}{a unique isolated organism identifier with no missing value}
#'    \item{\code{specimen_id}}{a specimen identifier with no missing value}
#'    \item{\code{patient_id}}{a patient identifier with no missing value}
#'    \item{\code{organism_code}}{a character vector containing either: \itemize{
#'       \item a microorganism code validated using \code{\link[AMR]{as.mo}()}
#'       \item \code{NA_character_} if no microorganism was isolated, for instance
#'       due to no growth or mixed heavy growth
#'    }}
#'    \item{\code{organism_name}}{a microorganism name provided by 
#'    \code{\link[AMR]{mo_name}()}, or \code{NA} if no microorganism was isolated}
#'    \item{\code{organism_display_name}}{microorganism name as labelled by the
#'    laboratory, for display in user interfaces. No growth/mixed heavy growth should
#'    be referenced here}
#'    \item{\code{isolation_datetime}}{datetime when the organism was first isolated 
#'    (or reported) by the laboratory}
#'  }
#' 
#' \emph{The following field is optional:}
#' \describe{
#'    \item{\code{mdr_classification}}{character vector of classifications produced by 
#'    \code{\link[AMR]{mdro}}. Admissible values are: \itemize{
#'       \item \code{NA_character_} when susceptibilities are not available/conclusive
#'       \item \code{"Negative"} for isolate presenting no wide resistance phenotype
#'       \item other character codes dependent on the \code{guideline} parameter 
#'       provided to \code{\link[AMR]{mdro}}
#'    }}
#' }  
#'  
#' @section \code{susceptibilities} data frame:
#' 
#' \emph{The following fields are mandatory:}
#' \describe{
#'   \item{\code{organism_id}}{an isolated organism identifier with no missing value}
#'   \item{\code{specimen_id}}{a specimen identifier with no missing value}
#'   \item{\code{patient_id}}{a patient identifier with no missing value}
#'   \item{\code{organism_code}}{a microorganism code validated using 
#'   \code{\link[AMR]{as.mo}()}, with no missing values}
#'   \item{\code{organism_name}}{a microorganism name provided by 
#'   \code{\link[AMR]{mo_name}()}, with no missing values}
#'   \item{\code{organism_display_name}}{microorganism name as labelled by the
#'   laboratory, for display in user interfaces, with no missing values}
#'   \item{\code{drug_id}}{code of the antimicrobial tested as provided by 
#'   \code{\link[AMR]{as.ab}()}}
#'   \item{\code{drug_name}}{name of the antimicrobial tested as provided by 
#'   \code{\link[AMR]{ab_name}()}}
#'   \item{\code{drug_display_name}}{name of the antimicrobial tested, 
#'   with no missing values, for  display in reports and user interfaces
#'   (can be the same as \code{drug_name})}
#'   \item{\code{rsi_code}}{\code{"R"} (resistant), \code{"S"} (susceptible), 
#'    or \code{"I"} (intermediate exposure), as determined by the laboratory or by
#'   \code{\link[AMR]{as.rsi}()} on the basis of minimum inhibitory concentrations
#'   or disk diffusion diameters}
#' }
#' 
#' @return TRUE if the validation is passed
#' @export
validate_microbiology <- function(specimens, isolates, susceptibilities) {
  
  schema <- .inpatient_microbiology_variables()
  
  forgetme <- .validate_variable_exist(
    data = specimens,
    vectorname = schema$specimens$variable_name[
      schema$specimens$must_exist
    ],
    action = "error"
  )
  forgetme <- .validate_variable_exist(
    data = isolates,
    vectorname = schema$isolates$variable_name[
      schema$isolates$must_exist
    ],
    action = "error"
  )
  forgetme <- .validate_variable_exist(
    data = susceptibilities,
    vectorname = schema$susceptibilities$variable_name[
      schema$susceptibilities$must_exist
    ],
    action = "error"
  )
  
  forgetme <- .validate_variable_no_missing(
    specimens, 
    schema$specimens$variable_name[schema$specimens$must_be_nonmissing])
  forgetme <- .validate_variable_no_missing(
    isolates, 
    schema$isolates$variable_name[schema$isolates$must_be_nonmissing])
  forgetme <- .validate_variable_no_missing(
    susceptibilities, 
    schema$susceptibilities$variable_name[schema$susceptibilities$must_be_nonmissing])
  
  forgetme <- .validate_values_unique(
    specimens,
    schema$specimens$variable_name[schema$specimens$must_be_unique]
  )
  forgetme <- .validate_values_unique(
    isolates,
    schema$isolates$variable_name[schema$isolates$must_be_unique]
  )
  .validate_values_unique(
    susceptibilities,
    schema$susceptibilities$variable_name[schema$susceptibilities$must_be_unique]
  )
  
  invalid_specimen_codes <- !(specimens$specimen_type_code %in% reference_specimen_type$conceptId)
  if( any(invalid_specimen_codes) ) {
    warning(paste(
      c("Some values in `specimen_type_code` are not valid SNOMED CT specimen concepts:",
        paste(utils::head(unique(specimens$specimen_type_code[invalid_specimen_codes])), collapse = ", ")),
      collapse = "\n"
    ))
  }
  
  if( any(!susceptibilities$organism_id %in% isolates$organism_id) ) {
    stop("Some `organism_id` in `susceptibility` are missing from `isolates`")
  }
  if( any(!susceptibilities$specimen_id %in% isolates$specimen_id) ) {
    stop("Some `specimen_id` in `susceptibility` are missing from `isolates`")
  }
  if( any(!susceptibilities$specimen_id %in% specimens$specimen_id) ) {
    stop("Some `specimen_id` in `susceptibility` are missing from `specimens`")
  }
  if( any(!isolates$specimen_id %in% specimens$specimen_id) ) {
    stop("Some `specimen_id` in `isolates` are missing from `specimens`")
  }
  
  stopifnot(is(specimens$specimen_datetime, "POSIXt"))
  stopifnot(is(isolates$isolation_datetime, "POSIXt"))
  
  invalid_organism_codes <- na.omit(unique(c(isolates$organism_code,
                                     susceptibilities$organism_code)))
  invalid_organism_codes <- invalid_organism_codes[
    !invalid_organism_codes %in% AMR::microorganisms.codes$mo
  ]
  if( length(invalid_organism_codes) > 0 ) {
    stop(paste(
      "Some `organism_code` values are invalid:",
      paste(utils::head(invalid_organism_codes), collapse = ", "),
      collapse = "\n"
    ))
  }
  
  invalid_drug_codes <- na.omit(unique(c(isolates$drug_id,
                                 susceptibilities$drug_id)))
  invalid_drug_codes <- invalid_drug_codes[
    !invalid_drug_codes %in% AMR::antibiotics$ab
  ]
  if( length(invalid_drug_codes) > 0 ) {
    stop(paste(
      "Some `organism_code` values are invalid:",
      paste(utils::head(invalid_drug_codes), collapse = ", "),
      collapse = "\n"
    ))
  }
  
  TRUE
}


#' Validate records of observations & investigations
#'
#' @param investigations a data frame
#' @param custom_units a character vector of valid unit codes not listed in
#' the UCUM. Default is: \code{c("breaths", "beats", "U")}.
#' @return TRUE if the validation is passed
#' @section Mandatory variables:
#' The following variables are required:
#' \describe{
#'    \item{\code{"observation_id"}}{a unique identifier with no missing value}
#'    \item{\code{"patient_id"}}{a patient identifier with no missing value}
#'    \item{\code{"spell_id"}}{identifier of the hospital spell during which 
#'    the investigation was performed (if observations are made during admission)}
#'    \item{\code{"status"}}{Codes from the following value set 
#'    \url{http://hl7.org/fhir/observation-status} \itemize{
#'   \item \code{"registered"}: The existence of the observation is registered, but there is no result yet available.
#'   \item \code{"preliminary"}: his is an initial or interim observation: data may be incomplete or unverified.
#'   \item \code{"final"}: The observation is complete and there are no further actions needed. 
#'   \item \code{"amended"}: Subsequent to being Final, the observation has been modified 
#'   subsequent. This includes updates/new information and corrections.
#'   \item \code{"corrected"}: Subsequent to being Final, the observation has been modified to 
#'   correct an error in the test result.
#'   \item \code{"cancelled"}: The observation is unavailable because the 
#'   measurement was not started or not completed.
#'   \item \code{"entered-in-error"}: The observation has been withdrawn following previous 
#'   final release. This electronic record should never have existed, though it is possible 
#'   that real-world decisions were based on it. (If real-world activity has occurred, 
#'   the status should be "cancelled" rather than "entered-in-error".).
#'   \item \code{"unknown"}: The authoring/source system does not know which of the status 
#'   values currently applies for this observation. Note: This concept is not to be used for 
#'   "other" - one of the listed statuses is presumed to apply, but the authoring/source 
#'   system does not know which.
#'   }}
#'    \item{\code{"request_datetime"}}{a datetime when the observation was 
#'    requested with no missing value}
#'    \item{\code{"observation_datetime"}}{a datetime when the investigation 
#'    was performed with no missing value}
#'    \item{\code{"observation_code_system"}}{URL of the code system (for instance: 
#'    "http://snomed.info/sct", "http://loinc.org")}
#'    \item{\code{"observation_code"}}{LOINC concept code or SNOMED-CT concept 
#'    code corresponding to a SNOMED CT observable entity or evaluation procedure}
#'    \item{\code{"observation_name"}}{code system name for the observation}
#'    \item{\code{"observation_display"}}{observation name to display}
#'    \item{\code{"observation_value_text"}}{observation string value or codable
#'    concept, for example: TRUE/FALSE, Yes/No, SNOMED CT qualifier value}
#'    \item{\code{"observation_value_numeric"}}{observation numeric value}
#'    \item{\code{"observation_unit"}}{a unit code passing 
#'    \code{\link[units]{as_units}()}. See examples. 
#'    See also: \code{\link[units]{valid_udunits}}, 
#'    \code{\link[units]{install_symbolic_unit}}
#'    \url{http://unitsofmeasure.org}}
#' }
#' @export 
#' @examples 
#' # the units "breaths/min" (http://loinc.org/8867-4) or
#' # "beats/min" () do not exist in the https://ucum.org/. 
#' library(units)
#' \dontrun{as_units("breaths/min")} # fails
#' 
#' # Yet, they may be declared.
#' install_symbolic_unit("breaths") 
#' as_units("breaths/min") # succeeds
validate_investigations <- function(investigations, 
                                    custom_units = c("breaths",
                                                     "beats",
                                                     "U")) {
  
  investigation_schema <- .inpatient_investigations_variables()
  
  variable_exists <- investigation_schema[
    investigation_schema[["must_exist"]],
    "variable_name"]

  not_exist <- !sapply(variable_exists, exists, where = investigations)
  if( any(not_exist) ){
    stop(
      simpleError(paste(
        "The following variables must exist:",
        paste(paste0("`", variable_exists[not_exist], "`"), collapse = ", "))
      )
    )
  }
  
  exists_non_missing <- investigation_schema[
    investigation_schema$must_be_nonmissing,
    "variable_name"]
  missing_data <- suppressWarnings(
    !sapply(exists_non_missing, 
            FUN = .validate_variable_no_missing,
            data = investigations)
  )
  if( any(missing_data) ){
    stop(
      paste(
        "The following variables must not contain missing data:",
        paste(paste0("`", 
                     exists_non_missing[missing_data],
                     "`"), collapse = ", ")
        ))
  }
  
  must_be_unique <- .validate_values_unique(
    investigations,
    investigation_schema[
    investigation_schema$must_be_unique, 
    "variable_name"])
  
  if( !is.null(custom_units) ){
    custom_units <- custom_units[custom_units != ""]
    for (unit in custom_units) {
      units::install_symbolic_unit(unit, warn = FALSE)
    }
  }
  
  units_validate <- .validate_UCUM_codes(unique(investigations$observation_unit))
  
  all(!not_exist, !missing_data, must_be_unique, units_validate)
}

arrange_variables <- function(data, first_column_names) {
  other_names <- colnames(data)[!colnames(data) %in% first_column_names]
  data[, c(first_column_names, other_names)]
}
