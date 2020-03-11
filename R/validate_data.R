
global_vars <- c(
  "patient_id", 
  "spell_id", 
  "admission_date", 
  "discharge_date",
  "episode_number",
  "last_episode_in_spell_indicator",
  "episode_start",
  "episode_end",
  "DDD",
  "ddd_value",
  "ddd_unit",
  "valid_ab",
  "icd_code"
)

utils::globalVariables(global_vars)
utils::globalVariables(paste0(global_vars, ".x"))
utils::globalVariables(paste0(global_vars, ".y"))


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
#' @param data a data frame containing 
#'
#' @return TODO
#' @export
validate_inpatient_episodes <- function(data) {

  exists_non_missing <- list(
    "patient_id", 
    "spell_id", 
    "admission_date", 
    "discharge_date",
    "episode_number",
    "last_episode_in_spell_indicator",
    "episode_start",
    "episode_end"
  )
  
  validation_result <- !any(
    !sapply(exists_non_missing,
            FUN = validate_variable_no_missing,
            data = data)
    )
  
  if (validation_result) {
    validation_result <- append(validate_inpatient_spells(data), validation_result)
    validation_result <- append(validate_inpatient_episode_dates(data), validation_result)
  }
  
  !any(!validation_result)
}

#' Validate the consistency of admission and discharge dates
#'
#' @param data a data frame object
#' @importFrom data.table data.table
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
#' @return a boolean indicating whether the data passed the set of validations
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
    !sapply(list("icd_code", "icd_label"),
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
        simpleWarning("all `icd_code` values in `diagnoses_data` must match an `icd_code` in `diagnoses_lookup`")
      )
    }
  }
  
  validation_result
}

#' Validate medication prescription records
#'
#' @param data a data frame containing one row per prescription. See Details for other conditions.
#'
#' @details 
#'   In order to pass the validation, prescription data should contain the following columns with 
#'   no missing data:
#'   \itemize{
#'      \item \code{patient_id}: a patient identifier with no missing value
#'      \item \code{prescription_id}: a prescription identifier with no missing value
#'      \item \code{drug_id}: identifier of the drug (from a dictionary such as SNOMED-CT or
#'       from \code{\link{AMR}{as.ab}()})
#'      \item \code{drug_name}: preferred name of the drug in the drug dictionary
#'      \item \code{drug_display_name}: drug name to display in reports (can be the same as \code{drug_name})
#'      \item \code{ATC_code}
#'      \item \code{ATC_group}
#'      \item \code{ATC_route}
#'      \item \code{authoring_date}: timestamp when the prescription was issued
#'      \item \code{prescription_start}: timestamp of the prescription start
#'      \item \code{prescription_end}: timestamp of the prescription end
#'      \item \code{prescription_context}: either 'inpatient', 'opat', 'discharge'
#'     }
#'    
#'   Optional columns include:
#'   \itemize{
#'      \item \code{ddd}: the number of defined daily doses, see \code{\link{compute_DDDs}()}.
#'   }
#'   
#' @return NULL if the dataset is valid
#' @export
validate_prescriptions <- function(data) {
  
  # TODO: Indication data should be loaded separately, see \code{\link{load_indications}()}
  # TODO: write validate_prescriptions() and check uniqueness
  # TODO: check for lonely OOF
  # TODO: validate prescriptions and administrations together... potentially?
  exists_non_missing <- list(
    "patient_id", 
    "prescription_id", 
    "drug_id", 
    "drug_name",
    "drug_display_name",
    "ATC_code",
    "ATC_group",
    "prescription_text",
    "prescription_status",
    "prescription_context",
    "authoring_date",
    "prescription_start",
    "prescription_end",
    "dose",
    "unit",
    "frequency",
    "daily_frequency",
    "route",
    "authoring_date",
    "prescription_start",
    "prescription_end",
    "ddd"
  )
  
  !any(!sapply(exists_non_missing, 
               FUN = validate_variable_no_missing,
               data = data))
  

}



#' Validate medication administration records
#'
#' @param data a data frame containing one row per prescription. See Details for other conditions.
#'
#' @details 
#'   In order to pass the validation, prescription data should contain the following columns with 
#'   no missing data:
#'   \itemize{
#'      \item \code{patient_id}: a patient identifier with no missing value
#'      \item \code{prescription_id}: a prescription identifier with no missing value
#'      \item \code{administration_id}: an administration identifier with no missing value
#'      \item \code{drug_id}: identifier of the drug (from a dictionary such as SNOMED-CT or
#'       from \code{\link{AMR}{as.ab}()})
#'      \item \code{drug_name}: preferred name of the drug in the drug dictionary
#'      \item \code{drug_display_name}: drug name to display in reports (can be the same as \code{drug_name})
#'      \item \code{ATC_code}
#'      \item \code{ATC_group}
#'      \item \code{ATC_route}
#'      \item \code{administration_time}: timestamp of the drug administration
#'      \item \code{administration_context}: either 'inpatient', 'opat', 'discharge'
#'     }
#'    
#'   Optional columns include:
#'   \itemize{
#'      \item \code{dose}: dose administered
#'      \item \code{unit}: dose unit, see \code{\link[units]{ud_units}}
#'      \item \code{route}: route of administration of the drug
#'      \item \code{ddd}: the number of defined daily doses, see \code{\link{compute_DDDs}()}.
#'   }
#'   
#' @return NULL if the dataset is valid
#' @export
validate_administrations <- function(data) {
}

arrange_variables <- function(data, first_column_names) {
  other_names <- colnames(data)[!colnames(data) %in% first_column_names]
  data[, c(first_column_names, other_names)]
}
