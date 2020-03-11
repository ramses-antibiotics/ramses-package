
#' Download the ICD-10-CM reference file by the US National Center for 
#' Health Statistics
#'
#' @return A data frame containing ICD-10-CM codes 
#' (\code{icd_code}) and labels (\code{icd_desc})
#' @export
download_icd10cm <- function() {
  icd10cm_url <- "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2020-ICD-10-CM-Codes.zip"
  icd10cm_file <- tempfile()
  utils::download.file(url = icd10cm_url, destfile = icd10cm_file)
  icd10cm_file <- unz(
    icd10cm_file, 
    grep("icd10cm_codes_[0-9]{4}.txt$", 
         utils::unzip(icd10cm_file, list = T)$Name, 
         value = T))
  icd10cm <- utils::read.fwf(file = icd10cm_file, 
                             widths = c(8, 250), header = F,
                             col.names = c("icd_code", "icd_label"), 
                             stringsAsFactor = FALSE)
  icd10cm$icd_code <- trimws(icd10cm$icd_code)
  
  icd10cm
}

#' Map ICD-10 to Charlson comorbidities and weights
#'
#' @description Map ICD-10 or ICD-10-CM codes to a list of chronic
#' comorbidities, comorbidity groups, and Charlson Comorbidity Index 
#' weights from \insertCite{Quan2005}{Ramses}.
#' @param df a data frame containing ICD-10 look up data
#' @param icd_column  a variable name or column number in `df` 
#' containing ICD-10 codes.
#' @return The input data frame `df` enhanced with the following 
#' variables:
#'   \itemize{
#'     \item \code{comorb} a character vector of 17 comorbidity 
#'     codes (adapted from the 
#'     \code{\link[comorbidity]{comorbidity}} package)
#'     \item \code{comorb_group} a character vector of codes 
#'     grouping \code{comorb} codes into 14 categories 
#'     collapsing different severities of the same condition 
#'     (for diabetes, liver disease, cancer). This is to avoid 
#'     double counting comorbidities.
#'     \item \code{charlson_weights} an integer vector of weights 
#'     from \insertCite{Quan2005}{Ramses} used to compute the 
#'     weighted Charlson comorbidity score
#'   }
#' @export
#' @source Adapted from \code{\link[comorbidity]{comorbidity}()}
#' @references \insertAllCited{}
#' @examples
#' mock_icd_data <- data.frame(
#'    list(icd10_code = c("J44", "J44X", "J440")), 
#'    stringsAsFactors = FALSE)
#' mock_icd_data <- map_charlson_comorbidities(
#'   mock_icd_data, "icd10_code")
#' mock_icd_data
map_charlson_comorbidities <- function(df, icd_column){
  
  if (length(icd_column) != 1) {
    stop(simpleError(
      "`icd_column` must be of length 1"
    ))
  }
  
  if (is.integer(icd_column)) {
    icd_column <- names(df)[icd_column]
  }

  icd <- unique(df[, icd_column])
  loc <- sapply(charlson_regex, grep, icd, value = TRUE)
  loc <- utils::stack(loc)
  names(loc)[1] <- "icd_code"
  names(loc)[2] <- "comorb"
  loc <- loc %>% 
    dplyr::mutate(comorb_group = dplyr::case_when(
      comorb %in% c("diab", "diabwc") ~ "diab",
      comorb %in% c("mld", "msld") ~ "ld",
      comorb %in% c("canc", "metacanc") ~ "canc",
      TRUE ~ as.character(comorb)
    )) %>% 
    dplyr::mutate(charlson_weights = dplyr::case_when(
      comorb %in% c("ami", "chf", "pvd", "cevd", "dementia",
                    "copd", "rheumd", "pud", "mld", "diab") ~ 1L,
      comorb %in% c("diabwc", "hp", "rend", "canc") ~ 2L,
      comorb %in% c("mlsd") ~ 3L,
      comorb %in% c("metacanc", "aids") ~ 6L,
      TRUE ~ NA_integer_
    ))
  
  merge(df, loc, by.x = icd_column, by.y = "icd_code", 
        all.x = TRUE, sort = FALSE)
}



#' Map ICD-10 to common infections and antibiotic indications
#' 
#' @description Map ICD-10 or ICD-10-CM codes by infection types 
#' and whether antibiotics are commonly indicated based on definitions
#' set by \insertCite{Hashimoto2020}{Ramses}.
#' @param df a data frame containing ICD-10 codes
#' @param icd_column a variable name or column number in `df` 
#' containing ICD-10 codes.
#' @details For more detail on infection categories and indications
#' for antibiotic therapy, see \code{\link{antibiotic_icd_indications}}.
#' 
#' This function is designed to be compatible with both ICD-10 and 
#' ICD-10-CM versions.
#' @return The input data frame \code{x} with additional variables 
#' describing infection type and whether antibiotics are indicated. 
#' @export
#' @seealso \code{\link{antibiotic_icd_indications}}
#' @examples
#' mock_icd_data <- data.frame(
#'    list(icd10_code = c("J44", "J44X", "J440")), 
#'    stringsAsFactors = FALSE)
#' mock_icd_data <- map_infections_abx_indications(
#'   mock_icd_data, "icd10_code")
#' mock_icd_data
map_infections_abx_indications <- function(df, icd_column) {
  
  # TODO: assess quality of mapping on UK ICD
  
  if (length(icd_column) != 1) {
    stop(simpleError(
      "`icd_column` must be of length 1"
    ))
  }
  
  if (is.integer(icd_column)) {
    icd_column <- names(df)[icd_column]
  }
  
  input_variables <- names(df)
  join_output <- list()
  
  antibiotic_icd_indications <- Ramses::antibiotic_icd_indications
  width_ICD_codes <- stringr::str_length(antibiotic_icd_indications$icd_root)
  width_ICD_codes <- range(width_ICD_codes)
  
  for(i in width_ICD_codes){
    # Take ICD codes, remove placeholders X, and extract 3-character 
    # and 4-character versions
    df[, paste0("RAMSES_icd_length_", i)] <- 
      substr(gsub("X$", "", df[[icd_column]]), 0, i)
    join_output[[i]] <- merge(
      df[, c(icd_column, paste0("RAMSES_icd_length_", i))], 
      antibiotic_icd_indications, all = FALSE,
      by.x = paste0("RAMSES_icd_length_", i), by.y = "icd_root")
  }
  
  join_output <- dplyr::bind_rows(join_output)
  join_output <- join_output[, -grep("^RAMSES_icd_length_", names(join_output))]
  
  df <- merge(df, unique(join_output), all.x = T)
  
  return(df[, c(input_variables, "infection_group1_code", 
               "infection_group1_label", "infection_group2_label", 
               "infection_group2_code", "antibiotics_indicated")])
  
}



#' Map ICD-10 codes to the Clinical Classifications Software (CCS)
#' 
#' @description Map ICD-10 codes to clinically meaning full 
#' categories within the Clinical Classifications Software
#' and the Clinical Classifications Software Refined (CCSR), 
#' further described in [`ccs`](css) and 
#' \code{\link{ccsr}}.
#' @param df a data frame containing ICD-10 codes
#' @param icd_column a variable name or column number in `df` 
#' containing ICD-10 codes.
#' @details This function is designed to be compatible with 
#' both ICD-10 and ICD-10-CM versions.
#' @return The input data frame with additional CCS/CCSR classifications.
#' CCS categories are mutually exclusive. CCSR categories are not.
#' @export
#' @seealso \code{\link{ccs}}, \code{\link{ccsr}}.
#' @name map_ICD10_CCS
#' @examples
#' mock_icd_data <- data.frame(
#'   list(icd10_code = c("J44", "J44X", "J440")), 
#'   stringsAsFactors = FALSE)
#' mock_icd_data <- map_ICD10_CCS(mock_icd_data, "icd10_code")
#' mock_icd_data <- map_ICD10_CCSR(mock_icd_data, "icd10_code")
#' str(mock_icd_data)
NULL

#' @rdname map_ICD10_CCS
#' @export
map_ICD10_CCS <- function(df, icd_column) {
  .map_ICD10_CCSX(df, icd_column, ccsx = Ramses::ccs)
}
#' @rdname map_ICD10_CCS
#' @export
map_ICD10_CCSR <- function(df, icd_column) {
  .map_ICD10_CCSX(df, icd_column, ccsx = Ramses::ccsr)
}

.map_ICD10_CCSX <- function(df, icd_column, ccsx) {
  
  df$match_key <- gsub("X$", "", df[[icd_column]])
  
  # Prepare look ups for left joins
  ccsx_allchars <- dplyr::select(ccsx, 
                                 -.data$keep_3chars, -.data$keep_4chars,
                                 -.data$icd_code_3chars, -.data$icd_code_4chars)
  ccsx_3chars <- dplyr::filter(ccsx, .data$keep_3chars) %>% 
    dplyr::select(-.data$keep_3chars, -.data$keep_4chars, 
                  -.data$icd_code_4chars, -.data$icd_code)
  ccsx_4chars <- dplyr::filter(ccsx, .data$keep_4chars) %>% 
    dplyr::select(-.data$keep_3chars, -.data$keep_4chars, 
                  -.data$icd_code_3chars, -.data$icd_code)
  
  # Match on all
  x_matched <- dplyr::inner_join(df, 
                                 ccsx_allchars,
                                 by = c("match_key" = "icd_code"))
  x_unmatched <- dplyr::anti_join(df,
                                  ccsx_allchars,
                                  by = c("match_key" = "icd_code"))
  # Match on 4 characters
  x_matched <- dplyr::inner_join(x_unmatched,
                                 ccsx_4chars,
                                 by = c("match_key" = "icd_code_4chars")) %>% 
    dplyr::bind_rows(.data, x_matched)
  
  x_unmatched <- dplyr::anti_join(x_unmatched,
                                  ccsx_4chars,
                                  by = c("match_key" = "icd_code_4chars"))
  
  # Match on 3 characters
  x_matched <- dplyr::inner_join(x_unmatched,
                                 ccsx_3chars,
                                 by = c("match_key" = "icd_code_3chars")) %>% 
    dplyr::bind_rows(.data, x_matched)
  
  x_unmatched <- dplyr::anti_join(x_unmatched,
                                  ccsx_3chars,
                                  by = c("match_key" = "icd_code_3chars"))
  
  # Match on 3 characters on both datasets
  
  x_unmatched <- mutate(x_unmatched,
                        match_key = substr(.data$match_key, 0, 3))
  x_matched <- dplyr::inner_join(x_unmatched,
                                 ccsx_3chars,
                                 by = c("match_key" = "icd_code_3chars")) %>% 
    dplyr::bind_rows(.data, x_matched)
  
  x_unmatched <- dplyr::anti_join(x_unmatched,
                                  ccsx_3chars,
                                  by = c("match_key" = "icd_code_3chars"))
  
  # Get both matched and unmatched codes
  x_all <- dplyr::bind_rows(x_matched, x_unmatched) %>%
    dplyr::arrange(!!sym(icd_column)) %>%
    dplyr::select(-.data$match_key)
  
  x_all
}



#' Compute Defined Daily Doses (DDDs)
#'
#' @description Compute Defined Daily Doses (DDDs) for a drug prescription 
#' or administration record using the World Health Organisation Anatomical 
#' Therapeutic Chemical (ATC) classification \insertCite{WHO-ATC2020}{Ramses}.
#' @param ab  a character vector coercible to an antibiotic code using 
#' \code{\link[AMR]{as.ab}()} 
#' @param administration a character vector indicating the route of 
#' administration (see Details)
#' @param dose a numeric vector indicating the total dose for the basis 
#' of strength of the ATC DDD: in the case of prescriptions, this should
#' be the total dose given in one day
#' @param unit a character vector coercible to a `units` object with 
#' \code{\link[units]{as.units}()}. 
#'
#' @details This function queries the WHO ATC website for the most 
#' up-to-date reference values of the DDDs.
#'   
#' The `administration` parameter must be one of the following values:
#'  \itemize{
#'      \item \code{'Implant'} for implants
#'      \item \code{'Inhal'} for inhalation (eg: nebuliser or inhalers)
#'      \item \code{'Instill'} for instillation (eg: topical drops)
#'      \item \code{'N'} for nasal administration
#'      \item \code{'O'} for oral administration or administration via 
#'      percutaneous endoscopic gastrostomy tube 
#'      \item \code{'P'} for parenteral administration (eg intravenous 
#'      bolus/infusion injections, intramuscular injections)
#'      \item \code{'R'} for rectal administration
#'      \item \code{'SL'} for sublingual/buccal/oromucosal administration
#'      \item \code{'TD'} for transdermal administration (eg: patch)
#'      \item \code{'V'} for vaginal administration
#'  }
#' @return a numeric vector containing the DDDs
#' @export
#' @importFrom AMR as.ab atc_online_property
#' @importFrom units set_units
#' @importFrom stats na.omit
#' @references \insertAllCited{}
#' @examples
#' # Three tablets of amoxicillin 500mg
#' compute_DDDs("Amoxicillin", "O", 1.5, "g")
#' 
#' # Three tablets of co-amoxiclav 625mg (containing 500mg amoxicillin each)
#' compute_DDDs("Coamoxiclav", "O", 1.5, "g") 
compute_DDDs <- function(ab, administration, dose, unit) {
  
  x <- data.table(
    ab = ab, 
    administration = administration,
    dose = dose, 
    unit = unit
  )
  
  reference <- na.omit(
    unique(
    x[, list(ab, administration)]
    ))
  reference$valid_ab <- AMR::as.ab(reference$ab)
  reference <- reference[!is.na(valid_ab)]
  
  if (nrow(reference)==0) {
    stop(simpleError("`ab` and `administration` must contain valid values"))
  }
  
  reference[ , `:=`(
    ddd_value = mapply(
      FUN = AMR::atc_online_property,
      atc_code = valid_ab,
      administration = administration, 
      property = "DDD"),
    ddd_unit = mapply(
      FUN = AMR::atc_online_property,
      atc_code = valid_ab,
      administration = administration,
      property = "U")
    )]
  
  x <- merge(x, reference, all.x = T, sort = F)
  x$DDD <- NA_real_

  x[!(is.na(ddd_value) | is.na(dose) | is.na(unit)),
    DDD := as.numeric(
      units::mixed_units(x = dose, value = unit) /
        units::mixed_units(x = ddd_value, value = ddd_unit)
      )]
  
  return(x[["DDD"]])
}
