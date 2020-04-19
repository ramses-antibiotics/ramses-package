
#' Download the ICD-10-CM reference file by the US National Center for 
#' Health Statistics
#'
#' @return A data frame containing ICD-10-CM codes 
#' (\code{icd_code}) and labels (\code{icd_description})
#' @export
download_icd10cm <- function() {
  icd10cm_url <- "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2020-ICD-10-CM-Codes.zip"
  icd10cm_file <- tempfile()
  utils::download.file(url = icd10cm_url, destfile = icd10cm_file)
  icd10cm_file <- unz(
    icd10cm_file, 
    grep("icd10cm_order_[0-9]{4}.txt$", 
         utils::unzip(icd10cm_file, list = T)$Name, 
         value = T))
  icd_source <- utils::read.fwf(file = icd10cm_file, 
                             widths = c(6,8,2,61), header = F,
                             col.names = c("scrap", "icd_code", 
                                           "header_indicator", "icd_description"),
                             stringsAsFactor = FALSE)[,-1]
  icd_source$icd_code <- trimws(icd_source$icd_code)
  icd_source$icd_description <- trimws(icd_source$icd_description)
  icd_source$level <- stringr::str_length(icd_source$icd_code)
  
  icd3 <- filter(icd_source, .data$level == 3) %>% 
    transmute(category_code = .data$icd_code,
              category_description = .data$icd_description)
  
  icd5 <- filter(icd_source, .data$header_indicator == 1) %>% 
    mutate(category_code = substring(.data$icd_code, 0, 3))
  
  icd <- merge(icd5, icd3, by = "category_code", all.x = T)
  
  icd$category_description[is.na(icd$category_description)] <- 
    icd$description[is.na(icd$category_description)]
  
  icd$icd_display <- paste0(substring(icd$icd_code, first = 0, last = 3),
                            ".", substring(icd$icd_code, first = 4))
  
  icd$edition <- "ICD-10-CM 2020"
  
  icd <- dplyr::select(icd, -header_indicator, -level)

  icd <- arrange_variables(icd, 
                           first_column_names = c(
                             "icd_code",
                             "icd_display",
                             "icd_description",
                             "category_code",
                             "category_description",
                             "edition"
                           ))
  icd
}


#' International Classification of Diseases
#' 
#' @description The World Health Organisation (WHO) International 
#' Statistical Classification of Diseases and Related Health 
#' Problems Tenth Revision is available under restrictive licence terms.
#' 
#' The UK implementation of ICD-10 is available from the NHS TRUD website
#' \url{https://isd.digital.nhs.uk/trud3/user/guest/group/0/pack/28} 
#' at no cost to UK users subject to a licence agreement.
#'  
#' The US implementation of ICD-10-CM is available from 
#' \url{https://www.cdc.gov/nchs/icd/icd10cm.htm}
#'
#' @param archive path to the ZIP archive of the ICD-10 release 
#' (5th Edition is recommended)
#' @param version a character string labelling the edition 
#' (e.g. "GB 5th Edition")
#'
#' @return A data frame ready for importing into the database with the 
#' following variables:
#' \describe{
#'     \item{icd_code}{character vector of ICD-10 codes without punctuation. 
#'     May include \code{"X"} placeholder characters depending on the edition.}
#'     \item{icd_display}{character vector of ICD-10 codes with a dot after the first character,
#'     for display in user interfaces.}
#'     \item{icd_description}{charactor vector of ICD-10 code descriptions}
#'     \item{category_code}{character vector of three-character ICD-10 codes}
#'     \item{category_description}{charactor vector three-character ICD-10 code descriptions}
#'     \item{edition}{edition label provided in parameter \code{version}}
#'     \item{\code{...}}{any other variables present in the \code{archive} source}
#' }
#' @import dplyr magrittr  
#' @export
#'
#' @examples
#' \dontrun{
#' import_icd("icd_df_10.5.0_20151102000001.zip", "GB 5th Edition")
#' }
import_icd <- function(archive, version) {
  
  temp <- tempfile()
  source_files <- utils::unzip(archive, exdir = temp)
  
  icd_source <- readr::read_delim(
    grep("CodesAndTitlesAndMetadata_GB", source_files, value = T),
    delim = "\t", guess_max = 10000)
  rm(temp)
  
  colnames(icd_source) <- tolower(colnames(icd_source))
  
  icd_source$level <- stringr::str_length(icd_source[["alt_code"]])
  
  icd3 <- filter(icd_source, .data$level == 3) %>% 
    transmute(category_code = .data$alt_code,
              category_description = .data$description)
  
  icd5 <- filter(icd_source, .data$level > 3) %>% 
    mutate(category_code = substring(.data$alt_code, 0, 3))
  
  icd <- merge(icd5, icd3, by = "category_code", all.x = T)
  
  icd$category_description[is.na(icd$category_description)] <- 
    icd$description[is.na(icd$category_description)]
  
  icd$edition <- version
  
  icd <- dplyr::rename(icd,
                       icd_display = code,
                       icd_code = alt_code,
                       icd_description = description)
  icd <- arrange_variables(icd, 
                           first_column_names = c(
                             "icd_code",
                             "icd_display",
                             "icd_description",
                             "category_code",
                             "category_description",
                             "edition"
                           ))
  icd
  
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
#' This function includes modified source code by \code{
#' \link[AMR]{atc_online_property}()}.
#' @param ATC_code a character vector of ATC codes (can be easily obtained from)
#' \code{\link[AMR]{ab_atc}() in the `AMR` package} 
#' @param ATC_administration a character vector indicating the ATC route of 
#' administration (see Details)
#' @param dose a numeric vector indicating the total dose for the basis 
#' of strength of the ATC DDD: in the case of prescriptions, this should
#' be the total dose given in one day
#' @param unit a character vector coercible to a `units` object with 
#' \code{\link[units]{as.units}()}. 
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
#' @importFrom units set_units
#' @importFrom stats na.omit
#' @importFrom AMR ab_atc
#' @references With thanks to the AMR package authors.
#' \insertAllCited{}
#' @examples
#' # Three tablets of amoxicillin 500mg
#' compute_DDDs("J01CA04", "O", 3 * 500, "mg")
#' 
#' # Three tablets of co-amoxiclav 625mg (containing 500mg amoxicillin each)
#' compute_DDDs("J01CR02", "O", 3 * 500, "mg") 
#' 
#' # This function may also be used with `dplyr`
#' library(dplyr)
#' library(magrittr)
#' library(AMR)
#' 
#' Ramses::drug_prescriptions %>% 
#'   head() %>% 
#'   transmute(rxsummary, 
#'             tr_DESC,
#'             route,
#'             dose, 
#'             units,
#'             daily_frequency = case_when(
#'               frequency == "BD" ~ 2,
#'               frequency == "TDS" ~ 3,
#'               frequency == "6H" ~ 4
#'             )) %>% 
#'   mutate(DDD = compute_DDDs(
#'     ATC_code = AMR::ab_atc(tr_DESC),
#'     ATC_administration = if_else(route == "ORAL", "O", "P"),
#'     dose = dose * daily_frequency,
#'     unit = units))
compute_DDDs <- function(ATC_code, ATC_administration, dose, unit) {
  
  if (any(!requireNamespace("curl", quietly = TRUE),
          !requireNamespace("xml2", quietly = TRUE),
          !requireNamespace("rvest", quietly = TRUE))) {
    stop("Packages \"curl\", \"rvest\" and \"xml2\" are required for this function to work. Please install them.",
         call. = FALSE)
  }
  
  if (!curl::has_internet()) {
    stop("An internet connection is required for this function to work.")
  }
  
  check_units <- na.omit(unique(as.character(unit)))
  invalid_units <- sapply(check_units, function(X) {
    inherits(
      try(units::as_units(X), silent = TRUE),
      "try-error")
  }, simplify = T)
  
  if(any(invalid_units)) {
    stop(simpleError(paste(
      "Invalid `unit` found:",
      paste(paste0("`", check_units, "`"), collapse = ", ")
    )))
  }
  
  stopifnot(na.omit(ATC_administration) %in% 
              c('Inhal', 'Instill', 'N', 'O', 
                'P', 'R', 'SL', 'TD', 'V'))
  
  search_ATC <- na.omit(unique(ATC_code))
  
  if(length(search_ATC) == 0) {
    stop("`ATC_code` must contain valid entries.")
  }
  
  reference_DDD <- list()
  who_url <- "https://www.whocc.no/atc_ddd_index/?code=%s&showdescription=no"
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Incorporating an adaptation of lines 126-180 
  # of atc_online.R of the AMR package
  # https://gitlab.com/msberends/AMR/-/blob/219cff403fa7515c07faab129f55f39ae648feb9/R/atc_online.R#L126
  # Original code verbatim below:
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # progress <- progress_estimated(n = length(atc_code))
  # 
  # for (i in seq_len(length(atc_code))) {
  #   
  #   progress$tick()$print()
  #   
  #   atc_url <- sub("%s", atc_code[i], url, fixed = TRUE)
  #   
  #   if (property == "groups") {
  #     tbl <- xml2::read_html(atc_url) %>%
  #       rvest::html_node("#content") %>%
  #       rvest::html_children() %>%
  #       rvest::html_node("a")
  #     
  #     # get URLS of items
  #     hrefs <- tbl %>% rvest::html_attr("href")
  #     # get text of items
  #     texts <- tbl %>% rvest::html_text()
  #     # select only text items where URL like "code="
  #     texts <- texts[grepl("?code=", tolower(hrefs), fixed = TRUE)]
  #     # last one is antibiotics, skip it
  #     texts <- texts[seq_len(length(texts)) - 1]
  #     returnvalue <- c(list(texts), returnvalue)
  #     
  #   } else {
  #     tbl <- xml2::read_html(atc_url) %>%
  #       rvest::html_nodes("table") %>%
  #       rvest::html_table(header = TRUE) %>%
  #       as.data.frame(stringsAsFactors = FALSE)
  #     
  #     # case insensitive column names
  #     colnames(tbl) <- tolower(colnames(tbl)) %>% gsub("^atc.*", "atc", .)
  #     
  #     if (length(tbl) == 0) {
  #       warning("ATC not found: ", atc_code[i], ". Please check ", atc_url, ".", call. = FALSE)
  #       returnvalue[i] <- NA
  #       next
  #     }
  #     
  #     if (property %in% c("atc", "name")) {
  #       # ATC and name are only in first row
  #       returnvalue[i] <- tbl[1, property]
  #     } else {
  #       if (!"adm.r" %in% colnames(tbl) | is.na(tbl[1, "adm.r"])) {
  #         returnvalue[i] <- NA
  #         next
  #       } else {
  #         for (j in seq_len(nrow(tbl))) {
  #           if (tbl[j, "adm.r"] == administration) {
  #             returnvalue[i] <- tbl[j, property]
  #           }
  #         }
  #       }
  #     }
  #   }
  # }

  progress <- progress_estimated(n = length(search_ATC))
  for (i in seq_len(length(search_ATC))) {
    progress$tick()$print()
    atc_url <- sub("%s", search_ATC[i], who_url, fixed = TRUE)
    atc_page <- xml2::read_html(atc_url) 
    if( length(rvest::html_nodes(
          atc_page, 
          paste0("table:contains(", search_ATC[i], ")")
          )) == 0 ) {
      warning("ATC not found: ", search_ATC[i], 
              ". Please check <<https://www.whocc.no/atc_ddd_index/>>.", call. = FALSE)
      next
    } else {
      atc_page <- atc_page %>% rvest::html_nodes("table") %>% 
        rvest::html_table(header = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
      colnames(atc_page) <- gsub("^atc[.]", "ATC_", tolower(colnames(atc_page)))
      colnames(atc_page) <- gsub("^ddd$", "ddd_value", colnames(atc_page))
      
      atc_page$ATC_code <- search_ATC[i]
      reference_DDD[[i]] <- dplyr::select(atc_page, -.data$name, -.data$note)
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  reference_DDD <- data.table::rbindlist(reference_DDD)
  x <- data.table(
    ATC_code = ATC_code, 
    adm.r = ATC_administration, 
    dose = dose,
    unit = unit
  )
  
  x <- merge(x, reference_DDD, all.x = T, sort = F)
  x$DDD <- NA_real_
  u = NULL
  x[!(is.na(ddd_value) | is.na(dose) | is.na(unit)),
    DDD := as.numeric(
      units::mixed_units(x = dose, value = unit) /
        units::mixed_units(x = ddd_value, value = u)
    )]
  
  return(x[["DDD"]])
}



#' Look up ATC name on the WHO website
#' 
#' @description Look up drug name from an ATC code using the WHO DDD website.
#' Requires an internet connection.
#' @param x a character vector of ATC codes
#'
#' @return a character vector of ATC names (lowercase).
#' @export
#' @references Adapted from \code{\link[AMR]{atc_name}()} with thanks to 
#' the \link{AMR} package authors.
#' @examples
#' get_ATC_name("J01XA01")
get_ATC_name <- function(x) {
  if (any(!requireNamespace("curl", quietly = TRUE),
          !requireNamespace("xml2", quietly = TRUE),
          !requireNamespace("rvest", quietly = TRUE))) {
    stop("Packages \"curl\", \"rvest\" and \"xml2\" are required for this function to work. Please install them.",
         call. = FALSE)
  }
  
  if (!curl::has_internet()) {
    stop("An internet connection is required for this function to work.")
  }
  
  search_ATC <- gsub(" ", "", unique(x))
  search_ATC[search_ATC == ""] <- NA
  search_ATC <- stats::na.omit(search_ATC)

  if(length(search_ATC) == 0) {
    stop("`x` must contain valid entries.")
  }
  
  output <- list()
  who_url <- "https://www.whocc.no/atc_ddd_index/?code=%s&showdescription=no"
  
  progress <- progress_estimated(n = length(search_ATC))
  for (i in seq_len(length(search_ATC))) {
    progress$tick()$print()
    atc_url <- sub("%s", search_ATC[i], who_url, fixed = TRUE)
    atc_page <- xml2::read_html(atc_url) 
    if( length(rvest::html_nodes(
      atc_page, 
      paste0("table:contains(", search_ATC[i], ")")
    )) == 0 ) {
      warning("ATC not found: `", search_ATC[i], 
              "`. Please check <https://www.whocc.no/atc_ddd_index/>.", call. = FALSE)
      next
    } else {
      atc_page <- atc_page %>% rvest::html_nodes("table") %>% 
        rvest::html_table(header = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
      output[[i]] <- data.table(
        ATC_code = search_ATC[i],
        ATC_name = atc_page$Name[1]
      )
    }
  }
  output <- data.table::rbindlist(output)
  if (nrow(output)==0) {
    return(rep(NA_character_, length(x)))
  } else {
    return(
      merge(x = data.table("ATC_code" = x), y = output, by = "ATC_code",
            all.x = TRUE, sort = FALSE)[["ATC_name"]]
    )
  }
}
