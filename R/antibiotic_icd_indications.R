#' Common infections and indications for antimicrobial therapy
#' 
#' @description Look up table of common infections and whether 
#' they are commonly treated with antibiotics.
#' @format \code{antibiotic_icd_indications} is data frame of 276 rows 
#' defining 74 infections classified into 17 groups, with references 
#' to ICD-10 codes.
#' 
#' It contains 6 columns:
#' \describe{
#' \item{\code{icd_root}}{character vector of 3- to 4-character ICD-10 codes}
#' \item{\code{infection_group1_label}}{character vector referencing 17 infection groups}
#' \item{\code{infection_group2_label}}{character vector referencing 74 infections}
#' \item{\code{antibiotics_indicated}}{character vector indicating whether antibiotics
#' are indicated ("potentially", "rarely", "usually")}
#' \item{\code{infection_group1_code}}{character vector containing 3-character short codes
#' for infection groups}
#' \item{\code{infection_group2_code}}{character vector containing 5-character short codes
#' for infections}
#' }
#' @docType data
#' @name antibiotic_icd_indications
#' @source Adapted from Supplementary Table S1 in \insertCite{Hashimoto2020}{Ramses}.
#' @references
#'   \insertAllCited{}
"antibiotic_icd_indications"