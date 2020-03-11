#' Clinical Classifications Software *Refined* (CCSR): map to 4-character ICD code
#' 
#' @description A map of 538 clinical concepts to 4-character ICD code 
#' adapted from the Clinical Classifications Software Refined (CCSR) 
#' \insertCite{HCUP-CCSR}{Ramses}.
#' 
#' @details The Clinical Classifications Software Refined (CCSR) is a 
#' database developed as part of the Healthcare Cost and Utilization 
#' Project (HCUP) which maps ICD-10-CM codes into 538 CCSR
#' categories. Unlike the pre-existing \code{\link{ccs}}:
#' \itemize{
#'     \item CCSR categories are not mutually exclusive.
#'     \item CCSR thus contains new categories that did not 
#'     exist in \code{\link{ccs}}
#'     \item CCSR categories are organized into 21 body systems
#' }
#' 
#' @format A data frame with 72,715 ICD-10-CM codes mapped to 
#' CCSR categories.
#' @docType data
#' @name ccsr
#' @seealso For the mapping function, see \code{\link{map_ICD10_CCSR}()}. For the Clinical 
#' Classifications Software, see \code{\link{ccs}}.
#' @source Adapted from \insertCite{HCUP-CCSR}{Ramses} for 
#' 4-character ICD-10 codes.
#' @references
#'   \insertAllCited{}
"ccsr"