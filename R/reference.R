#' English Adaptation of the WHO AWaRe Index
#'
#' @description This `AWaRe Index' is a classification by the World Health
#' Organisation of key antibiotics into three categories
#' \insertCite{Sharland2018}{Ramses} to:
#' \itemize{
#'        \item improve access (Access)
#'        \item monitor important antibiotics (Watch) and 
#'        \item preserve effectiveness of ‘last resort’ antibiotics (Reserve).
#' }
#' It was adapted by Public Health England for antibiotic stewardship
#' and quality improvement in English hospitals 
#' \insertCite{PHE-aware2019}{Ramses} 
#' 
#' @format A data frame with 208 antimicrobial agents, referenced 
#' against the Anatomical Therapeutic Chemical (ATC) 
#' classification \insertCite{WHO-ATC2020}{Ramses}, the World Health Organisation's
#' and Public Health England's respective AWaRe Indices, 
#' and the SNOMED-CT medical product concept codes and names 
#' (from Virtual Therapeutic Moieties codes of the UK NHS Dictionary of Medicines 
#' (dm+d) \insertCite{DMD2015}{Ramses}).
#' It contains 6 columns:
#' \describe{
#' \item{\code{ATC_code}}{ATC code of the antibiotic}
#' \item{\code{ATC_route}}{route of administration as defined in
#' the ATC ("O" = oral; "P" = parenteral; "R" = rectal;
#' "V" = vaginal)}
#' \item{\code{ATC_name}}{ATC name of the antibiotic}
#' \item{\code{aware_category}}{AWaRe index ("Access", "Watch", "Reserve")}
#' \item{\code{version}}{AWaRe Index version (eg. "WHO", "England")}
#' \item{\code{year}}{AWaRe Index version year}
#' \item{\code{WHONET_ab_code}}{WHONET antibiotic code (from \code{\link[AMR]{as.ab}()})}
#' \item{\code{WHONET_ab_name}}{WHONET antibiotic name (from \code{\link[AMR]{ab_name}()})}
#' \item{\code{VTM_code}}{SNOMED-CT medicinal product code/Virtual Therapeutic Moiety 
#' concept code in the SNOMED-CT UK Drug Extension (see \insertCite{DMD2015}{Ramses})}
#' \item{\code{VTM_name}}{SNOMED-CT medicinal product code/Virtual Therapeutic Moiety 
#' concept name in the SNOMED-CT UK Drug Extension (see \insertCite{DMD2015}{Ramses})}
#' }
#' @docType data
#' @name reference_aware
#' @source Adapted from \insertCite{WHO-aware2019,PHE-aware2019;textual}{Ramses} with Virtual  
#' Therapeutic Moiety equivalents from the NHS Dictionary of Medicines (dm+d)
#' \insertCite{DMD2015}{Ramses}.
#' @references{\insertAllCited{}}
"reference_aware"

#' Clinical Classifications Software (CCS): map to 4-character ICD code
#' 
#' @description A map of 283 clinical concepts to 4-character ICD code adapted from the 
#'   Clinical Classifications Software (CCS) \insertCite{HCUP-CCS}{Ramses}.
#' 
#' @details The Clinical Classifications Software (CCS) is a database developed as part 
#'   of the Healthcare Cost and Utilization Project (HCUP) which maps ICD-10-CM codes into 283 
#'   hierarchical and mutually-exclusive CCS categories. The level-1 categories correspond to the
#'   ICD-10's 17 chapters plus two supplementary classifications:
#'   
#'   \enumerate{
#'     \item Infectious and parasitic diseases
#'     \item Neoplasms
#'     \item Endocrine; nutritional; and metabolic diseases and immunity disorders
#'     \item Diseases of the blood and blood-forming organs
#'     \item Diseases of the nervous system and sense organs
#'     \item Diseases of the circulatory system
#'     \item Diseases of the respiratory system
#'     \item Diseases of the digestive system
#'     \item Diseases of the genitourinary system
#'     \item Complications of pregnancy; childbirth; and the puerperium
#'     \item Diseases of the skin and subcutaneous tissue
#'     \item Diseases of the musculoskeletal system and connective tissue
#'     \item Congenital anomalies
#'     \item Certain conditions originating in the perinatal period
#'     \item Injury and poisoning
#'     \item Symptoms; signs; and ill-defined conditions and factors influencing health status
#'     \item Residual codes; unclassified; all E codes [259. and 260.]
#'     \item Mental Illness
#'   }
#'   
#'   In Ramses, this dataset is useful to facilitate machine learning tasks that rely on ICD-10 
#'   clinical diagnosis codes and other applications that require mutually-exclusive categories 
#'   (eg tasks involving data aggregation).
#'   
#' @format A data frame with 72,446 ICD-10-CM codes mapped to CCS multilevel categories.
#' @docType data
#' @name ccs
#' @seealso For the mapping function, see \code{\link{map_ICD10_CCS}()}. For the Clinical 
#' Classifications Software Refined, see \code{\link{ccsr}}.
#' @source Adapted from \insertCite{HCUP-CCS}{Ramses} for 4-character ICD-10 codes.
#' @references
#'   \insertAllCited{}
"ccs"


#' Clinical Classifications Software \emph{Refined} (CCSR): map to 4-character ICD code
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

#' Prescription frequency abbreviation look-up table
#' @description This look-up table provides the number of times a drug should 
#' be administered per day based on the prescription frequency abbreviation 
#' (including latin abbreviations).
#' 
#' Example: BD stands for \emph{bis die}, meaning twice a day. The 
#' corresponding `daily_frequency`` value is 2.
#' 
#' @format A data frame with 64 abbreviations and their corresponding daily frequency value.
#' @docType data
#' @name reference_drug_frequency
"reference_drug_frequency"

#' Clinical investigations and observations LOINC codes look-up table
#' @description This look-up table provides a subset of useful clinical investigation
#' codes from the LOINC terminology. It can be used to map clinical investigations
#' and observations that are relevant to modelling antibiotic prescribing decisions.
#' @details This material contains content from LOINC (\url{http://loinc.org}). 
#' LOINC is copyright © 1995-2020, Regenstrief Institute, Inc. and the 
#' Logical Observation Identifiers Names and Codes(LOINC) Committee and 
#' is available at no cost under the license at \url{http://loinc.org/license}.
#' LOINC® is a registered United States trademark of Regenstrief Institute, Inc.
#' @source LOINC Table File Version 2.68. Released 2020-06-17. \url{http://loinc.org}
#' @format A data frame.
#' @docType data
#' @name reference_loinc
"reference_loinc"

#' Laboratory specimen type reference table (SNOMED CT concepts )
#' @description This reference table contains the list of admissible specimen
#' types in microbiology records (see \code{\link[Ramses]{validate_microbiology}()}).
#' These concepts are from the 
#' \href{SNOMED CT International Edition}{https://browser.ihtsdotools.org/} and 
#' correspond to teh descendants of the \code{123038009 | Specimen (specimen) |} 
#' concept.
#' @details This data frame contains 5 variables:
#' \describe{
#'     \item{\code{conceptId}}{SNOMED CT concept code}
#'     \item{\code{moduleId}}{SNOMED CT module code}
#'     \item{\code{fsn_term}}{SNOMED CT Full Specified Name}
#'     \item{\code{pt_term}}{SNOMED CT Preferred Term}
#'     \item{\code{snomed_release_version}}{SNOMED CT RF2 release date}
#' }
#' @seealso \href{https://snomedizer.web.app}
#' @format A data frame.
#' @docType data
#' @name reference_specimen_type
"reference_specimen_type"
