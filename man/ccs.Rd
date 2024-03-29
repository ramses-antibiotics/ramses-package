% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reference.R
\docType{data}
\name{ccs}
\alias{ccs}
\title{Clinical Classifications Software (CCS): map to 4-character ICD codes}
\format{
A data frame with 72,446 ICD-10-CM codes mapped to CCS multilevel categories.
}
\source{
Adapted from \insertCite{HCUP-CCS}{Ramses} for 4-character ICD-10 codes.
}
\usage{
ccs
}
\description{
A map of 283 clinical concepts to 4-character ICD codes adapted from the 
  Clinical Classifications Software (CCS) \insertCite{HCUP-CCS}{Ramses}.
}
\details{
The Clinical Classifications Software (CCS) is a database developed as part 
  of the Healthcare Cost and Utilization Project (HCUP) which maps ICD-10-CM codes into 283 
  hierarchical and mutually-exclusive CCS categories. Level-1 categories correspond to the
  ICD-10's 17 chapters plus two supplementary classifications:
  
  \enumerate{
    \item Infectious and parasitic diseases
    \item Neoplasms
    \item Endocrine; nutritional; and metabolic diseases and immunity disorders
    \item Diseases of the blood and blood-forming organs
    \item Diseases of the nervous system and sense organs
    \item Diseases of the circulatory system
    \item Diseases of the respiratory system
    \item Diseases of the digestive system
    \item Diseases of the genitourinary system
    \item Complications of pregnancy; childbirth; and the puerperium
    \item Diseases of the skin and subcutaneous tissue
    \item Diseases of the musculoskeletal system and connective tissue
    \item Congenital anomalies
    \item Certain conditions originating in the perinatal period
    \item Injury and poisoning
    \item Symptoms; signs; and ill-defined conditions and factors influencing health status
    \item Residual codes; unclassified; all E codes [259. and 260.]
    \item Mental Illness
  }
  
  This dataset can help group ICD-10 into a smaller number of mutually-exclusive 
  categories, such as aggregating data.
}
\examples{
data("ccs")
head(ccs)
}
\references{
\insertAllCited{}
}
\seealso{
For the mapping function, see \code{\link{map_ICD10_CCS}()}. For the Clinical 
Classifications Software Refined, see \code{\link{ccsr}}.
}
\keyword{datasets}
