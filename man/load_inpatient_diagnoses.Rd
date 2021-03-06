% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{load_inpatient_diagnoses}
\alias{load_inpatient_diagnoses}
\title{Load inpatient diagnosis records into the warehouse}
\usage{
load_inpatient_diagnoses(
  conn,
  diagnoses_data,
  diagnoses_lookup,
  overwrite = FALSE
)
}
\arguments{
\item{conn}{a database connection}

\item{diagnoses_data}{a data frame validated with 
\code{\link{validate_inpatient_diagnoses}()}}

\item{diagnoses_lookup}{a data frame containing an ICD-10 reference 
lookup table}

\item{overwrite}{if \code{TRUE} (the default), will overwrite any existing
\code{inpatient_diagnoses} database table}
}
\description{
Validate, then load records of clinical diagnoses into the 
warehouse. This function automatically generates derived ICD-10 look up 
tables for comorbidities, infection indications, and the \code{\link{ccs}}.
}
\seealso{
\code{\link{validate_inpatient_episodes}()}, 
\code{\link{map_infections_abx_indications}()},
\code{\link{map_charlson_comorbidities}()},
\code{\link{map_ICD10_CCS}()},
\code{\link{map_ICD10_CCSR}()}
}
