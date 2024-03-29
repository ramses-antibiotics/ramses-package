% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_data.R
\name{validate_investigations}
\alias{validate_investigations}
\title{Validate records of observations & investigations}
\usage{
validate_investigations(
  investigations,
  custom_units = c("breaths", "beats", "U")
)
}
\arguments{
\item{investigations}{a data frame}

\item{custom_units}{a character vector of valid unit codes not listed in
the UCUM. Default is: \code{c("breaths", "beats", "U")}.}
}
\value{
\code{TRUE} if the validation is passed
}
\description{
Validate records of observations & investigations
}
\section{Mandatory variables}{

The following variables are required:
\describe{
   \item{\code{"observation_id"}}{a unique identifier with no missing value}
   \item{\code{"patient_id"}}{a patient identifier with no missing value}
   \item{\code{"status"}}{Codes from the following value set 
   \url{http://hl7.org/fhir/observation-status/} \itemize{
  \item \code{"registered"}: The existence of the observation is registered, but there is no result yet available.
  \item \code{"preliminary"}: his is an initial or interim observation: data may be incomplete or unverified.
  \item \code{"final"}: The observation is complete and there are no further actions needed. 
  \item \code{"amended"}: Subsequent to being Final, the observation has been modified 
  subsequent. This includes updates/new information and corrections.
  \item \code{"corrected"}: Subsequent to being Final, the observation has been modified to 
  correct an error in the test result.
  \item \code{"cancelled"}: The observation is unavailable because the 
  measurement was not started or not completed.
  \item \code{"entered-in-error"}: The observation has been withdrawn following previous 
  final release. This electronic record should never have existed, though it is possible 
  that real-world decisions were based on it. (If real-world activity has occurred, 
  the status should be "cancelled" rather than "entered-in-error".).
  \item \code{"unknown"}: The authoring/source system does not know which of the status 
  values currently applies for this observation. Note: This concept is not to be used for 
  "other" - one of the listed statuses is presumed to apply, but the authoring/source 
  system does not know which.
  }}
   \item{\code{"request_datetime"}}{a datetime when the observation was 
   requested with no missing value}
   \item{\code{"observation_datetime"}}{a datetime when the investigation 
   was performed with no missing value}
   \item{\code{"observation_code_system"}}{URL of the code system (for instance: 
   \code{"http://snomed.info/sct"}, \code{"http://loinc.org"})}
   \item{\code{"observation_code"}}{LOINC concept code or SNOMED-CT concept 
   code corresponding to a SNOMED CT observable entity or evaluation procedure}
   \item{\code{"observation_name"}}{code system name for the observation}
   \item{\code{"observation_display"}}{observation name to display}
   \item{\code{"observation_value_text"}}{observation string value or codable
   concept, for example: \code{"TRUE"}/\code{"FALSE"}, 
   \code{"Yes"}/\code{"No"}, SNOMED CT qualifier value}
   \item{\code{"observation_value_numeric"}}{observation numeric value}
   \item{\code{"observation_unit"}}{a unit code passing 
   \code{\link[units]{as_units}()}. See examples. All observations with the
   same \code{"observation_code"} must be converted to the same 
   \code{"observation_unit"}. See also: \code{\link[units]{valid_udunits}}, 
   \code{\link[units]{install_unit}}
   \url{https://ucum.org/}}
}
}

\examples{
# the units "breaths/min" (http://loinc.org/8867-4) or
# "beats/min" () do not exist in the https://ucum.org/. 
library(units)
\dontrun{as_units("breaths/min")} # fails

# Yet, they may be declared.
install_unit("breaths") 
as_units("breaths/min") # succeeds
}
