## code to prepare `data-raw/drug_*.csv` datasets goes here
drug_prescriptions <- read.csv(
  file = "data-raw/drug_prescriptions.csv", stringsAsFactors = F,
  colClasses = c("character", "character", "character", "POSIXct", 
                 "POSIXct", "POSIXct", "character", 
                 "character", "character", "character", 
                 "character", "numeric"))
drug_prescriptions$dose <- as.numeric(drug_prescriptions$dose)
usethis::use_data(drug_prescriptions, overwrite = T)

drug_administrations <- read.csv(
  file = "data-raw/drug_administrations.csv", stringsAsFactors = F,
  colClasses = c("character", "character", "character",
                 "character", "character", "character",
                 "POSIXct"))
drug_administrations$dose <- as.numeric(drug_administrations$dose)

usethis::use_data(drug_administrations, overwrite = T)

drug_prescriptions_variables <- read.csv(
  file = "data-raw/drug_prescriptions_variables.csv", stringsAsFactors = F,
  colClasses = c("integer", "character", "logical", "logical")
)

usethis::use_data(drug_prescriptions_variables, overwrite = T)
