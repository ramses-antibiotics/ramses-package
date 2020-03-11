## code to prepare `data-raw/drug_prescriptions.csv` dataset goes here
## code to prepare `data-raw/drug_administrations.csv` dataset goes here
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
