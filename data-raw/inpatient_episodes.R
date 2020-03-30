## code to prepare `data-raw/inpatient_episodes.csv` dataset goes here
## code to prepare `data-raw/inpatient_diagnoses.csv` dataset goes here
inpatient_episodes <- read.csv(
  file = "data-raw/inpatient_episodes.csv", stringsAsFactors = F,
  colClasses = c("character", "character", "Date", "Date", 
                 "character", "character", "character", 
                 "character", "character", "character", 
                 "POSIXct", "POSIXct", "integer", 
                 "integer", "character", "POSIXct", "POSIXct"))
usethis::use_data(inpatient_episodes, overwrite = T)

inpatient_diagnoses <- read.csv(
  file = "data-raw/inpatient_diagnoses.csv", stringsAsFactors = F, 
  colClasses = c("character", "character", "POSIXct", 
                 "POSIXct", "integer", "character",
                 "character", "integer"))
usethis::use_data(inpatient_diagnoses, overwrite = T)

inpatient_wards <- read.csv(
  file = "data-raw/inpatient_wards.csv", stringsAsFactors = F, 
  colClasses = c("character", "character", "character",
                 "POSIXct", "POSIXct"))
usethis::use_data(inpatient_wards, overwrite = T)
