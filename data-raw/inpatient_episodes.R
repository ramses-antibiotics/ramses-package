## code to prepare `data/patients.rda` dataset goes here
## code to prepare `data/inpatient_episodes.rda` dataset goes here
## code to prepare `data/inpatient_diagnoses.rda` dataset goes here
## code to prepare `data/inpatient_wards.rda` dataset goes here
## code to prepare `data/inpatient_microbiology.rda` dataset goes here
## code to prepare `data/inpatient_investigations.rda` dataset goes here
library(dplyr)
library(lubridate)

inpatient_episodes <- read.csv(
  file = "data-raw/inpatient_episodes.csv", stringsAsFactors = F,
  colClasses = c("character", "character", "Date", "Date", 
                 "character", "character", "character", 
                 "character", "character", "character", 
                 "POSIXct", "POSIXct", "integer", 
                 "integer", "character", "POSIXct", "POSIXct", 
                 "character", "character"))
inpatient_episodes2 <- read.csv(
  file = "data-raw/inpatient_episodes2.csv", stringsAsFactors = F,
  colClasses = c("character", "character", "Date", "Date", 
                 "character", "character", "character", 
                 "character", "character", "character", 
                 "POSIXct", "POSIXct", "integer", 
                 "integer", "character", "POSIXct", "POSIXct", 
                 "character", "character"))
inpatient_episodes <- dplyr::bind_rows(inpatient_episodes,
                                       inpatient_episodes2)

patients <- dplyr::transmute(
  inpatient_episodes,
  patient_id,
  NHS_number,
  forename,
  surname,
  sex,
  ethnic_category_UK = ethnic_group,
  date_of_birth,
  date_of_death
) %>% 
  dplyr::distinct()

usethis::use_data(patients, overwrite = T)

inpatient_episodes <- dplyr::select(
  inpatient_episodes,
  -NHS_number,
  -forename,
  -surname,
  -sex,
  -ethnic_group,
  -age_on_admission
)

for (i in which(vapply(inpatient_episodes, is, class2 = "POSIXct", FUN.VALUE = logical(1)))) {
  attr(inpatient_episodes[[i]], "tzone") <- "Europe/London"
}

usethis::use_data(inpatient_episodes, overwrite = T)

inpatient_diagnoses <- read.csv(
  file = "data-raw/inpatient_diagnoses.csv", stringsAsFactors = F, 
  colClasses = c("character", "character",
                 "integer", "character",
                 "integer", "POSIXct", 
                 "POSIXct", "character"))
inpatient_diagnoses2 <- read.csv(
  file = "data-raw/inpatient_diagnoses2.csv", stringsAsFactors = F, 
  colClasses = c("character", "character",
                 "integer", "character",
                 "integer", "POSIXct", 
                 "POSIXct", "character"))
inpatient_diagnoses <- dplyr::bind_rows(inpatient_diagnoses,
                                        inpatient_diagnoses2) %>% 
  dplyr::select(-.data$episode_start, -.data$episode_end)

for (i in which(vapply(inpatient_diagnoses, is, class2 = "POSIXct", FUN.VALUE = logical(1)))) {
  attr(inpatient_diagnoses[[i]], "tzone") <- "Europe/London"
}

usethis::use_data(inpatient_diagnoses, overwrite = T)

inpatient_wards <- read.csv(
  file = "data-raw/inpatient_wards.csv", stringsAsFactors = F, 
  colClasses = c("character", "character", "character","character", "character",
                 "POSIXct", "POSIXct"))
inpatient_wards2 <- read.csv(
  file = "data-raw/inpatient_wards2.csv", stringsAsFactors = F, 
  colClasses = c("character", "character", "character","character", "character",
                 "POSIXct", "POSIXct"))
inpatient_wards <- dplyr::bind_rows(inpatient_wards,
                                    inpatient_wards2)

for (i in which(vapply(inpatient_wards, is, class2 = "POSIXct", FUN.VALUE = logical(1)))) {
  attr(inpatient_wards[[i]], "tzone") <- "Europe/London"
}

usethis::use_data(inpatient_wards, overwrite = T)

inpatient_investigations <- read.csv(
  "data-raw/inpatient_investigations.csv", stringsAsFactors = F,
  colClasses = c("character", "character",  "character", "character", 
                 "POSIXct", "POSIXct", "character", "numeric", "character",
                 "character", "character", "character", "character"))

for (i in which(vapply(inpatient_investigations, is, class2 = "POSIXct", FUN.VALUE = logical(1)))) {
  attr(inpatient_investigations[[i]], "tzone") <- "Europe/London"
}

usethis::use_data(inpatient_investigations, overwrite = T)



inpatient_microbiology <- read.csv(
  "data-raw/inpatient_microbiology.csv", 
  stringsAsFactors = F,
  colClasses = c("character", "character", "character", "character",
                 "POSIXct", "character", "character", "character",
                 "character", "character", "character")) %>% 
  select(-organism_code, -agent_code) %>% 
  mutate(status = NA_character_,
         specimen_datetime = lubridate::as_datetime(specimen_datetime),
         isolation_datetime = lubridate::as_datetime(specimen_datetime) %m+% days(3))

for (i in which(vapply(inpatient_microbiology, is, class2 = "POSIXct", FUN.VALUE = logical(1)))) {
  attr(inpatient_microbiology[[i]], "tzone") <- "Europe/London"
}

usethis::use_data(inpatient_microbiology, overwrite = TRUE)

