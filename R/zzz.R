
.drug_prescriptions_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "drug_prescriptions.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.drug_administrations_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "drug_administrations.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.inpatient_patients_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "patients.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.inpatient_episodes_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "inpatient_episodes.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.inpatient_diagnoses_data_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "inpatient_diagnoses_data.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.inpatient_diagnoses_lookup_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "inpatient_diagnoses_lookup.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.inpatient_wards_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "inpatient_wards.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.inpatient_investigations_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "inpatient_investigations.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.inpatient_microbiology_variables <- function() {
  
  specimens <- utils::read.csv(
    system.file("Schema", 
                "microbiology_specimens.csv", 
                package = "Ramses"), stringsAsFactors = F)
  isolates <- utils::read.csv(
    system.file("Schema", 
                "microbiology_isolates.csv", 
                package = "Ramses"), stringsAsFactors = F)
  susceptibilities <- utils::read.csv(
    system.file("Schema", 
                "microbiology_susceptibilities.csv", 
                package = "Ramses"), stringsAsFactors = F)
  
  return(list(
    specimens = specimens,
    isolates = isolates,
    susceptibilities = susceptibilities
  ))
}
