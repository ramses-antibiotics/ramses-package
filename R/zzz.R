
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

global_vars <- c(
  "admission_date",
  "discharge_date",
  "spell_id",
  "ddd_value",
  "ddd_unit",
  "valid_ab",
  "icd_code",
  "code",
  "alt_code",
  "description",
  "type",
  "from_id", 
  "to_id",
  "id1",
  "id2",
  "ab",
  "strength",
  "basis_of_strength",
  "authored_on",
  "duration",
  "reference_drug_frequency",
  "daily_dose",
  "duration_days",
  "grp",
  "id",
  "edge_type",
  "start",
  "end",
  "level",
  "header_indicator",
  "therapy_start",
  "therapy_end",
  "reference_specimen_type",
  "conceptId",
  "pt_term",
  "icd_text",
  "prim_diag",
  "start_time",
  "end_time",
  "category_description",
  "infection_group1_code",
  "infection_group1_label",
  "infection_group2_code",
  "infection_group2_label",
  "year",
  "DDD_prescribed",
  "DOT",
  "LOT"
)

utils::globalVariables(global_vars)
utils::globalVariables(paste0(global_vars, ".x"))
utils::globalVariables(paste0(global_vars, ".y"))
utils::globalVariables(.drug_prescriptions_variables()[["variable_name"]])
utils::globalVariables(.drug_administrations_variables()[["variable_name"]])
utils::globalVariables(.inpatient_diagnoses_lookup_variables()[["variable_name"]])
utils::globalVariables(.inpatient_diagnoses_data_variables()[["variable_name"]])
utils::globalVariables(.inpatient_episodes_variables()[["variable_name"]])
utils::globalVariables(.inpatient_wards_variables()[["variable_name"]])
utils::globalVariables(.inpatient_microbiology_variables()[["specimens"]][["variable_name"]])
utils::globalVariables(.inpatient_microbiology_variables()[["isolates"]][["variable_name"]])
utils::globalVariables(.inpatient_microbiology_variables()[["susceptibilities"]][["variable_name"]])