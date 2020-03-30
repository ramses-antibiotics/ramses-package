
.drug_prescriptions_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "drug_prescriptions_variables.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

.drug_administrations_variables <- function() {
  utils::read.csv(
    system.file("Schema", 
                "drug_administrations_variables.csv", 
                package = "Ramses"), stringsAsFactors = F)
}

global_vars <- c(
  "spell_id", 
  "admission_date", 
  "discharge_date",
  "episode_number",
  "last_episode_in_spell_indicator",
  "episode_start",
  "episode_end",
  "ddd_value",
  "ddd_unit",
  "valid_ab",
  "icd_code",
  "from_id", 
  "to_id",
  "id1",
  "id2",
  "ab",
  "strength",
  "basis_of_strength",
  "authored_on",
  "duration",
  "freq",
  "LU_frequency",
  "daily_freq",
  "daily_dose",
  "duration_days",
  "grp",
  "id",
  "edge_type"
)

utils::globalVariables(global_vars)
utils::globalVariables(paste0(global_vars, ".x"))
utils::globalVariables(paste0(global_vars, ".y"))
utils::globalVariables(.drug_prescriptions_variables()[["variable_name"]])
utils::globalVariables(.drug_administrations_variables()[["variable_name"]])
