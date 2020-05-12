## code to prepare `data-raw/care_episodes.csv` dataset goes here
reference_drug_frequency <- read.csv("data-raw/reference_drug_frequency.csv", stringsAsFactors = F)
usethis::use_data(reference_drug_frequency, overwrite = T)
