## code to prepare `data-raw/care_episodes.csv` dataset goes here
LU_frequency <- read.csv("data-raw/LU_frequency.csv", stringsAsFactors = F)
usethis::use_data(LU_frequency, overwrite = T)
