## code to prepare `data-raw/antibiotic_icd_indications.csv` dataset goes here
library(dplyr)
library(magrittr)
antibiotic_icd_indications <- read.csv("data-raw/antibiotic_icd_indications.csv", stringsAsFactors = F)
antibiotic_icd_indications <- antibiotic_icd_indications %>%
  group_by(antibiotics_indicated) %>%
  mutate(infection_group1_code = paste0(
    substr(antibiotics_indicated, 0, 1),
    formatC(dense_rank(infection_group1_label),
      width = 2, format = "d", flag = "0"
    )
  )) %>%
  ungroup()
antibiotic_icd_indications <- antibiotic_icd_indications %>%
  group_by(infection_group1_code) %>%
  mutate(infection_group2_code = paste0(
    infection_group1_code,
    formatC(dense_rank(infection_group2_label),
      width = 2, format = "d", flag = "0"
    )
  )) %>% 
  data.frame()
usethis::use_data(antibiotic_icd_indications, overwrite = T)