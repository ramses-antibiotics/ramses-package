## code to prepare `data/ccsr.rda dataset
library(dplyr)
library(magrittr)

tempfile_ccsr <- tempfile()
download.file("https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR2020_1.zip", tempfile_ccsr)

ccsr <- read.csv(
  unz(tempfile_ccsr, 
      grep("[$]DXCCSR.+[.]CSV$", 
           unzip(tempfile_ccsr, list = T)$Name, 
           value = T)),
  stringsAsFactors = F)

names(ccsr) <- c(
  "icd_code", "icd_desc",
  paste0("ccsr_cat", c("_code.", "_desc."), sort(rep(1:5, 2)))
)
ccsr <- ccsr[, -2]

for (i in 1:ncol(ccsr)) {
  ccsr[, i] <- gsub("(^')|('$)", "", ccsr[, i])
}

ccsr <- reshape(
  ccsr, direction = "long",
  idvar = c("icd_code"), #, "icd_code_4chars", "keep_4chars"
  varying = paste0("ccsr_cat", c("_code.", "_desc."), sort(rep(1:5, 2))),
  timevar = "ccsr_cat_rank") %>% 
  filter(ccsr_cat_code != " ")

ccsr$ccsr_body_system_code <- substr(ccsr$ccsr_cat_code, 0, 3)

bs_lu <- data.frame(list(
  ccsr_body_system_code = c("INF", "NEO", "BLD", "END", "MBD", "NVS", "EYE", "EAR", "CIR", "RSP", "DIG", "SKN", "MUS", "GEN", "PRG", "PNL", "MAL", "SYM", "INJ", "EXT"),
  ccsr_body_system_label = c("Certain infectious and parasitic diseases", "Neoplasms", "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism", "Endocrine, nutritional and metabolic diseases", "Mental, behavioral and neurodevelopmental disorders", "Diseases of the nervous system", "Diseases of the eye and adnexa", "Diseases of the ear and mastoid process", "Diseases of the circulatory system", "Diseases of the respiratory system", "Diseases of the digestive system", "Diseases of the skin and subcutaneous tissue", "Diseases of the musculoskeletal system and connective tissue", "Diseases of the genitourinary system", "Pregnancy, childbirth and the puerperium", "Certain conditions originating in the perinatal period", "Congenital malformations, deformations and chromosomal abnormalities", "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", "Injury, poisoning and certain other consequences of external causes", "External causes of morbidity")
  ),
  stringsAsFactors = F
)

ccsr <- ccsr %>%
  arrange(icd_code) %>%
  mutate(icd_code_4chars = gsub("X$", "", substr(icd_code, 0, 4)),
         icd_code_3chars = gsub("X$", "", substr(icd_code, 0, 3))) %>%
  group_by(icd_code_4chars, ccsr_cat_rank) %>%
  mutate(keep_4chars = (dense_rank(icd_code) ==  1)) %>%
  ungroup() %>% 
  group_by(icd_code_3chars, ccsr_cat_rank) %>%
  mutate(keep_3chars = (dense_rank(icd_code) ==  1)) %>%
  ungroup() 

ccsr <- merge(ccsr, bs_lu, all.x = T)
ccsr <- ccsr[, c("icd_code", sort(names(ccsr)[-2]))]

usethis::use_data(ccsr, overwrite = T)
