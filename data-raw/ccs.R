## code to prepare `data/ccs.rda dataset
library(dplyr)
library(magrittr)
tempfile_ccs <- tempfile()
download.file("https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip", tempfile_ccs)
ccs <- read.csv(unz(tempfile_ccs, 
                    grep("ccs_dx_icd10cm_.+[.]csv$", unzip(tempfile_ccs, list = T)$Name, value = T)), 
                stringsAsFactors = F)
names(ccs) <- c(
  "icd_code",
  "ccs_cat_code",
  "icd_desc",
  "ccs_cat_desc",
  "ccs_L1_code",
  "ccs_L1_desc",
  "ccs_L2_code",
  "ccs_L2_desc"
)
ccs <- ccs[, -3]
for (i in seq_len(ncol(ccs))) {
  ccs[, i] <- gsub("(^')|('$)", "", ccs[, i])
}

ccs <- ccs %>% 
  arrange(icd_code) %>% 
  mutate(icd_code_4chars = gsub("X$", "", substr(icd_code, 0, 4)),
         icd_code_3chars = gsub("X$", "", substr(icd_code, 0, 3))) %>% 
  group_by(icd_code_4chars) %>% 
  mutate(keep_4chars = (row_number(icd_code) ==  1)) %>% 
  ungroup() %>% 
  group_by(icd_code_3chars) %>% 
  mutate(keep_3chars = (row_number(icd_code) ==  1)) %>% 
  ungroup()

usethis::use_data(ccs, overwrite = T)
