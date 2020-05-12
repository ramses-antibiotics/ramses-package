## code to prepare `data-raw/AWaRe_England.csv` dataset goes here
aware <- read.csv("data-raw/AWaRe_England.csv", stringsAsFactors = F)
usethis::use_data(aware, overwrite = T)
