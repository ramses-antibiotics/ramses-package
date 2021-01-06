devtools::load_all()
library(dplyr)
library(magrittr)
library(readxl)


# Budd E, Cramp E, Sharland M, Hand K, Howard P, Wilson P, Wilcox M, Muller-Pebody B, Hopkins S (2019). 
# “Adaptation of the WHO Essential Medicines List for national antibiotic stewardship policy in England: being AWaRe.” 
# Journal of Antimicrobial Chemotherapy, 74(11), 3384-3389. ISSN 0305-7453, doi: 10.1093/jac/dkz321.

aware_england <- read.csv("data-raw/aware_england.csv", 
                          colClasses = "character",
                          stringsAsFactors = FALSE)
aware_england <- dplyr::bind_rows(
  aware_england,
  data.frame(list(
    ATC_name = "METRONIDAZOLE",
    ATC_code = "J01XD01",
    ATC_route = "O",
    aware_category = "Access",
    VTM_code = "1222004",
    VTM_name = "Metronidazole"
  ))
)
aware_england$version <- "England"
aware_england$year <- 2019
aware_england$ATC_name <- get_ATC_name(aware_england$ATC_code)
VTM_reference <- dplyr::distinct(aware_england[,c("ATC_code", "VTM_code", "VTM_name")])
# aware_england$ab_code <- AMR::as.ab(aware_england$ATC_code)
# aware_england$ab_name <- AMR::ab_name(aware_england$ab_code)


# The 2019 WHO AWaRe classification of antibiotics for evaluation and monitoring of use. 
# Geneva: World Health Organization; 2019. (WHO/EMP/IAU/2019.11).
# 
# who_source <- tempfile()
# download.file("https://apps.who.int/iris/bitstream/handle/10665/327957/WHO-EMP-IAU-2019.11-eng.xlsx",
#               destfile = who_source)
# aware_who <- readxl::read_xlsx(who_source, sheet = 2, skip = 2)[,1:5]
# names(aware_who) <- c("antibiotic_name", "ATC_group", "ATC_code", "aware_category", "present_in_EML2019")
# aware_who$ATC_route <- gsub(
#   "([(])|([)])", "", 
#   regmatches(aware_who$antibiotic_name, 
#              gregexpr("[(].*[)]$", 
#                       aware_who$antibiotic_name))
# )
# aware_who <- split(aware_who, aware_who$ATC_route)
# 
# aware_who$ATC_code[aware_who$ATC_code == "to be assigned"] <- NA
# aware_who$ATC_code <- gsub("[:blank:]| ", "", aware_who$ATC_code)
#   
# # New ATC codes 2020
# aware_who$ATC_code[aware_who$antibiotic_name == "Plazomicin"] <- "J01GB14"
# aware_who$ATC_code[aware_who$antibiotic_name == "Omadacycline"] <- "J01AA15"
# 
# aware_who <- filter(aware_who, !is.na(ATC_code))
# aware_who$version <- "WHO"
# aware_who$year <- 2019
# # aware_who$ab_code <- AMR::as.ab(aware_who$ATC_code)
# aware_who$ATC_name <- get_ATC_name(aware_who$ATC_code)
# # aware_who$ab_name <- AMR::ab_name(aware_who$ab_code)
# aware_who <- merge(aware_who, VTM_reference, all.x = T, all.y = F)
# 
# # TODO: add antiinfective category and atc class
# reference_aware <- bind_rows(aware_who, aware_england) %>% 
#   select(ATC_code,
#          ATC_route,
#          ATC_name,
#          aware_category,
#          version,
#          year,
#          # ab_code,
#          # ab_name,
#          VTM_code,
#          VTM_name) %>% 
#   arrange(version, year, ATC_name)

reference_aware <- select(
  aware_england,
  ATC_code,
  ATC_route,
  ATC_name,
  aware_category,
  version,
  year,
  VTM_code,
  VTM_name
) %>% 
  arrange(version, year, ATC_name, ATC_code)

usethis::use_data(reference_aware, overwrite = T)
