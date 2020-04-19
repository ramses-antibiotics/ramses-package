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
aware_england$version <- "England"
aware_england$year <- 2019
aware_england$ATC_name <- get_ATC_name(aware_england$ATC_code)
VTM_reference <- dplyr::distinct(aware_england[,c("ATC_code", "VTM_code", "VTM_name")])
aware_england$WHONET_ab_code <- AMR::as.ab(aware_england$ATC_code)
aware_england$WHONET_ab_name <- AMR::ab_name(aware_england$WHONET_ab_code)


# The 2019 WHO AWaRe classification of antibiotics for evaluation and monitoring of use. 
# Geneva: World Health Organization; 2019. (WHO/EMP/IAU/2019.11).

who_source <- tempfile()
download.file("https://apps.who.int/iris/bitstream/handle/10665/327957/WHO-EMP-IAU-2019.11-eng.xlsx",
              destfile = who_source)
aware_who <- readxl::read_xlsx(who_source, sheet = 2, skip = 2)[,1:5]
names(aware_who) <- c("antibiotic_name", "ATC_group", "ATC_code", "aware_category", "present_in_EML2019")
aware_who$ATC_code[aware_who$ATC_code == "to be assigned"] <- NA
aware_who$ATC_code <- gsub("[:blank:]| ", "", aware_who$ATC_code)
aware_who <- filter(aware_who, !is.na(ATC_code))
aware_who$version <- "WHO"
aware_who$year <- 2019
aware_who$WHONET_ab_code <- AMR::as.ab(aware_who$ATC_code)
aware_who$ATC_name <- get_ATC_name(aware_who$ATC_code)
aware_who$WHONET_ab_name <- AMR::ab_name(aware_who$WHONET_ab_code)
aware_who <- merge(aware_who, VTM_reference, all.x = T, all.y = F)

# TODO: add antiinfective category and atc class
reference_aware <- bind_rows(aware_who, aware_england) %>% 
  select(ATC_code,
         ATC_route,
         ATC_name,
         aware_category,
         version,
         year,
         WHONET_ab_code,
         WHONET_ab_name,
         VTM_code,
         VTM_name) %>% 
  arrange(version, year, ATC_name)

usethis::use_data(reference_aware, overwrite = T)
