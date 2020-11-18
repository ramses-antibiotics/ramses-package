## code to prepare `data/reference_specimen_type.rda dataset
library(dplyr)
library(magrittr)
library(snomedizer)


reference_specimen_type <- api_concepts(branch = "MAIN", 
                                        ecl = "<<123038009", 
                                        limit = 5000) %>% 
  result_flatten() %>% 
  transmute(conceptId, 
            moduleId, 
            fsn_term = fsn.term, 
            pt_term = pt.term) %>% 
  mutate(snomed_release_version = unique(result_flatten(api_code_system(shortName = "SNOMEDCT"))$latestVersion.effectiveDate))


usethis::use_data(reference_specimen_type, overwrite = TRUE)


# tessy <- read.csv("data-raw/tessy_45.csv", stringsAsFactors = F)
# tessy <- tessy[
#   !tessy$tessy_codelist %in% 
#     c("SpecimenDIPH", 
#       "SpecimenSeroVPD"), ]
# tessy <- tessy[,-1]
# for(i in ncol(tessy)){
#   tessy[[i]] <- trimws(tessy[[i]])
# }
# tessy$tessy_description[tessy$tessy_description=="Cerebro spinal fluid"] <- "Cerebrospinal fluid"
# 
# tessy <- unique(tessy)
# tessy$normally_sterile <- tessy$tessy_code %in% c(
#   "BAL", "BLOOD", "BONE", "CSF", "LUNGTISSUE", 
#   "PLEURAL", "SER", "SYNO", "URI", "URINE", 
#   "OTHSTERILE")
# tessy$other_codes <- tessy$tessy_code %in% c(
#   "O", "OTHER", "UNK", NA,
#   "NONSTERILE", "OTHSTERILE"
#   )
# tessy <- dplyr::arrange(tessy, -normally_sterile, other_codes, tessy_code)
# tessy <- dplyr::select(tessy, -other_codes)
# write.csv(tessy, file="data-raw/tessy_short.csv", row.names = FALSE)
# 
# 
# 

# -------------------------------------------------------------------------


# download.file(url = "https://www.hl7.org/fhir/conceptmap-example-specimen-type.json",
#               destfile = "data-raw/CodeSystem-v2-0487.json")
# t0487 <- jsonlite::fromJSON(readLines("data-raw/CodeSystem-v2-0487.json"))
# reference_specimen_type <- t0487$concept
#  
# read.csv( , file = "data-raw/t0487_categories.csv")

# truc <- hl7$concept %>% 
#   dplyr::select(-designation) %>% 
#   mutate(status = purrr::map_chr(property, function(X) X[X$code == "status",]$valueCode),
#          othertruc = purrr::map_chr(property, function(X) X[X$code == "v2-concComment",]$valueString))
# 
# jsonlite::flatten(hl7)
# dplyr::bind_rows(truc$property)
# 
# truc <- bind_cols(truc, dplyr::bind_rows( hl7$concept$property))
