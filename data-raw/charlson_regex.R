# Obtaining Charlson Comorbidity regular expressions from the comorbidity package
charlson_regex <- comorbidity:::lofregex$charlson$icd10
# names(charlson_regex) <- paste0("charlson_", names(charlson_regex))
usethis::use_data(charlson_regex, internal = T, overwrite = T)
