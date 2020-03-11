#' International Classification of Diseases
#' 
#' @description The World Health Organisation (WHO) International 
#' Statistical Classification of Diseases and Related Health 
#' Problems Tenth Revision is available under restrictive licence terms.
#' 
#' The UK implementation of ICD-10 is available from the NHS TRUD website
#' \url{https://isd.digital.nhs.uk/trud3/user/guest/group/0/pack/28} 
#' at no cost to UK users subject to a licence agreement.
#'  
#' The US implementation of ICD-10-CM is available from 
#' \url{https://www.cdc.gov/nchs/icd/icd10cm.htm}
#'
#' @param archive path to the ZIP archive of the ICD-10 release 
#' (5th Edition is recommended)
#' @param version a character string labelling the edition 
#' (e.g. "GB 5th Edition")
#'
#' @return A data frame ready for importing into the database.
#' @import dplyr magrittr  
#' @export
#'
#' @examples
#' \dontrun{
#' import_icd("icd_df_10.5.0_20151102000001.zip", "GB 5th Edition")
#' }
import_icd <- function(archive, version) {
  
  temp <- tempfile()
  source_files <- utils::unzip(archive, exdir = temp)
  # 
  # icd_source <- readr::read_tsv(
  #   grep("CodesAndTitlesAndMetadata_GB", source_files, value = T),
  #   guess_max = 10000)
  # 
  icd_source <- readr::read_delim(
    grep("CodesAndTitlesAndMetadata_GB", source_files, value = T),
    delim = "\t", guess_max = 10000)
  rm(temp)
  
  colnames(icd_source) <- tolower(colnames(icd_source))
  
  icd_source$level <- stringr::str_length(icd_source[["alt_code"]])
  
  icd3 <- filter(icd_source, .data$level == 3) %>% 
    transmute(category = .data$alt_code,
              category_description = .data$description)
  
  icd5 <- filter(icd_source, .data$level > 3) %>% 
    mutate(category = substring(.data$alt_code, 0, 3))

  icd <- merge(icd5, icd3, by = "category", all.x = T)

  icd$category_description[is.na(icd$category_description)] <- 
    icd$description[is.na(icd$category_description)]
  
  icd$edition <- version
  
  icd

}
 