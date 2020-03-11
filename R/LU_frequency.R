#' Prescription frequency abbreviation look-up table
#' @description This look-up table provides the number of times a drug should 
#' be administered per day based on the prescription frequency abbreviation 
#' (including latin abbreviations).
#' 
#' Example: BD stands for \emph{bis die}, meaning twice a day. The 
#' corresponding `daily_freq`` value is 2.
#' 
#' @format A data frame with 64 abbreviations and their corresponding daily frequency value.
#' @docType data
#' @name LU_frequency
"LU_frequency"