% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{download_icd10cm}
\alias{download_icd10cm}
\title{Download the ICD-10-CM reference file by the US National Center for 
Health Statistics}
\usage{
download_icd10cm(silent = FALSE)
}
\arguments{
\item{silent}{if \code{TRUE}, suppress status messages, and the progress bar.
The default is \code{FALSE}.}
}
\value{
A data frame containing ICD-10-CM codes 
(\code{icd_code}) and labels (\code{icd_description})
}
\description{
Download the ICD-10-CM reference file by the US National Center for 
Health Statistics
}
