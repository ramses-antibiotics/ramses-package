% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{create_mock_database}
\alias{create_mock_database}
\title{Create a mock database for training/demonstration purposes}
\usage{
create_mock_database(file, timezone = Sys.timezone(), silent = FALSE)
}
\arguments{
\item{file}{A file path to an existing or new database file with a
".duckdb" extension.}

\item{timezone}{A string for the time zone in which to return data to 
R from the database. By default, it is set to \code{\link{Sys.timezone}()}.}

\item{silent}{if \code{TRUE}, the progress bar will be hidden. The default is 
\code{FALSE}.}
}
\value{
An object of class \code{duckdb_connection}.
}
\description{
Create a local database on disk using 
\code{\link[duckdb]{duckdb}()} and load it with synthetic data ready for analysis.
}
\details{
This function creates a database on disk at the desired path. 
The database and its content will persist after it is disconnected.
}
