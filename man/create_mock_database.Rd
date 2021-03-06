% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{create_mock_database}
\alias{create_mock_database}
\title{Create a mock database for training/demonstration purposes}
\usage{
create_mock_database(file, silent = FALSE)
}
\arguments{
\item{file}{A file path to an existing or new database file with a
".sqlite" extension.}

\item{silent}{if \code{TRUE}, the progress bar will be hidden. The default is 
\code{FALSE}.}
}
\value{
An object of class \code{SQLiteConnection}.
}
\description{
Create a local database on disk using 
\code{\link[RSQLite]{SQLite}()} and load synthetic data ready for analysis.
}
\details{
This function creates a database on disk at the desired path. 
The database and its content will persist after it is disconnected.
}
\seealso{
The dbplyr website provides excellent guidance on how to connect to databases: 
\url{https://db.rstudio.com/getting-started/connect-to-database}
}
