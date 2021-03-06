% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Create a unified name of archive}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A character or numeric vector which represents the sequence of numbers}
}
\value{
The function returns a character vector with a unified name of a file.
}
\description{
This function transform a character or a numeric vector with the
sequence of numbers into a unified filename.
}
\examples{
\dontrun{
make_filename(2019)
make_filename(c(2019, 2018, "2017"))
}

}
