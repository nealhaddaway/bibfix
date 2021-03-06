% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_ris.R
\name{build_ris}
\alias{build_ris}
\title{Build RIS files from other sources}
\usage{
build_ris(data, save = FALSE, filename = "export", path = NULL)
}
\arguments{
\item{save}{Logical argument to specify whether the output
file should be saved as an .ris file.}

\item{filename}{Optional name of the output file if
save = TRUE. Default is 'export'.}

\item{path}{Path to which file should be saved.}

\item{input}{A dataframe object containing bibliographic
data. Each item is an independent line in the
dataframe. The dataframe must contain
columns named as follows: 'authors', 'year', 'title',
'source', 'volume', 'issue', 'start_page', 'end_page',
and 'doi'.}
}
\value{
An RIS formatted text file saved to the desired
path.
}
\description{
Builds an RIS file based on a basic input of
fields corresponding to a minimum information for
deduplication and record identification from external
API sources (e.g. CrossRef).
}
\examples{
\dontrun{
data <- read.csv('inst/extdata/data.csv')
ris <- build_ris(data, save=TRUE)
}
}
