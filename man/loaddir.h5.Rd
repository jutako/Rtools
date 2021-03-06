% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h5.tools.R
\name{loaddir.h5}
\alias{loaddir.h5}
\title{Load data from all hdf5 files in a directory, output a list}
\usage{
loaddir.h5(indir, element, pattern = "*.h5")
}
\arguments{
\item{indir}{character, Directory name}

\item{element}{character, Data element to load}

\item{pattern}{character, File name pattern to use}
}
\value{
A list of data matrices
}
\description{
Load data from all hdf5 files in a directory, output a list
}
\examples{
\dontrun{
indir <- '/path/to/h5/'
pattern <- '.*NB01.*_ERPdata.h5'
element <- '/erp'
}

}
