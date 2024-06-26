% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WIOFunctions.R
\name{getWIOFiles}
\alias{getWIOFiles}
\title{Setup the WIO specs based on the input files. 
This function is essentially a wrapper for disaggregateSetup() function, but included 
for clarifying the different code flows between the two model types.}
\usage{
getWIOFiles(model, configpaths = NULL)
}
\arguments{
\item{model}{An EEIO model object with model specs and IO tables loaded}

\item{configpaths}{str vector, paths (including file name) of disagg configuration file(s).
If NULL, built-in config files are used.}
}
\value{
A model object with the correct WIO specs.
}
\description{
Setup the WIO specs based on the input files. 
This function is essentially a wrapper for disaggregateSetup() function, but included 
for clarifying the different code flows between the two model types.
}
