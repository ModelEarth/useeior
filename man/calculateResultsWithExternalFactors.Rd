% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculationFunctions.R
\name{calculateResultsWithExternalFactors}
\alias{calculateResultsWithExternalFactors}
\title{Calculate total emissions/resources (LCI) and total impacts (LCIA) for an EEIO model that has external import factors
for a given demand vector.
Note that for this calculation, perspective is always FINAL}
\usage{
calculateResultsWithExternalFactors(
  model,
  perspective = "FINAL",
  demand = "Consumption",
  location = NULL,
  use_domestic_requirements = FALSE,
  household_emissions = FALSE,
  show_RoW = FALSE
)
}
\arguments{
\item{model}{A complete EEIO model: a list with USEEIO model components and attributes.}

\item{perspective}{Perspective of the model, can be "DIRECT" or "FINAL". "DIRECT" perspective
aligns results with the sectors in which they are produced, while "FINAL" perspective aligns
results with the sectors consumed by the final user.}

\item{demand}{A demand vector, can be name of a built-in model demand vector, e.g. "Production" or "Consumption"}

\item{location, }{str optional location code for demand vector, required for two-region models}

\item{use_domestic_requirements}{bool, if TRUE, return only domestic portion of results}

\item{household_emissions, }{bool, if TRUE, include calculation of emissions from households}

\item{show_RoW, }{bool, if TRUE, include rows for commodities in RoW, e.g. `111CA/RoW` in result objects.}
}
\value{
A list with LCI and LCIA results (in data.frame format) of the EEIO model.
}
\description{
Calculate total emissions/resources (LCI) and total impacts (LCIA) for an EEIO model that has external import factors
for a given demand vector.
Note that for this calculation, perspective is always FINAL
}
