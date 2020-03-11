#' Get state-level GDP for all states at a specific year.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains state GDP for all states at a specific year.
getStateGDP <- function(year) {
  # Load pre-saved state GDP 2007-2018 
  GDPtable <- useeior::State_GDP_2007_2018
  StateGDP <- StateGDP[, c("GeoName", "LineCode", as.character(year))]
  return(StateGDP)
}

#' Map state table (GDP, Tax, Employment Compensation, and GOS) to BEA Summary.
#' @param statetable Pre-saved state table (GDP, Tax, Employment Compensation, and GOS).
#' @return A dataframe contains state industry output for specified state with row names being BEA sector code.
mapStateTabletoBEASummary <- function(statetable) {
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "BEAStateGDPtoBEASummary.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Merge state table with BEA Summary sector code and name
  StateTableBEA <- merge(statetable, BEAStateGDPtoBEASummary, by = "LineCode")
  return(StateTableBEA)
}

# Calculate state-US value added ratios.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains ratios of state/US value added for all states at a specific year.
generateStateUSValueAddedRatios <- function(year) {
  # Load state value added table
  StateValueAdded <- getStateGDP(year)
  # Map state value added table to BEA summary level
  StateValueAdded <- mapStateTabletoBEA(StateValueAdded)
  # Apply allocation to the sectors whose value added needs to be allocated
  
  # Load US value added table
  USValueAdded <- model$GDP$BEAGrossOutputIO
  # Merge state and US value added tables
  StateUSValueAdded <- merge(StateValueAdded, USValueAdded, by.x = "", by.y = "")
  # Calculate the state-US value added ratios
  
  return(StateUSValueAdded)
}

#' Estimate state output based on alternative sources.
#' @param industry A BEA industry name.
#' @return A dataframe contains state output for all states and a specific industry.
getAlternativeStateIndustryOutputEstimates <- function(industry) {
  
}

#' Estimate state industry output based on BEA and alternative sources.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains state industry output for all states and a specific industry.
getStateIndustryOutput <- function(year) {
  
}

#' Estimate state commodity output
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @return A dataframe contains state commodity output for specified state with row names being BEA sector code.
getStateCommodityOutputEstimates <- function(location_acronym) {
  
}

