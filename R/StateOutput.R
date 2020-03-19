#' Get industry-level GDP for all states at a specific year.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains state GDP for all states at a specific year.
getStateGDP <- function(year) {
  # Load pre-saved state GDP 2007-2018 
  StateGDP <- useeior::State_GDP_2007_2018
  StateGDP <- StateGDP[, c("GeoName", "LineCode", as.character(year))]
  return(StateGDP)
}

#' Map state table (GDP, Tax, Employment Compensation, and GOS) to BEA Summary, apply allocation based on BLS QCEW Employment
#' @param statetable Pre-saved state table (GDP, Tax, Employment Compensation, and GOS).
#' @return A dataframe contains state industry output for specified state with row names being BEA sector code.
mapStateTabletoBEASummary <- function(statetable) {
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Merge state table with BEA Summary sector code and name
  StateTableBEA <- merge(statetable, BEAStateGDPtoBEASummary, by = "LineCode")
  # Change column name for processing convenience
  colnames(StateTableBEA)[3] <- "OriginalValue"
  # Determine code of BEA sectors that need allocation
  allocation_codes <- BEAStateGDPtoBEASummary[duplicated(BEAStateGDPtoBEASummary$LineCode)|
                                                duplicated(BEAStateGDPtoBEASummary$LineCode, fromLast = TRUE),
                                              "BEA_2012_Summary_Code"]
  # Generate a mapping table only for allocation_codes based on MasterCrosswalk2012
  crosswalk <- useeior::MasterCrosswalk2012[useeior::MasterCrosswalk2012$BEA_2012_Summary_Code%in%allocation_codes, ]
  # Import flowsa to assist allocation of certain sectors' GDP
  library(reticulate)
  flowsa <- import("flowsa")
  # Load pre-saved BLS QCEW Employment data
  QCEW_EMP <- flowsa$getFlowByActivity("Employment", list(as.character(year)), "BLS_QCEW_EMP")
  # Merge QCEW_EMP with crosswalk
  QCEW_EMP <- merge(QCEW_EMP[, c("FIPS", "ActivityProducedBy", "FlowAmount")],
                    crosswalk[nchar(crosswalk$NAICS_2012_Code)==3, ],
                    by.x = "ActivityProducedBy", by.y = "NAICS_2012_Code")
  # Load fips from flowsa
  fips <- flowsa$common$read_stored_FIPS()
  fips$State <- as.character(fips$State)
  fips$FIPS <- as.numeric(fips$FIPS)
  # Merge QCEW_EMP with FIPS to get state name
  QCEW_EMP <- merge(QCEW_EMP, fips[, c("State", "FIPS")], by = "FIPS")
  # Aggregate QCEW_EMP by state and BEA Summary code
  QCEW_EMP <- stats::aggregate(QCEW_EMP$FlowAmount, by = list(QCEW_EMP$State, QCEW_EMP$BEA_2012_Summary_Code), sum)
  colnames(QCEW_EMP) <- c("State", "BEA_2012_Summary_Code", "EMP")
  # Merge StateTableBEA with QCEW_EMP to get the sectors need allocation
  StateTableBEA_QCEW <- merge(StateTableBEA, QCEW_EMP,
                              by.x = c("GeoName", "BEA_2012_Summary_Code"),
                              by.y = c("State", "BEA_2012_Summary_Code"))
  # Apply allocation to StateTableBEA_QCEW
  for (state in unique(StateTableBEA_QCEW$GeoName)) {
    for (linecode in unique(StateTableBEA_QCEW$LineCode)) {
      value_vector <- StateTableBEA_QCEW[StateTableBEA_QCEW$GeoName==state&StateTableBEA_QCEW$LineCode==linecode, "OriginalValue"]
      weight_vector <- StateTableBEA_QCEW[StateTableBEA_QCEW$GeoName==state&StateTableBEA_QCEW$LineCode==linecode, "EMP"]
      allocatedvalues <- value_vector*(weight_vector/sum(weight_vector))
      StateTableBEA_QCEW[StateTableBEA_QCEW$GeoName==state&StateTableBEA_QCEW$LineCode==linecode, "AllocatedValue"] <- allocatedvalues
    }
  }
  StateTableBEA_QCEW$OriginalValue <- StateTableBEA_QCEW$AllocatedValue
  # Append StateTableBEA_QCEW to un-allocated StateTableBEA
  StateTableBEA <- rbind(StateTableBEA_QCEW[, c("GeoName", "BEA_2012_Summary_Code", "OriginalValue")],
                         StateTableBEA[!StateTableBEA$LineCode%in%StateTableBEA_QCEW$LineCode,
                                       c("GeoName", "BEA_2012_Summary_Code", "OriginalValue")])
  # Change column names of StateTableBEA
  colnames(StateTableBEA)[3] <- colnames(statetable)[3]
  # Sort StateTableBEA
  StateTableBEA <- StateTableBEA[order(StateTableBEA$GeoName, StateTableBEA$BEA_2012_Summary_Code), ]
  # Re-number rownames
  rownames(StateTableBEA) <- NULL
  return(StateTableBEA)
}

# Calculate state-US GDP (value added) ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains ratios of state/US GDP (value added) for all states at a specific year at BEA Summary level.
generateStateUSValueAddedRatios <- function(year) {
  # Load state GDP (value added) table
  StateValueAdded <- getStateGDP(year)
  # Map state GDP (value added) table to BEA summary level
  StateValueAdded <- mapStateTabletoBEASummary(StateValueAdded)
  # Load US value added table
  USValueAdded <- useeior::Summary_ValueAdded_IO[, as.character(year), drop = FALSE]
  # Merge state GDP and US value added tables
  StateUSValueAdded <- merge(StateValueAdded, USValueAdded, by.x = "BEA_2012_Summary_Code", by.y = 0)
  # Calculate the state-US GDP (value added) ratios
  StateUSValueAdded$Ratio <- StateUSValueAdded[, paste0(year, ".x")]/StateUSValueAdded[, paste0(year, ".y")]
  StateUSValueAdded <- StateUSValueAdded[order(StateUSValueAdded$GeoName, StateUSValueAdded$BEA_2012_Summary_Code),
                                         c("BEA_2012_Summary_Code", "GeoName", "Ratio")]
  rownames(StateUSValueAdded) <- NULL
  return(StateUSValueAdded)
}

#' Estimate state output based on alternative sources.
#' @param industry A BEA industry name.
#' @return A dataframe contains state output for all states and a specific industry.
getAlternativeStateIndustryOutputEstimates <- function(industry) {
  # Census B. EconomicCensus RCPTotal, No_Establishments, PAYANN, No_Employees for most all sectors:
  # SI/Census/EconomicCensus_2012.csv, mapped to BEA using mapEconomicCensus2012toBEA() function
  #
  # BTS RailTransport No employees: SI/BTS/RailTransEmployment_2011.csv
  #
  # FederalGov No. of employees (best for industry so may not be needed): SI/FedGov/FedGovEmployment2012.csv
  # 
  # Postal service No. of employees: SI/USPS/USPSWorkforce2012.csv
  # 
  # Census B. State and Local Gov PAYANN: SI/Census/GovCensus_StateLocal_2012.csv  
  # 
  # BLS QCEW employment for all states and industries: SI/BLS/QCEW_GA_2012_Static.csv, QCEW_GA_2014_Static.csv
}

#' Estimate Summary-level state industry output based on BEA and alternative sources.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains Summary-level state industry output for all states and a specific industry.
getStateIndustryOutput <- function(year) {
  
}

#' Estimate state commodity output
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @return A dataframe contains state commodity output for specified state with row names being BEA sector code.
getStateCommodityOutputEstimates <- function(location_acronym) {
  
}

