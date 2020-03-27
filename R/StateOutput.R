#' Get industry-level GDP for all states at a specific year.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains state GDP for all states at a specific year.
getStateGDP <- function(year) {
  # Load pre-saved state GDP 2007-2018 
  StateGDP <- useeior::State_GDP_2007_2018
  StateGDP <- StateGDP[, c("GeoName", "LineCode", as.character(year))]
  return(StateGDP)
}

#' Map state table to BEA Summary, mark sectors that need allocation
#' @param statetablename Name of pre-saved state table, canbe GDP, Tax, Employment Compensation, and GOS.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains state value for all states with row names being BEA sector code.
mapStateTabletoBEASummary <- function(statetablename, year) {
  if (statetablename=="GDP") {
    StateTable <- getStateGDP(year)
  } # Add else-if statement for other state table options
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Merge state table with BEA Summary sector code and name
  StateTableBEA <- merge(StateTable, BEAStateGDPtoBEASummary, by = "LineCode")
  return(StateTableBEA)
}

#' Calculate allocation factors based on state-level data, such as employment
#' @param statetablename Name of pre-saved state table, canbe GDP, Tax, Employment Compensation, and GOS.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains allocation factors for all states with row names being BEA sector code.
calculateStatetoBEASummaryAllocationFactor <- function(year, allocationweightsource) {
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Determine BEA sectors that need allocation
  allocation_sectors <- BEAStateGDPtoBEASummary[duplicated(BEAStateGDPtoBEASummary$LineCode)| duplicated(BEAStateGDPtoBEASummary$LineCode, fromLast = TRUE), ]
  allocation_codes <- allocation_sectors$BEA_2012_Summary_Code
  # Generate a mapping table only for allocation_codes based on MasterCrosswalk2012
  crosswalk <- useeior::MasterCrosswalk2012[useeior::MasterCrosswalk2012$BEA_2012_Summary_Code%in%allocation_codes, ]
  # Load pre-saved data and use it as weight for allocation
  if (allocationweightsource=="Employment") {
    # Use BEA state emp for retail sectors (44RT)
    BEAStateEmployment <- useeior::State_Employment_2009_2018[, c("GeoFips", "GeoName", "LineCode", as.character(year))]
    # Map BEA state emp (from LineCode) to BEA Summary
    BEAStateEmptoBEAmapping <- utils::read.table(system.file("extdata", "Crosswalk_StateEmploymenttoBEASummaryIO2012Schema.csv", package = "useeior"),
                                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    BEAStateEmptoBEAmapping <- BEAStateEmptoBEAmapping[BEAStateEmptoBEAmapping$BEA_2012_Summary_Code%in%
                                                         crosswalk[crosswalk$BEA_2012_Sector_Code=="44RT", "BEA_2012_Summary_Code"], ]
    BEAStateEmployment <- merge(BEAStateEmployment, BEAStateEmptoBEAmapping, by = "LineCode")
    BEAStateEmployment <- stats::aggregate(BEAStateEmployment[, as.character(year)],
                                           by = list(BEAStateEmployment$GeoFips,
                                                     BEAStateEmployment$GeoName,
                                                     BEAStateEmployment$BEA_2012_Summary_Code),
                                           sum)
    colnames(BEAStateEmployment) <- c("GeoFips", "GeoName", "BEA_2012_Summary_Code", "Weight")
    # Use BLS QCEW state Emp data for real estate and gov sectors
    OtherStateEmptoBEAmapping <- crosswalk[crosswalk$BEA_2012_Sector_Code%in%c("FIRE", "G"), ]
    if (year > 2013) {
      library(reticulate)
      flowsa <- import("flowsa")
      # Load BLS QCEW Emp data from flowsa
      OtherStateEmployment <- flowsa$getFlowByActivity("Employment", list(as.character(year)), "BLS_QCEW_EMP")[, c("FIPS", "ActivityProducedBy", "FlowAmount")]
      # Assign state name
      fips <- flowsa$common$read_stored_FIPS()
      fips$State <- as.character(fips$State)
      OtherStateEmployment <- merge(OtherStateEmployment, fips, by = "FIPS")
    } else {
      # Load pre-saved BLS QCEW Emp data
      OtherStateEmployment <- get(paste("BLS_QCEW", year, sep = "_"))[, c("GeoFips", "GeoName", "NAICS", "EMP")]
      colnames(OtherStateEmployment) <- c("FIPS", "State", "ActivityProducedBy", "FlowAmount")
    }
    # Map OtherStateEmployment (from NAICS/ActivityProducedBy, with OtherStateEmptoBEAmapping) to BEA Summary
    OtherStateEmployment <- merge(OtherStateEmployment,
                                  OtherStateEmptoBEAmapping[nchar(OtherStateEmptoBEAmapping$NAICS_2012_Code)==3, ],
                                  by.x = "ActivityProducedBy", by.y = "NAICS_2012_Code")
    OtherStateEmployment <- stats::aggregate(OtherStateEmployment$FlowAmount,
                                             by = list(OtherStateEmployment$FIPS,
                                                       OtherStateEmployment$State,
                                                       OtherStateEmployment$BEA_2012_Summary_Code),
                                             sum)
    colnames(OtherStateEmployment) <- colnames(BEAStateEmployment)
    allocationweight <- rbind(BEAStateEmployment, OtherStateEmployment)
  }
  # Calculate allocation factor
  allocation_df <- merge(allocationweight, allocation_sectors, by = "BEA_2012_Summary_Code")
  for (state in unique(allocation_df$GeoName)) {
    for (linecode in unique(allocation_df$LineCode)) {
      weight_vector <- allocation_df[allocation_df$GeoName==state&allocation_df$LineCode==linecode, "Weight"]
      allocation_df[allocation_df$GeoName==state&allocation_df$LineCode==linecode, "AllocationFactor"] <- weight_vector/sum(weight_vector)
    }
  }
  return(allocation_df)
}

#' Allocate state table (GDP, Tax, Employment Compensation, and GOS) to BEA Summary based on specified weight
#' @param statetablename Name of pre-saved state table, canbe GDP, Tax, Employment Compensation, and GOS.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @param allocationweightsource Source of allocatino weight, can be "Employment".
#' @return A dataframe contains allocated state value for all states with row names being BEA sector code.
allocateStateTabletoBEASummary <- function(statetablename, year, allocationweightsource) {
  # Generate StateTableBEA
  StateTableBEA <- mapStateTabletoBEASummary(statetablename, year)
  # Generate allocation factor
  allocation_df <- calculateStatetoBEASummaryAllocationFactor(year, allocationweightsource)
  # Merge StateTableBEA with allocation_df
  StateTableBEA_allocation <- merge(StateTableBEA, allocation_df, by = c("LineCode", "GeoName", "BEA_2012_Summary_Code"))
  # Modify value in StateTableBEA_allocation
  StateTableBEA_allocation[, as.character(year)] <- StateTableBEA_allocation[, as.character(year)]*StateTableBEA_allocation$AllocationFactor
  # Append StateTableBEA_allocation to un-allocated StateTableBEA
  StateTableBEA <- rbind(StateTableBEA_allocation[, c("GeoName", "BEA_2012_Summary_Code", as.character(year))],
                         StateTableBEA[!StateTableBEA$LineCode%in%StateTableBEA_allocation$LineCode,
                                       c("GeoName", "BEA_2012_Summary_Code", as.character(year))])
  # Sort StateTableBEA
  StateTableBEA <- StateTableBEA[order(StateTableBEA$GeoName, StateTableBEA$BEA_2012_Summary_Code), ]
  # Re-number rownames
  rownames(StateTableBEA) <- NULL
  return(StateTableBEA)
}

# Calculate state-US GDP (value added) ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2018 specifying the year of interest.
#' @return A dataframe contains ratios of state/US GDP (value added) for all states at a specific year at BEA Summary level.
calculateStateUSValueAddedRatio <- function(year) {
  # Generate state GDP (value added) table
  StateValueAdded <- allocateStateTabletoBEASummary("GDP", year, "Employment")
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


#' Estimate state commodity output
#' @param year Abbreviated location name of the model, e.g. "US" or "GA".
#' @return A dataframe contains state commodity output for specified state with row names being BEA sector code.
getStateCommodityOutputEstimates <- function(year) {
  # Import flowsa
  #library(reticulate)
  #flowsa <- import("flowsa")
  
  # Agriculture
  # pre-saved CoA data
  #CoA <- flowsa$getFlowByActivity("CoA", list(as.character(year)), "USDA_CoA_Cropland")
  # USDA 2012 Census Report (by BEA)
  AgOutput <- utils::read.table(system.file("extdata", "USDAStateAgProdMarketValueByBEA.csv", package = "useeior"),
                                sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                                colClasses = c("character",rep("numeric",51)))
  # Reshape table from wide to long
  AgOutput <- reshape2::melt(AgOutput, id.vars = "BEA_389")
  colnames(AgOutput) <- c("BEA_2012_Detail_Code", "GeoName", "Output")
  
  # Forestry
  # read in USDA-FS Forest Product Cut Values data by state (FY2012)
  ForestOutput <- utils::read.table(system.file("extdata", "ForestProdCutValueFY2012.csv", package = "useeior"),
                                    sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character", "numeric"))
  # Add BEA Summary code
  ForestOutput <- cbind.data.frame("113000", ForestOutput)
  colnames(ForestOutput) <- c("BEA_2012_Detail_Code", "GeoName", "Output")
  ForestOutput$Output <- ForestOutput$Output*1E-6
  
  # Fishery
  # read in NOAA Fishery Landings data by state by year (2010-2016)
  FisheryOutput <- utils::read.table(system.file("extdata", "FisheryLandings2010-2016.csv", package = "useeior"),
                                     sep = ",", header = TRUE, check.names = FALSE, colClasses = c("numeric", "character", "numeric"))
  # calculate State/US Fishery Output ratio
  FisheryOutput <- cbind.data.frame("114000", FisheryOutput[FisheryOutput$Year==year, ])
  FisheryOutput$Year <- NULL
  colnames(FisheryOutput) <- c("BEA_2012_Detail_Code", "GeoName", "Output")
  FisheryOutput$Output <- FisheryOutput$Output*1E-6
  
  # Assemble Commodity Output from Agriculture, Forestry and Fishery
  StateCommodityOutput <- rbind(AgOutput, ForestOutput, FisheryOutput)
  return(StateCommodityOutput)
}

