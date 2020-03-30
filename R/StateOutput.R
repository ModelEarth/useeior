#' Get industry-level GDP for all states at a specific year.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A dataframe contains state GDP for all states at a specific year.
getStateGDP <- function(year) {
  # Load pre-saved state GDP 2007-2018 
  StateGDP <- useeior::State_GDP_2007_2018
  StateGDP <- StateGDP[, c("GeoName", "LineCode", as.character(year))]
  return(StateGDP)
}

#' Map state table to BEA Summary, mark sectors that need allocation
#' @param statetablename Name of pre-saved state table, canbe GDP, Tax, Employment Compensation, and GOS.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
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
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A dataframe contains allocation factors for all states with row names being BEA sector code.
calculateStatetoBEASummaryAllocationFactor <- function(year, allocationweightsource) {
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Determine BEA sectors that need allocation
  allocation_sectors <- BEAStateGDPtoBEASummary[duplicated(BEAStateGDPtoBEASummary$LineCode) | duplicated(BEAStateGDPtoBEASummary$LineCode, fromLast = TRUE), ]
  allocation_codes <- allocation_sectors$BEA_2012_Summary_Code
  # Generate a mapping table only for allocation_codes based on MasterCrosswalk2012
  crosswalk <- useeior::MasterCrosswalk2012[useeior::MasterCrosswalk2012$BEA_2012_Summary_Code%in%allocation_codes, ]
  # Generate allocation_weight df based on pre-saved data
  if (allocationweightsource=="Employment") {
    # Load BEA State Emp to BEA Summary mapping
    BEAStateEmptoBEAmapping <- utils::read.table(system.file("extdata", "Crosswalk_StateEmploymenttoBEASummaryIO2012Schema.csv", package = "useeior"),
                                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    BEAStateEmptoBEAmapping <- BEAStateEmptoBEAmapping[BEAStateEmptoBEAmapping$BEA_2012_Summary_Code%in%
                                                         crosswalk[crosswalk$BEA_2012_Sector_Code%in%c("44RT", "FIRE", "G"), "BEA_2012_Summary_Code"], ]
    # For real estate (FIRE) and gov (G) sectors, calculate allocation factors using US GDP/Gross Output
    allocation_factors <- merge(BEAStateEmptoBEAmapping,
                                useeior::Summary_GrossOutput_IO[, as.character(year), drop = FALSE],
                                by.x = "BEA_2012_Summary_Code", by.y = 0)
    for (linecode in unique(allocation_factors$LineCode)) {
      weight_vector <- allocation_factors[allocation_factors$LineCode==linecode, as.character(year)]
      allocation_factors[allocation_factors$LineCode==linecode, "factor"] <- weight_vector/sum(weight_vector)
    }
    # Load BEA state Emp
    BEAStateEmployment <- useeior::State_Employment_2009_2018[, c("GeoFips", "GeoName", "LineCode", as.character(year))]
    # Map BEA state Emp (from LineCode) to BEA Summary
    BEAStateEmployment <- merge(BEAStateEmployment,
                                allocation_factors[, c("BEA_2012_Summary_Code", "LineCode", "factor")],
                                by = "LineCode")
    # Adjust BEA state Emp value based on allocation factor
    BEAStateEmployment[, as.character(year)] <- BEAStateEmployment[, as.character(year)]*BEAStateEmployment$factor
    allocation_weight <- stats::aggregate(BEAStateEmployment[, as.character(year)],
                                          by = list(BEAStateEmployment$GeoFips,
                                                    BEAStateEmployment$GeoName,
                                                    BEAStateEmployment$BEA_2012_Summary_Code),
                                          sum)
    colnames(allocation_weight) <- c("GeoFips", "GeoName", "BEA_2012_Summary_Code", "Weight")
    allocation_weight$GeoFips <- as.numeric(allocation_weight$GeoFips)
  }
  # Calculate allocation factor
  allocation_df <- merge(allocation_weight, allocation_sectors, by = "BEA_2012_Summary_Code")
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
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
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

#' Calculate state-US GDP (value added) ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
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

#' Calculate state-US GDP (value added) ratios by BEA State LineCode.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A dataframe contains ratios of state/US GDP (value added) for all states at a specific year by BEA State LineCode.
calculateStateUSVARatiobyLineCode <- function(year) {
  # Load LineCode-coded State ValueAdded
  StateValueAdded <- getStateGDP(year)
  # Aggregate to USValueAdded
  USValueAdded <- stats::aggregate(StateValueAdded[, as.character(year)], by = list(StateValueAdded$LineCode), sum)
  colnames(USValueAdded) <- c("LineCode", as.character(year))
  # MergeLineCode-coded State and US ValueAdded
  StateUSValueAdded <- merge(StateValueAdded, USValueAdded, by = "LineCode")
  # Calculate the state-US ValueAdded ratios by LineCode
  StateUSValueAdded$Ratio <- StateUSValueAdded[, paste0(year, ".x")]/StateUSValueAdded[, paste0(year, ".y")]
  StateUSValueAdded <- StateUSValueAdded[order(StateUSValueAdded$LineCode, StateUSValueAdded$GeoName),
                                         c("LineCode", "GeoName", "Ratio")]
  return(StateUSValueAdded)
}

#' Calculate state industry output by BEA State LineCode via multiplying state_US_VA_ratio_LineCode by USGrossOutput_LineCode.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A dataframe contains state industry output by BEA State LineCode.
calculateStateIndustryOutputbyLineCode <- function(year) {
  # Generate state_US_VA_ratio_LineCode
  state_US_VA_ratio_LineCode <- calculateStateUSVARatiobyLineCode(year)
  # Load US Gross Output
  USGrossOutput <- useeior::Summary_GrossOutput_IO[, as.character(year), drop = FALSE]
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Generate LineCode-coded US Gross Output
  USGrossOutput <- merge(USGrossOutput, BEAStateGDPtoBEASummary, by.x = 0, by.y = "BEA_2012_Summary_Code")
  USGrossOutput <- stats::aggregate(USGrossOutput[, as.character(year)], by = list(USGrossOutput$LineCode), sum)
  colnames(USGrossOutput) <- c("LineCode", as.character(year))
  # Calculate state industry output by LineCode
  StateGrossOutput <- merge(state_US_VA_ratio_LineCode, USGrossOutput, by = "LineCode")
  StateGrossOutput[, as.character(year)] <- StateGrossOutput[, as.character(year)]*StateGrossOutput$Ratio
  # Re-order
  StateGrossOutput <- StateGrossOutput[order(StateGrossOutput$LineCode, StateGrossOutput$GeoName),
                                       c("LineCode", "GeoName", as.character(year))]
  return(StateGrossOutput)
}

#' Estimate state commodity output
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
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

