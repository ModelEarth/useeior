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
  # Generate sum of state GDP (value added) table
  StateValueAdded_sum <- stats::aggregate(StateValueAdded[, as.character(year)],
                                          by = list(StateValueAdded$BEA_2012_Summary_Code),
                                          sum)
  colnames(StateValueAdded_sum) <- c("BEA_2012_Summary_Code", "StateVA_sum")
  # Load US value added table
  USValueAdded <- useeior::Summary_ValueAdded_IO[, as.character(year), drop = FALSE]
  # Merge sum of state GDP with US VA to get VA of Overseas region
  OverseasValueAdded <- merge(StateValueAdded_sum, USValueAdded,
                              by.x = "BEA_2012_Summary_Code", by.y = 0)
  OverseasValueAdded[, paste0(year, ".x")] <- OverseasValueAdded[, as.character(year)] - OverseasValueAdded$StateVA_sum
  OverseasValueAdded[, paste0(year, ".y")] <- OverseasValueAdded[, as.character(year)]
  OverseasValueAdded$GeoName <- "Overseas"
  # Merge state GDP and US value added tables
  StateUSValueAdded <- merge(StateValueAdded, USValueAdded, by.x = "BEA_2012_Summary_Code", by.y = 0)
  # Append Overseas VA to StateUSValueAdded
  StateUSValueAdded <- rbind(StateUSValueAdded, OverseasValueAdded[, colnames(StateUSValueAdded)])
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
  # Generate sum of State ValueAdded table
  StateValueAdded_sum <- stats::aggregate(StateValueAdded[, as.character(year)], by = list(StateValueAdded$LineCode), sum)
  colnames(StateValueAdded_sum) <- c("LineCode", as.character(year))
  # Load US ValueAdded from state GDP table
  GDPtable <- utils::read.table("inst/extdata/SAGDP/SAGDP2N__ALL_AREAS_1997_2018.csv", sep = ",",
                                header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE)
  USValueAdded <- GDPtable[GDPtable$GeoName=="United States *", c("LineCode", as.character(year))]
  # Convert values to numeric and to US $
  USValueAdded[, as.character(year)] <- as.numeric(USValueAdded[, as.character(year)])*1E6
  # Merge sum of state GDP with US VA to get VA of Overseas region
  OverseasValueAdded <- merge(StateValueAdded_sum, USValueAdded, by = "LineCode")
  OverseasValueAdded[, as.character(year)] <- OverseasValueAdded[, paste0(year, ".y")] - OverseasValueAdded[, paste0(year, ".x")]
  OverseasValueAdded$GeoName <- "Overseas"
  # Append Overseas VA to StateUSValueAdded
  StateValueAdded <- rbind(StateValueAdded, OverseasValueAdded[, colnames(StateValueAdded)])
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

#' Estimate state commodity output ratios
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A dataframe contains state commodity output for specified state with row names being BEA sector code.
getStateCommodityOutputRatioEstimates <- function(year) {
  # Import flowsa
  #library(reticulate)
  #flowsa <- import("flowsa")
  
  # Agriculture (111CA)
  # pre-saved CoA data
  #CoA <- flowsa$getFlowByActivity("CoA", list(as.character(year)), "USDA_CoA_Cropland")
  # Load USDA 2012 Census Report (by BEA)
  AgOutput <- utils::read.table(system.file("extdata", "USDAStateAgProdMarketValueByBEA.csv", package = "useeior"),
                                sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                                colClasses = c("character",rep("numeric",51)))
  # Map to BEA Summary then aggregate
  AgOutput <- merge(AgOutput, unique(useeior::MasterCrosswalk2012[, c("BEA_2012_Detail_Code", "BEA_2012_Summary_Code")]),
                    by.x = "BEA_389", by.y = "BEA_2012_Detail_Code")
  AgOutput <- stats::aggregate(AgOutput[, c("United States", state.name)],
                               by = list(AgOutput$BEA_2012_Summary_Code), sum, na.rm = TRUE)
  colnames(AgOutput)[1] <- "BEA_2012_Summary_Code"
  # Calculate state/US Ag output ratio
  AgOutput[, state.name] <- AgOutput[, state.name]/AgOutput[, "United States"]
  # Reshape table from wide to long
  AgOutput <- reshape2::melt(AgOutput[, c("BEA_2012_Summary_Code", state.name)], id.vars = "BEA_2012_Summary_Code")
  colnames(AgOutput) <- c("BEA_2012_Summary_Code", "GeoName", "OutputRatio")
  
  # Forestry & Fishery (113FF)
  # Load USDA-FS Forest Product Cut Values data by state (FY2012)
  ForestOutput <- utils::read.table(system.file("extdata", "ForestProdCutValueFY2012.csv", package = "useeior"),
                                    sep = ",", header = TRUE, check.names = FALSE, colClasses = c("character", "numeric"))
  # Load NOAA Fishery Landings data by state by year (2010-2016)
  FisheryOutput <- utils::read.table(system.file("extdata", "FisheryLandings2010-2016.csv", package = "useeior"),
                                     sep = ",", header = TRUE, check.names = FALSE, colClasses = c("numeric", "character", "numeric"))
  FisheryOutput <- FisheryOutput[FisheryOutput$Year==year, ]
  # Combine Forestry & Forestry
  ForestryandFishery <- merge(ForestOutput[ForestOutput$State%in%state.name, ],
                              FisheryOutput[FisheryOutput$State%in%state.name, ],
                              by = "State", all = TRUE)
  ForestryandFishery[is.na(ForestryandFishery)] <- 0
  ForestryandFishery$Output <- ForestryandFishery$CutValue + ForestryandFishery$LandingsValue
  # Calculate state/US Forestry & Forestry output ratio
  ForestryandFishery$OutputRatio <- ForestryandFishery$Output/sum(ForestryandFishery$Output)
  # Add BEA sector code
  ForestryandFishery <- cbind.data.frame("113FF", ForestryandFishery[, c("State", "OutputRatio")])
  colnames(ForestryandFishery) <- c("BEA_2012_Summary_Code", "GeoName", "OutputRatio")
  
  # Assemble Commodity Output from Agriculture, Forestry and Fishery
  StateCommodityOutputRatio <- rbind(AgOutput, ForestryandFishery)
  return(StateCommodityOutputRatio)
}

