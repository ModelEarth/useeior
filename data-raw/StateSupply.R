#'Creates Combined Supply Table (in Make Table form) for all 50 states for a given year
#'Stores table by year .rda files in useeior package

#' 1 - Load in state total value added (VA) by industry for the given year. Map to BEA Summary level
year <- 2013
state_VA <- getStateGDP(year)
state_VA_BEA <- mapStateTabletoBEASummary("GDP", year)

#' 2 - Load in alternative state output sources. Where VA must be allocated from a LineCode to 
#' multiple industries, create allocation factors based on these alternative sources. In absense of 
#' an alternative source, allocate based on national industry output 
AllocationFactors <- calculateStatetoBEASummaryAllocationFactor(year, allocationweightsource = "Employment")
StateValueAdded <- allocateStateTabletoBEASummary("GDP", year, allocationweightsource = "Employment")

#' 3 - Load in available state commodity output data for a given year. Use these data to estimate state
#' commodity output by creating ratios. Estimate initial state commodity output
State_CommodityOutput <- getStateCommodityOutputEstimates(year)

#' 4 - Load US Summary Make table for given year
US_Summary_Make <- get(paste("Summary_Make", year, "BeforeRedef", sep = "_"),
                       as.environment("package:useeior"))*1E6

#' 5 - Create initial make table for each state. Use standard row/column naming.
#'  For each state and each industry,apply the state's industry output to divide each value in the state Make table 
#' along each row by this ratio. Verify the row sums of all tables combined equal national
#' Make rowsum.
state_US_VA_ratio <- calculateStateUSValueAddedRatio(year)
VA_ratio_list <- list()
State_Summary_MakeTrasaction_list <- list()
State_Summary_IndustryOutput_list <- list()
State_Summary_CommodityOutput_list <- list()
for (state in unique(state_US_VA_ratio$GeoName)) {
  # Subset the state_US_VA_ratio for specified state
  VA_ratio <- state_US_VA_ratio[state_US_VA_ratio$GeoName==state, ]
  # Replace NA with zero
  VA_ratio[is.na(VA_ratio$Ratio), "Ratio"] <- 0
  # Extract US_Summary_MakeTrasaction
  US_Summary_MakeTrasaction <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                               -which(colnames(US_Summary_Make)=="Total Industry Output")]
  # Extract US_Summary_IndustryOutput
  US_Summary_IndustryOutput <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                               "Total Industry Output", drop = FALSE]
  # Re-order state_US_VA_ratio
  rownames(VA_ratio) <- VA_ratio$BEA_2012_Summary_Code
  VA_ratio <- VA_ratio[rownames(US_Summary_MakeTrasaction), ]
  VA_ratio_list[[state]] <- VA_ratio[, "Ratio", drop = FALSE]
  # Calculate State_Summary_MakeTrasaction by multiplying US_Summary_MakeTrasaction with VA_ratio
  State_Summary_MakeTrasaction_list[[state]] <- sweep(US_Summary_MakeTrasaction, 1, VA_ratio$Ratio, "*")
  # Calculate State_Summary_IndustryOutput by multiplying US_Summary_IndustryOutput with VA_ratio
  State_Summary_IndustryOutput_list[[state]] <- US_Summary_IndustryOutput*VA_ratio$Ratio
  # Calculate State_Summary_CommodityOutput by colSumming State_Summary_MakeTrasaction
  State_Summary_CommodityOutput_list[[state]] <- as.data.frame(colSums(State_Summary_MakeTrasaction_list[[state]]))
}
# Verify if row sums of all state Make tables equal to national Make rowsums
VA_raio_all <- do.call(rbind.data.frame, VA_ratio_list)
VA_ratio_sum <- as.data.frame(round(rowSums(do.call(cbind.data.frame, lapply(VA_ratio_list, rowSums))), 2))
IndustryOutput_StateSum <- as.data.frame(round(rowSums(do.call(cbind.data.frame, lapply(State_Summary_MakeTrasaction_list, rowSums))), 0))
IndustryOutput_US <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                     "Total Industry Output", drop = FALSE]
IndustryOutput_diff <- IndustryOutput_StateSum - IndustryOutput_US
colnames(IndustryOutput_diff) <- "StateSum - US"

#' 6 - Where state commodity output estimates are not given, assume national market shares to estimates
#' commodity output
# Apply colSums to calculate Total Commodity Output (use ag Comm Output to replace the ag sector values)

# Estimate AgOutput (BEA Detail, state)
AgOutput <- getStateCommodityOutputEstimates(year)
for (state in unique(state_US_VA_ratio$GeoName)) {
  # Map to BEA Summary then aggregate
  AgOutput <- merge(AgOutput[AgOutput$GeoName==state, ],
                    unique(useeior::MasterCrosswalk2012[, c("BEA_2012_Detail_Code", "BEA_2012_Summary_Code")]),
                    by = "BEA_2012_Detail_Code")
  AgOutput <- stats::aggregate(AgOutput$Output, by = list(AgOutput$BEA_2012_Summary_Code), sum, na.rm = TRUE)
  colnames(AgOutput) <- c("BEA_2012_Summary_Code", "V1")
  rownames(AgOutput) <- AgOutput$BEA_2012_Summary_Code
  AgOutput$BEA_2012_Summary_Code <- NULL
  # Replace the Ag sectors in State_Summary_CommodityOutput_list
  State_Summary_CommodityOutput_list[[state]][rownames(AgOutput), ] <- AgOutput
}

#' 7 - Horizontally stack all state Make trascation tables. Add a function to determine total (all states) commodity output (colsums)

#' 8 - Perform RAS until model is balanced
m_list <- list()
for (linecode in c("35", "57", "84", "86")) {
  # Generate industry output by LineCode by State, a vector/df of 1x50,
  # calculated by state_US_VA_ratio_LineCode * sum(US industry output Linecode sectors)
  StateIndustryOutputbyLineCode <- calculateStateIndustryOutputbyLineCode(year)
  t_c <- StateIndustryOutputbyLineCode[StateIndustryOutputbyLineCode$LineCode==linecode,
                                       as.character(year)]
  
  # Generate another vector of US industry output for the LineCode by BEA Summary
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Determine BEA sectors that need allocation
  allocation_sectors <- BEAStateGDPtoBEASummary[duplicated(BEAStateGDPtoBEASummary$LineCode)| duplicated(BEAStateGDPtoBEASummary$LineCode, fromLast = TRUE), ]
  BEA_sectors <- allocation_sectors[allocation_sectors$LineCode==linecode, "BEA_2012_Summary_Code"]
  t_r <- US_Summary_Make[BEA_sectors, "Total Industry Output"]
  
  
  # Create m0
  EstimatedStateIndustryOutput <- do.call(cbind, State_Summary_IndustryOutput_list)
  colnames(EstimatedStateIndustryOutput) <- names(State_Summary_IndustryOutput_list)
  m0 <- as.matrix(EstimatedStateIndustryOutput[BEA_sectors, ])
  
  # Apply RAS balancing
  m_list[[linecode]] <- RAS(m0, t_r, t_c, t=0.01, max_itr = 1000)
}

#' 9 - Save balanced table to .rda with use_data








