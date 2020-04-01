#'Creates Combined Supply Table (in Make Table form) for all 50 states for a given year
#'Stores table by year .rda files in useeior package

#' 1 - Load state GDP/value added (VA) by industry for the given year.
#' Map to BEA Summary level
year <- 2014
state_VA <- getStateGDP(year)
state_VA_BEA <- mapStateTabletoBEASummary("GDP", year)

#' 2 - Where VA must be allocated from a LineCode toBEA Summary industries,
#' calculate allocation factors using specified allocationweightsource.
#' When using state employment (from BEA) as source for allocation,
#' introduce national gross output to disaggregate state employment in real estate and gov industries
#' from LineCode to BEA Summary.
#' 
#' Adjust state VA based on LineCode-to-BEA allocation factors.
AllocationFactors <- calculateStatetoBEASummaryAllocationFactor(year, allocationweightsource = "Employment")
StateValueAdded <- allocateStateTabletoBEASummary("GDP", year, allocationweightsource = "Employment")

#' 3 - Load in available state commodity output data for a given year. Use these data to estimate state
#' commodity output by creating ratios. Estimate initial state commodity output
State_CommodityOutput <- getStateCommodityOutputEstimates(year)

#' 4 - Load US Summary Make table for given year
US_Summary_Make <- get(paste("Summary_Make", year, "BeforeRedef", sep = "_"),
                       as.environment("package:useeior"))*1E6

#' 5 - Calculate state_US_VA_ratio, for each state and each industry, divide state VA by US VA.
#' Create initial Make table for each state. Use standard row/column naming.
#' For each state and each industry, multiply each value in the Make table row-wise
#' by state_US_VA_ratio.
#' Verify the row sums of all tables combined equal national Make rowsum.
#' If not, apply RAS balancing to adjust state_US_VA_ratio.
#' Create state commodity output by applying colSums to state Make table.

# Calculate state_US_VA_ratio
state_US_VA_ratio <- calculateStateUSValueAddedRatio(year)
# Extract US_Summary_MakeTrasaction
US_Summary_MakeTrasaction <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                             -which(colnames(US_Summary_Make)=="Total Industry Output")]
# Sum US_Summary_MakeTrasaction to get US_Summary_IndustryOutput
US_Summary_IndustryOutput <- rowSums(US_Summary_MakeTrasaction)
for (state in state.name) {
  # Subset the state_US_VA_ratio for specified state
  VA_ratio <- state_US_VA_ratio[state_US_VA_ratio$GeoName==state, ]
  # Replace NA with zero
  VA_ratio[is.na(VA_ratio$Ratio), "Ratio"] <- 0
  # Re-order state_US_VA_ratio
  rownames(VA_ratio) <- VA_ratio$BEA_2012_Summary_Code
  VA_ratio <- VA_ratio[rownames(US_Summary_MakeTrasaction), ]
  # Calculate State_Summary_IndustryOutput by multiplying US_Summary_IndustryOutput with VA_ratio
  State_Summary_IndustryOutput_list[[state]] <- US_Summary_IndustryOutput*VA_ratio$Ratio
}

#' Apply RAS balancing method to adjust VA_ratio of the disaggregated sectors (retail, real estate, gov)
m0_list <- list()
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
  m0_list[[linecode]] <- cbind.data.frame(rownames(m0), m0)
  # Apply RAS balancing
  m <- RAS(m0, t_r, t_c, t = 0.01, max_itr = 1000)
  m_list[[linecode]] <- cbind.data.frame(rownames(m), m)
  # Re-calculate state_US_VA_ratio for the disaggregated sectors
  state_US_VA_ratio_linecode <- m/rowSums(m)
  # Replace the ratio values in state_US_VA_ratio with the re-calculated ratio
  for (sector in rownames(state_US_VA_ratio_linecode)) {
    state_US_VA_ratio[state_US_VA_ratio$BEA_2012_Summary_Code==sector, "Ratio"] <- state_US_VA_ratio_linecode[sector, ]
  }
}
writexl::write_xlsx(m0_list, "m0_list.xlsx")
writexl::write_xlsx(m_list, "m_list.xlsx")


VA_ratio_list <- list()
State_Summary_MakeTrasaction_list <- list()
State_Summary_IndustryOutput_list <- list()
State_Summary_CommodityOutput_list <- list()
for (state in unique(state_US_VA_ratio$GeoName)) {
  # Subset the state_US_VA_ratio for specified state
  VA_ratio <- state_US_VA_ratio[state_US_VA_ratio$GeoName==state, ]
  # Replace NA with zero
  VA_ratio[is.na(VA_ratio$Ratio), "Ratio"] <- 0
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



sectors <- rownames(VA_ratio_sum[VA_ratio_sum[, 1]<1, , drop = FALSE])
StateValueAdded_sum <- stats::aggregate(StateTableBEA[, as.character(year)],
                                        by = list(StateTableBEA$BEA_2012_Summary_Code),
                                        sum)
colnames(StateValueAdded_sum) <- c("BEA_2012_Summary_Code", "StateVA_sum")
State_US_VA <- merge(StateValueAdded_sum[StateValueAdded_sum$BEA_2012_Summary_Code%in%sectors, ],
                     USValueAdded[sectors, , drop = FALSE],
                     by.x = "BEA_2012_Summary_Code", by.y = 0)
State_US_VA$diff <- State_US_VA$StateVA_sum - State_US_VA[, as.character(year)]
State_US_VA$diff_pct <- State_US_VA$diff/State_US_VA[, as.character(year)]


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
  # Use state/US ratio * US Summary Comm Output to replace
}

#' 7 - Horizontally and vertically (diagolized) stack all state Make trascation tables.
#' Add a function to determine total (all states) commodity output (colsums)

#' 8 - Perform RAS until model is balanced
#' Apply RAS balancing to the entire Make table


#' 9 - Save balanced table to .rda with use_data








