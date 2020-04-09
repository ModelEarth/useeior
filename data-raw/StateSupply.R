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
#' commodity output by creating ratios.
StateCommodityOutputRatio <- getStateCommodityOutputRatioEstimates(year)

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
State_Summary_IndustryOutput_list <- list()
for (state in c(unique(state_US_VA_ratio$GeoName), "Overseas")) {
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
for (linecode in c("35", "57", "84", "86")) {
  # Generate industry output by LineCode by State, a vector/df of 1x50,
  # calculated by state_US_VA_ratio_LineCode * sum(US industry output Linecode sectors)
  StateIndustryOutputbyLineCode <- calculateStateIndustryOutputbyLineCode(year)
  t_c <- StateIndustryOutputbyLineCode[StateIndustryOutputbyLineCode$LineCode==linecode, as.character(year)]
  # Generate another vector of US industry output for the LineCode by BEA Summary
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Determine BEA sectors that need allocation
  allocation_sectors <- BEAStateGDPtoBEASummary[duplicated(BEAStateGDPtoBEASummary$LineCode) |
                                                  duplicated(BEAStateGDPtoBEASummary$LineCode, fromLast = TRUE), ]
  BEA_sectors <- allocation_sectors[allocation_sectors$LineCode==linecode, "BEA_2012_Summary_Code"]
  t_r <- as.data.frame(US_Summary_IndustryOutput)[BEA_sectors, ]
  # Adjust t_c/t_r, make sum(t_c)==sum(t_r)
  if (sum(t_c) > sum(t_r)) {
    t_r <- (t_r/sum(t_r))*sum(t_c)
  } else {
    t_c <- (t_c/sum(t_c))*sum(t_r)
  }
  # Create m0
  EstimatedStateIndustryOutput <- do.call(cbind, State_Summary_IndustryOutput_list)
  colnames(EstimatedStateIndustryOutput) <- names(State_Summary_IndustryOutput_list)
  m0 <- as.matrix(EstimatedStateIndustryOutput[BEA_sectors, ])
  # Apply RAS balancing
  if (linecode=="35") {
    t <- ToleranceforRAS(t_r, t_c, NULL, 0)
  } else if (linecode %in% c("57", "84")) {
    t <- ToleranceforRAS(t_r, t_c, NULL, 1)
  } else {
    t <- ToleranceforRAS(t_r, t_c, 3.5e-4, NULL)
  }
  m <- RAS(m0, t_r, t_c, t, max_itr = 1E6)
  # Re-calculate state_US_VA_ratio for the disaggregated sectors
  state_US_VA_ratio_linecode <- m/rowSums(m)
  # Replace the ratio values in state_US_VA_ratio with the re-calculated ratio
  for (sector in rownames(state_US_VA_ratio_linecode)) {
    state_US_VA_ratio[state_US_VA_ratio$BEA_2012_Summary_Code==sector, "Ratio"] <- state_US_VA_ratio_linecode[sector, ]
  }
}

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
  # Calculate State_Summary_MakeTrasaction by multiplying US_Summary_MakeTrasaction with VA_ratio
  State_Summary_MakeTrasaction_list[[state]] <- as.matrix(sweep(US_Summary_MakeTrasaction, 1, VA_ratio$Ratio, "*"))
  # Calculate State_Summary_IndustryOutput by multiplying US_Summary_IndustryOutput with VA_ratio
  State_Summary_IndustryOutput_list[[state]] <- US_Summary_IndustryOutput*VA_ratio$Ratio
  # Calculate State_Summary_CommodityOutput by colSumming State_Summary_MakeTrasaction
  State_Summary_CommodityOutput_list[[state]] <- as.data.frame(colSums(State_Summary_MakeTrasaction_list[[state]]))
}


#' 6 - Where state commodity output estimates are not given, assume national market shares to estimates
#' commodity output
# Apply colSums to calculate Total Commodity Output (use ag Comm Output to replace the ag sector values)
for (state in unique(state_US_VA_ratio$GeoName)) {
  # Calculate state/US commodit output ratio * US Summary Comm Output
  AdjustedCommodityOutput <- merge(colSums(US_Summary_MakeTrasaction),
                                   StateCommodityOutputRatio[StateCommodityOutputRatio$GeoName==state, ],
                                   by.x = 0, by.y = "BEA_2012_Summary_Code")
  AdjustedCommodityOutput$Output <- AdjustedCommodityOutput$x*AdjustedCommodityOutput$OutputRatio
  # Replace commodity output value in State_Summary_CommodityOutput_list
  industries <- AdjustedCommodityOutput$Row.names
  State_Summary_CommodityOutput_list[[state]][industries, ] <- AdjustedCommodityOutput[AdjustedCommodityOutput$Row.names%in%industries, "Output"]
}

#' 7 - Diagonally stack all state Make trascation tables.
#' Add a function to determine total (all states) commodity output (colsums)
State_Summary_MakeTrasaction <- as.matrix(Matrix::.bdiag(State_Summary_MakeTrasaction_list))
rownames(State_Summary_MakeTrasaction) <- paste(rep(names(State_Summary_MakeTrasaction_list),
                                                    each = nrow(State_Summary_MakeTrasaction_list[[1]])),
                                                rep(rownames(State_Summary_MakeTrasaction_list[[1]]),
                                                    time = length(names(State_Summary_MakeTrasaction_list))))
colnames(State_Summary_MakeTrasaction) <- paste(rep(names(State_Summary_MakeTrasaction_list),
                                                    each = ncol(State_Summary_MakeTrasaction_list[[1]])),
                                                rep(colnames(State_Summary_MakeTrasaction_list[[1]]),
                                                    time = length(names(State_Summary_MakeTrasaction_list))))

#' 8 - Perform RAS until model is balanced
#' Apply RAS balancing to the entire Make table


#' 9 - Save balanced table to .rda with use_data








