#'Creates Combined Supply Table (in Make Table form) for all 50 states for a given year
#'Stores table by year .rda files in useeior package

#' 1 - Load in state total value added (VA) by industry for the given year. Map to BEA Summary level
year <- 2014
state_VA <- getStateGDP(year)
state_VA_BEA <- mapStateTabletoBEASummary(state_VA)

#' 2 - Load in alternative state output sources. Where VA must be allocated from a LineCode to 
#' multiple industries, create allocation factors based on these alternative sources. In absense of 
#' an alternative source, allocate based on national industry output 
allocation_df <- calculateStatetoBEASummaryAllocationFactor(year, allocationweightsource="Employment")
StateValueAdded <- allocateStateTabletoBEASummary(year, "GDP", allocationweightsource="Employment")
StateUSVARatio <- generateStateUSValueAddedRatios(year)

#' 3 - Load in available state commodity output data for a given year. Use these data to estimate state
#' commodity output by creating ratios. Estimate initial state commodity output
State_CommodityOutput <- getStateCommodityOutputEstimates(year)

#' 4 - Load US Summary Make table for given year
US_Summary_Make <- get(paste("Summary_Make", year, "BeforeRedef", sep = "_"))

#' 5 - Create initial make table for each state. Use standard row/column naming.
#'  For each state and each industry,apply the state's industry output to divide each value in the state Make table 
#' along each row by this ratio. Verify the row sums of all tables combined equal national
#' Make rowsum.
state_US_VA_ratio <- generateStateUSValueAddedRatios(year)
State_Summary_Make_list <- list()
for (state in unique(State_IndustryOutput$GeoName)) {
  State_Summary_Make_list[[state]] <- sweep(US_Summary_Make, 1, state_US_VA_ratio[state_US_VA_ratio$GeoName==state, ], "*")
}

#' 6 - Where state commodity output estimates are not given, assume national market shares to estimates
#' commodity output
# Apply colSums to calculate Total Commodity Output (use ag Comm Output to replace the ag sector values)

#' 7 - Horizontally stack all state Make trascation tables. Add a function to determine total (all states) commodity output (colsums)

#' 8 - Perform RAS until model is balanced

#' 9 - Save balanced table to .rda with use_data








