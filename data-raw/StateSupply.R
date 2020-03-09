#'Creates Combined Supply Table (in Make Table form) for all 50 states for a given year
#'Stores table by year .rda files in useeior package

#' 1 - Load in state total value added (VA) by industry for the given year. Map to BEA Summary level

state_VA <- useeior::State_GDP_2007_2018

#' 2 - Load in alternative state output sources. Where VA must be allocated from a LineCode to 
#' multiple industries, create allocation factors based on these alternative sources. In absense of 
#' an alternative source, allocate based on national industry output 

#' 3 - Create VA_state:VA_US ratios for each state for each summary industry.
#'  Multiply US output by this ratio to get initial state industry output estimates

#' 4 - Load in available state commodity output data for a given year. Use these data to estimate state
#' commodity output by creating ratios. Estimate initial state commodity output
 
#' 5 - Load US Summary Make table for given year

#' 6 - Create initial make table for each state. Use standard row/column naming.
#'  For each state and each industry,apply the state's industry output to divide each value in the state Make table 
#' along each row by this ratio. Verify the row sums of all tables combined equal national
#' Make rowsum.

#' 7 - Where state commodity output estimates are not given, assume national market shares to estimates
#' commodity output

#' 8 - Horizontally stack all state Make tables. Add a function to determine total commodity output (colsums)
 
#' 9 - Perform RAS until model is balanced

#' 10 - Save balanced table to .rda with use_data








