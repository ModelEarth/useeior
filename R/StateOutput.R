#' Estimate state industry output
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe contains state industry output for specified state with row names being BEA sector code.
getBEAStateGDP <- function(model) {
  # Load State GDP to BEA Summary sector-mapping table
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "BEAStateGDPtoBEASummary.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Merge State_GDP with BEA Summary
  StateGDP <- merge(useeior::State_GDP_2007_2018, BEAStateGDPtoBEASummary, by = "LineCode")
  # Keep rows for SoI and columns of BEA Summary code
  StateGDP <- StateGDP[StateGDP$GeoName==state.name[match(model$specs$PrimaryRegionAcronym, state.abb)],
                       c("BEA_2012_Summary_Code", as.character(model$specs$IOYear))]
  # Merge with US GDP
  StateGDP <- merge(StateGDP, model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE],
                    by.x = "BEA_2012_Summary_Code", by.y = 0)
  # Rename columns
  colnames(StateGDP) <- c("SectorCode", "SoI", "US")
  # Aggregate from Summary to Sector if model is Sector-level
  if (model$BaseIOLevel == "Sector") {
    # aggregate from BEA Summary to Sector level
    StateGDP <- merge(StateGDP, unique(useeior::MasterCrosswalk2012[, c("BEA_2012_Sector_Code", "BEA_2012_Summary_Code")]),
                      by.x = "SectorCode", by.y = "BEA_2012_Summary_Code")
    StateGDP <- stats::aggregate(StateGDP[, c("SoI", "US")], by = list(StateGDP$BEA_2012_Sector_Code), sum)
    colnames(StateGDP)[1] <- "SectorCode"
  }
  return(StateGDP)
}
