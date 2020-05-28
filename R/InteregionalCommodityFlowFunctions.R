#' Calculate domestic local and traded ratio, which will be used in the next function generateDomestic2RegionICFs
#' @param state State name.
#' @param year A numeric value between 2012 and 2017 specifying the year of interest.
#' @param SoI A boolean variable indicating whether to calculate local and traded ratios for SoI or RoUS.
#' @param ioschema A numeric value of either 2012 or 2007 specifying the io schema year.
#' @param iolevel BEA sector level of detail, can be "Detail", "Summary", or "Sector".
#' @return A data frame contains local and traded ratios by BEA sectors for the specified state.
calculateLocalandTradedRatios <- function (state, year, SoI = TRUE, ioschema, iolevel) {
  # Specify BEA code
  bea_code <- paste("BEA", ioschema, iolevel, "Code", sep = "_")
  # Load the cluster mapping for NAICS to Traded/Local (from clustermapping.us)
  NAICStoTradedorLocal <- utils::read.table(system.file("extdata", "Crosswalk_ClusterMappingNAICStoTradedorLocal.csv", package = "useeior"),
                                            sep = ",", header = TRUE, check.names = FALSE)
  # Map NAICStoTradedorLocal to BEA
  BEAtoTradedorLocal <- merge(unique(useeior::MasterCrosswalk2012[, c("NAICS_2012_Code", bea_code)]),
                              NAICStoTradedorLocal, by.x = "NAICS_2012_Code", by.y = "NAICS")
  # Load pre-saved state commodity output data
  filename <- load(paste0("data/State_", iolevel, "_CommodityOutput_", year, ".rda"))
  StateCommOutput <- get(filename)[[state]]
  colnames(StateCommOutput) <- "CommodityOutput"
  # Merge with BEAtoTradedorLocal
  StateCommOutput <- merge(unique(BEAtoTradedorLocal[, c(bea_code, "Type")]),
                           StateCommOutput, by.x = bea_code, by.y = 0)
  # Transform table from long to wide
  StateCommOutput <- reshape2::dcast(StateCommOutput, paste(bea_code, "~ Type"), value.var = "CommodityOutput")
  StateCommOutput[is.na(StateCommOutput)] <- 0
  # Calculate local and traded ratios
  StateCommOutput$LocalRatio <- StateCommOutput$Local/(StateCommOutput$Local+StateCommOutput$Traded)
  StateCommOutput$TradedRatio <- StateCommOutput$Traded/(StateCommOutput$Local+StateCommOutput$Traded)
  LocalandTradedRatiosbyBEA <- StateCommOutput[, c(bea_code, "LocalRatio", "TradedRatio")]
  return(LocalandTradedRatiosbyBEA)
}

