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
  if (SoI == FALSE) {
    US_Make <- get(paste(iolevel, "Make", year, "BeforeRedef", sep = "_"))*1E6
    USCommOutput <- colSums(US_Make[-which(rownames(US_Make)=="Total Commodity Output"),
                                    -which(colnames(US_Make)=="Total Industry Output")])
    StateCommOutput <- merge(StateCommOutput, USCommOutput, by.x = bea_code, by.y = 0)
    StateCommOutput$CommodityOutput <- StateCommOutput$y - StateCommOutput$CommodityOutput
  }
  # Transform table from long to wide
  LocalorTraded <- reshape2::dcast(StateCommOutput, paste(bea_code, "~ Type"), value.var = "CommodityOutput")
  LocalorTraded[is.na(LocalorTraded)] <- 0
  # Calculate local and traded ratios
  LocalorTraded$LocalRatio <- LocalorTraded$Local/(LocalorTraded$Local + LocalorTraded$Traded)
  LocalorTraded$TradedRatio <- LocalorTraded$Traded/(LocalorTraded$Local + LocalorTraded$Traded)  
  LocalandTradedRatiosbyBEA <- LocalorTraded[, c(bea_code, "LocalRatio", "TradedRatio")]
  return(LocalandTradedRatiosbyBEA)
}

#' Generate domestic 2 region inter-regional commodity flows (ICFs) table.
#' @param state State name.
#' @param year A numeric value between 2012 and 2017 specifying the year of interest.
#' @param remove_scrap A boolean variable indicating whether to remove scrap from the table.
#' @param ioschema A numeric value of either 2012 or 2007 specifying the io schema year.
#' @param iolevel BEA sector level of detail, can be "Detail", "Summary", or "Sector".
#' @return A data frame contains domestic 2 region ICFs.
generateDomestic2RegionICFs <- function (state, year, remove_scrap = FALSE, ioschema, iolevel) {
  # Specify BEA code
  bea_code <- paste("BEA", ioschema, iolevel, "Code", sep = "_")
  # Generate SoI-RoUS commodity flow ratios
  ICF_2r <- calculateCommodityFlowRatios(state, year, "domestic", ioschema, iolevel)
  ICF_2r$flowpath <- paste0(ICF_2r$ORIG, "2", ICF_2r$DEST)
  ICF_2r_wide <- reshape2::dcast(ICF_2r[, c(bea_code, "ratio", "flowpath")],
                                 paste(bea_code, "~", "flowpath"), value.var = "ratio")
  
  # Calculate interregional commodity flow (ICF) ratios for all commodities
  # Merge ICF_2r_wide with complete BEA Commodity list
  ICF_comm <- merge(ICF_2r_wide, get(paste(iolevel, "CommodityCodeName_2012", sep = "_")),
                    by.x = bea_code,
                    by.y = paste("BEA", ioschema, iolevel, "Commodity_Code", sep = "_"),
                    all.y = TRUE)
  # Calculate SoI local and traded ratios
  localandtratdedratioSoI <- calculateLocalandTradedRatios(state, year, SoI = TRUE, ioschema, iolevel)
  # Calculate RoUS local and traded ratios
  localandtratdedratioRoUS <- calculateLocalandTradedRatios(state, year, SoI = FALSE, ioschema, iolevel)
  # Load SoI Commodity Output
  filename <- load(paste0("data/State_", iolevel, "_CommodityOutput_", year, ".rda"))
  StateCommOutput <- get(filename)[[state]]
  US_Make <- get(paste(iolevel, "Make", year, "BeforeRedef", sep = "_"))*1E6
  USCommOutput <- colSums(US_Make[-which(rownames(US_Make)=="Total Commodity Output"),
                                  -which(colnames(US_Make)=="Total Industry Output")])
  StateCommOutput$SoI_US_ratio <- StateCommOutput[, 1]/USCommOutput
  # Calculate local and traded ratios
  for (BEAcode in Reduce(intersect, list(ICF_comm[is.na(ICF_comm$SoI2SoI), bea_code],
                                         localandtratdedratioSoI[, bea_code],
                                         localandtratdedratioRoUS[, bea_code]))) {
    # Assign data source
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "source"] <- "State Supply model"
    # RoUS2RoUS = LocalRoUS + OR*TradedRoUS
    LocalRoUS <- localandtratdedratioRoUS[localandtratdedratioRoUS[, bea_code]==BEAcode, "LocalRatio"]
    OR <- StateCommOutput[BEAcode, "SoI_US_ratio"]
    TradedRoUS <- localandtratdedratioRoUS[localandtratdedratioRoUS[, bea_code]==BEAcode, "TradedRatio"]
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "RoUS2RoUS"] <- LocalRoUS + OR*TradedRoUS
    # SoI2SoI = SoI_Local
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "SoI2SoI"] <- localandtratdedratioSoI[localandtratdedratioSoI[, bea_code]==BEAcode, "LocalRatio"]
    # RoUS2SoI = 1 - SoI2SoI
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "RoUS2SoI"] <- 1 - ICF_comm[ICF_comm[, bea_code]==BEAcode, "SoI2SoI"]
    # SoI2RoUS = 1 - RoUS2RoUS
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "SoI2RoUS"] <- 1 - ICF_comm[ICF_comm[, bea_code]==BEAcode, "RoUS2RoUS"]
  }
  # Use SoI and RoUS commodity output
  for (BEAcode in intersect(ICF_comm[is.na(ICF_comm$source), bea_code], rownames(StateCommOutput))) {
    # Assign data source
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "source"] <- "State Commodity Output"
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "RoUS2RoUS"] <- 1 - StateCommOutput[BEAcode, "SoI_US_ratio"]
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "SoI2SoI"] <- StateCommOutput[BEAcode, "SoI_US_ratio"]
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "RoUS2SoI"] <- 1 - StateCommOutput[BEAcode, "SoI_US_ratio"]
    ICF_comm[ICF_comm[, bea_code]==BEAcode, "SoI2RoUS"] <- StateCommOutput[BEAcode, "SoI_US_ratio"]
  }
  
  ICF_comm[is.na(ICF_comm)] <- 0
  #note this is not applicable for now because SOO401 is not present
  if (remove_scrap) {
    ICF_comm <- ICF_comm[-which(ICF_comm[, bea_code]=="S00401"), ]
  }
  return(ICF_comm)
}
