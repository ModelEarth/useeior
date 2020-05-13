# Load BEA schema_info based on model BEA
SchemaInfo <- utils::read.table(system.file("extdata", "2012_Summary_Schema_Info.csv", package = "useeior"),
                                sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
# Extract desired columns from SchemaInfo, return vectors with strings of codes
getVectorOfCodes <- function(colName) {
  return(as.vector(stats::na.omit(SchemaInfo[, c("Code", colName)])[, "Code"]))
}
HouseholdDemandCodes <- getVectorOfCodes("HouseholdDemand")
InvestmentDemandCodes <- getVectorOfCodes("InvestmentDemand")
ImportCodes <- getVectorOfCodes("Import")
ExportCodes <- getVectorOfCodes("Export")
GovernmentDemandCodes <- getVectorOfCodes("GovernmentDemand")

#' Calculate state-US PCE (personal consumption expenditures) ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains ratios of state/US PCE for all states at a specific year at BEA Summary level.
calculateStateUSPCERatio <- function(year) {
  # Calculate PCE for Overseas
  StatePCE <- get("State_PCE_2007_2018")[, c("GeoName", "Line", as.character(year))]
  # Generate sum of state PCE
  StatePCE_sum <- stats::aggregate(StatePCE[, as.character(year)], by = list(StatePCE$Line), sum)
  colnames(StatePCE_sum) <- c("Line", as.character(year))
  # Load US PCE from state PCE table
  PCEtable <- utils::read.table("inst/extdata/SAEXP/SAEXP1__ALL_AREAS_1997_2018.csv", sep = ",",
                                header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE)
  USPCE <- PCEtable[PCEtable$GeoName=="United States", c("Line", as.character(year))]
  # Convert values to US $
  USPCE[, as.character(year)] <- USPCE[, as.character(year)]*1E6
  # Merge sum of state PCE with US PCE to get PCE of Overseas region
  OverseasPCE <- merge(StatePCE_sum, USPCE, by = "Line")
  OverseasPCE[, as.character(year)] <- OverseasPCE[, paste0(year, ".y")] - OverseasPCE[, paste0(year, ".x")]
  OverseasPCE$GeoName <- "Overseas"
  # Append Overseas PCE to StatePCE
  StatePCE <- rbind(StatePCE, OverseasPCE[, colnames(StatePCE)])
  # Merge State and US PCE by LineCode
  StateUSPCE <- merge(StatePCE, USPCE, by = "Line")
  # Calculate the state-US PCE ratios by LineCode
  StateUSPCE$Ratio <- StateUSPCE[, paste0(year, ".x")]/StateUSPCE[, paste0(year, ".y")]
  # Map to BEA Summary
  StatePCEtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StatePCEtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                            sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  StateUSPCE <- merge(StatePCEtoBEASummary[!StatePCEtoBEASummary$BEA_2012_Summary_Code=="", ],
                      StateUSPCE, by = "Line")
  # Adjust Ratio based on state PCE
  for (state in unique(StateUSPCE$GeoName)) {
    for (sector in unique(StateUSPCE$BEA_2012_Summary_Code)) {
      adjustedratio <- weighted.mean(StateUSPCE[StateUSPCE$GeoName==state&StateUSPCE$BEA_2012_Summary_Code==sector, "Ratio"],
                                     StateUSPCE[StateUSPCE$GeoName==state&StateUSPCE$BEA_2012_Summary_Code==sector, paste0(year, ".x")])
      StateUSPCE[StateUSPCE$GeoName==state&StateUSPCE$BEA_2012_Summary_Code==sector, "Ratio"] <- adjustedratio
    }
  }
  StateUSPCE$State <- StateUSPCE$GeoName
  # Keep wanted columns
  StateUSPCE <- unique(StateUSPCE[order(StateUSPCE$State, StateUSPCE$BEA_2012_Summary_Code),
                                  c("BEA_2012_Summary_Code", "State", "Ratio")])
  return(StateUSPCE)
}

