# Load BEA schema_info based on model BEA
SchemaInfo <- utils::read.table(system.file("extdata", "2012_Summary_Schema_Info.csv", package = "useeior"),
                                sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
# Extract desired columns from SchemaInfo, return vectors with strings of codes
getVectorOfCodes <- function(colName) {
  return(as.vector(stats::na.omit(SchemaInfo[, c("Code", colName)])[, "Code"]))
}
Commodities <- getVectorOfCodes("Commodity")
Industries <- getVectorOfCodes("Industry")
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
  # Replace NaN with zero
  StateUSPCE[is.na(StateUSPCE$Ratio), "Ratio"] <- 0
  StateUSPCE$State <- StateUSPCE$GeoName
  # Keep wanted columns
  StateUSPCE <- unique(StateUSPCE[order(StateUSPCE$State, StateUSPCE$BEA_2012_Summary_Code),
                                  c("BEA_2012_Summary_Code", "State", "Ratio")])
  return(StateUSPCE)
}

#' Estimate state household demand at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state household demand for all states at a specific year at BEA Summary level.
estimateStateHouseholdDemand <- function(year) {
  US_Summary_Use <- get(paste("Summary_Use", year, "PRO_BeforeRedef", sep = "_"), as.environment("package:useeior"))
  US_HouseholdDemand <- US_Summary_Use[Commodities, HouseholdDemandCodes, drop = FALSE]
  PCE_ratio <- calculateStateUSPCERatio(year)
  State_HouseholdDemand <- data.frame()
  for (state in unique(PCE_ratio$State)) {
    HouseholdDemand <- US_HouseholdDemand * PCE_ratio[PCE_ratio$State==state, "Ratio"]
    rownames(HouseholdDemand) <- paste(rownames(HouseholdDemand), state, sep = ".")
    State_HouseholdDemand <- rbind.data.frame(State_HouseholdDemand, HouseholdDemand)
  }
  return(State_HouseholdDemand)
}


#' Estimate state government demand at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @param gov Type of government, can be "federal" or "state and local".
#' @return A data frame contains state government demand for all states at a specific year at BEA Summary level.
estimateStateGovDemand <- function(year, gov) {
  
}

#' Estimate state-US employee compensation ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state-US employment compensation ratios for all states at a specific year at BEA Summary level.
calculateStateUSEmpCompensationRatio <- function(year) {
  # Generate state Employee Compensation table
  StateEmpCompensation <- allocateStateTabletoBEASummary("EmpCompensation", year, "Employment")
  # Separate into state Employee Compensation
  StateEmpCompensation <- StateEmpCompensation[StateEmpCompensation$GeoName!="United States *", ]
  # Map US Employee Compensation to BEA
  USEmpCompensation <- getStateEmpCompensation(year)
  USEmpCompensation <- USEmpCompensation[USEmpCompensation$GeoName=="United States *", ]
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  allocation_sectors <- BEAStateGDPtoBEASummary[duplicated(BEAStateGDPtoBEASummary$LineCode) | duplicated(BEAStateGDPtoBEASummary$LineCode, fromLast = TRUE), ]
  USEmpCompensation <- merge(USEmpCompensation, BEAStateGDPtoBEASummary, by = "LineCode")
  USEmpCompensation <- merge(USEmpCompensation, useeior::Summary_GrossOutput_IO[, as.character(year), drop = FALSE],
                             by.x = "BEA_2012_Summary_Code", by.y = 0)
  USEmpCompensation[, as.character(year)] <- USEmpCompensation[, paste0(year, ".x")]
  for (linecode in unique(allocation_sectors$LineCode)) {
    value_vector <- USEmpCompensation[USEmpCompensation$LineCode==linecode, paste0(year, ".x")]
    weight_vector <- USEmpCompensation[USEmpCompensation$LineCode==linecode, paste0(year, ".y")]
    USEmpCompensation[USEmpCompensation$LineCode==linecode, as.character(year)] <- value_vector*(weight_vector/sum(weight_vector))
  }
  USEmpCompensation <- USEmpCompensation[, c("BEA_2012_Summary_Code", as.character(year))]
  # Generate sum of state Employee Compensation
  StateEmpCompensation_sum <- stats::aggregate(StateEmpCompensation[, as.character(year)],
                                               by = list(StateEmpCompensation$BEA_2012_Summary_Code),
                                               sum, na.rm = TRUE)
  colnames(StateEmpCompensation_sum) <- c("BEA_2012_Summary_Code", "StateEmpCompensation_sum")
  # Merge sum of state Employee Compensation with US employee Compensation to get employee Compensation of Overseas region
  OverseasEmpCompensation <- merge(StateEmpCompensation_sum, USEmpCompensation,
                                   by = "BEA_2012_Summary_Code", all.x = TRUE)
  OverseasEmpCompensation[is.na(OverseasEmpCompensation)] <- 0
  OverseasEmpCompensation[, paste0(year, ".x")] <- OverseasEmpCompensation[, as.character(year)] - OverseasEmpCompensation$StateEmpCompensation_sum
  OverseasEmpCompensation[, paste0(year, ".y")] <- OverseasEmpCompensation[, as.character(year)]
  OverseasEmpCompensation$GeoName <- "Overseas"
  # Merge state GDP and US Employee Compensation tables
  StateUSEmpCompensation <- merge(StateEmpCompensation, USEmpCompensation, by = "BEA_2012_Summary_Code")
  # Append Overseas EmpCompensation to StateUSEmpCompensation
  StateUSEmpCompensation <- rbind(StateUSEmpCompensation, OverseasEmpCompensation[, colnames(StateUSEmpCompensation)])
  # Calculate the state-US Employee Compensation ratios
  StateUSEmpCompensation$Ratio <- StateUSEmpCompensation[, paste0(year, ".x")]/StateUSEmpCompensation[, paste0(year, ".y")]
  StateUSEmpCompensation <- StateUSEmpCompensation[order(StateUSEmpCompensation$GeoName, StateUSEmpCompensation$BEA_2012_Summary_Code),
                                         c("BEA_2012_Summary_Code", "GeoName", "Ratio")]
  rownames(StateUSEmpCompensation) <- NULL
  return(StateUSEmpCompensation)
}

#' Estimate weighting factor of each expenditure component over US total gov expenditure.
#' @param year A numeric value between 2007 and 2019 specifying the year of interest.
#' @param defense A boolean value indicating if the expenditure is spent on defense or not.
#' @return A data frame contains weighting factor of each expenditure component over US total gov expenditure.
calculateUSGovExpenditureWeightFactor <- function(year, defense) {
  # Load data
  GovInvestment <- useeior::GovInvestment_2007_2019
  GovConsumption <- useeior::GovConsumption_2007_2019
  # Keep rows by line code
  if (defense) {
    GovConsumption <- GovConsumption[GovConsumption$Line%in%c(26:28), ]
    GovInvestment <- GovInvestment[GovInvestment$Line%in%c(20:21, 23:24), ]
  } else {
    GovConsumption <- GovConsumption[GovConsumption$Line%in%c(37:39), ]
    GovInvestment <- GovInvestment[GovInvestment$Line%in%c(28:29, 31:32), ]
  }
  # Calculate weight factors and stack dfs together
  WeightFactor <- rbind(cbind(GovConsumption[, c("Line", "Description")],
                              GovConsumption[, as.character(year), drop = FALSE]/colSums(GovConsumption[, as.character(year), drop = FALSE])),
                        cbind(GovInvestment[, c("Line", "Description")],
                              GovInvestment[, as.character(year), drop = FALSE]/colSums(GovInvestment[, as.character(year), drop = FALSE])))
  # Modify Description
  WeightFactor$Description <- gsub("\\\\.*", "", WeightFactor$Description)
  return(WeightFactor)
}
