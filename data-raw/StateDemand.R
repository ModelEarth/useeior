#'Creates Combined Demand Table (in Use Table form) for all 52 states (including DC and Overseas) for a given year
#'Stores table by year .rda files in useeior package

#' 1 - Load state industry output for the given year.
year <- 2012
load(paste0("data/State_Summary_IndustryOutput_", year, ".rda"))
states <- names(State_Summary_IndustryOutput_list)

#' 2 - Load US Summary Make table for given year
#' Generate US Summary Industry Output
US_Summary_Make <- get(paste("Summary_Make", year, "BeforeRedef", sep = "_"))*1E6
US_Summary_MakeTransaction <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                              -which(colnames(US_Summary_Make)=="Total Industry Output")]
US_Summary_IndustryOutput <- rowSums(US_Summary_MakeTransaction)

#' 3 - Load US Summary Use table for given year
#' Generate US Summary Use Transaction
US_Summary_Use <- get(paste("Summary_Use", year, "PRO_BeforeRedef", sep = "_"))*1E6
US_Summary_UseTransaction <- US_Summary_Use[colnames(US_Summary_MakeTransaction),
                                            rownames(US_Summary_MakeTransaction)]

#' 4 - Calculate state_US_IndustryOutput_ratio, for each state and each industry,
#' Divide state IndustryOutput by US IndustryOutput.
State_Summary_UseTransaction_list <- list()
for (state in states) {
  IndustryOutputRatio <- State_Summary_IndustryOutput_list[[state]]/US_Summary_IndustryOutput
  State_Summary_UseTransaction_list[[state]] <- as.matrix(US_Summary_UseTransaction) %*% diag(IndustryOutputRatio)
  colnames(State_Summary_UseTransaction_list[[state]]) <- colnames(US_Summary_UseTransaction)
}

#' 5 - Vertically stack all state Use trascation tables.
State_Summary_UseTransaction <- do.call(rbind, State_Summary_UseTransaction_list)
rownames(State_Summary_UseTransaction) <- paste(rep(names(State_Summary_UseTransaction_list),
                                                    each = nrow(State_Summary_UseTransaction_list[[1]])),
                                                rep(rownames(State_Summary_UseTransaction_list[[1]]),
                                                    time = length(names(State_Summary_UseTransaction_list))),
                                                sep = ".")
colnames(State_Summary_UseTransaction) <- colnames(US_Summary_UseTransaction)

#' 6 - Validate if state totals == national total
# Row sum
rowSums(State_Summary_UseTransaction) - rowSums(US_Summary_UseTransaction)
# Column sum
State_CommInputTotal_list <- list()
for (industry in colnames(US_Summary_UseTransaction)) {
  State_CommInputTotal_list[[industry]] <- sum(State_Summary_UseTransaction[, paste(states, industry, sep = ".")])
}
unlist(State_CommInputTotal_list) - colSums(US_Summary_UseTransaction)

#' 7 - For each state, append Value Added to the end of Use table
StateVA <- allocateStateTabletoBEASummary("GDP", year, allocationweightsource = "Employment")
StateVA <- reshape2::dcast(StateVA, GeoName ~ BEA_2012_Summary_Code, value.var = as.character(year))
StateVA[is.na(StateVA)] <- 0
rownames(StateVA) <- StateVA$GeoName
StateVA$GeoName <- NULL
USValueAdded <- useeior::Summary_ValueAdded_IO[, as.character(year), drop = FALSE]
StateVA["Overseas", ] <- USValueAdded[, as.character(year)] - colSums(StateVA[rownames(StateVA)!="United States *", ], na.rm = TRUE)
StateVA <- StateVA[rownames(StateVA)!="United States *", ]
rownames(StateVA) <- paste(rownames(StateVA), "ValueAdded", sep = ".")
State_Summary_Use <- rbind(State_Summary_UseTransaction, StateVA)
State_Summary_Use <- State_Summary_Use[order(rownames(State_Summary_Use)), ]
