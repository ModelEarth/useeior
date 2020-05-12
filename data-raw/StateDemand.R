#'Creates Combined Demand Table (in Use Table form) for all 52 states (including DC and Overseas) for a given year
#'Stores table by year .rda files in useeior package

#' 1 - Load state industry output for the given year.
year <- 2012
load(paste0("data/State_Summary_IndustryOutput_", year, ".rda"))
State_Summary_IndustryOutput <- do.call(rbind)

#' 2 - Load US Summary Make table for given year
#' Generate US Summary Industry Output
US_Summary_Make <- get(paste("Summary_Make", year, "BeforeRedef", sep = "_"), as.environment("package:useeior"))*1E6
US_Summary_MakeTransaction <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                              -which(colnames(US_Summary_Make)=="Total Industry Output")]
US_Summary_IndustryOutput <- rowSums(US_Summary_MakeTransaction)

#' 3 - Load US Summary Make table for given year
#' Generate US Summary Industry Output
US_Summary_Use <- get(paste("Summary_Use", year, "PRO_BeforeRedef", sep = "_"), as.environment("package:useeior"))
US_Summary_UseTransaction <- US_Summary_Use[colnames(US_Summary_MakeTransaction),
                                            rownames(US_Summary_MakeTransaction)]*1E6

#' 4 - Calculate state_US_IndustryOutput_ratio, for each state and each industry,
#' Divide state IndustryOutput by US IndustryOutput.
State_Summary_UseTransaction_list <- list()
for (state in names(State_Summary_IndustryOutput_list)) {
  IndustryOutputRatio <- State_Summary_IndustryOutput_list[[state]]/US_Summary_IndustryOutput
  State_Summary_UseTransaction_list[[state]] <- as.matrix(US_Summary_UseTransaction) %*% diag(IndustryOutputRatio)
  colnames(State_Summary_UseTransaction_list[[state]]) <- colnames(US_Summary_UseTransaction)
}

#' 5 - Horizontally stack all state Use trascation tables.
State_Summary_UseTransaction <- do.call(cbind, State_Summary_UseTransaction_list)
colnames(State_Summary_UseTransaction) <- paste(rep(names(State_Summary_UseTransaction_list),
                                                     each = ncol(State_Summary_UseTransaction_list[[1]])),
                                                 rep(colnames(State_Summary_UseTransaction_list[[1]]),
                                                     time = length(names(State_Summary_UseTransaction_list))),
                                                 sep = ".")
rownames(State_Summary_UseTransaction) <- rownames(US_Summary_UseTransaction)
