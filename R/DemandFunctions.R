# Functions for assembling final demand vectors

#'Registry of functions that construct various demand vector in the form of as a named list with nested names
#'as keys and function name as values
DemandVectorFunctionRegistry <- list()
# Production
DemandVectorFunctionRegistry$Production$Complete <- "prepareProductionDemand"
DemandVectorFunctionRegistry$Production$Domestic <- "prepareDomesticProductionDemand"
# Consumption
DemandVectorFunctionRegistry$Consumption$Complete <- "prepareConsumptionDemand"
DemandVectorFunctionRegistry$Consumption$Domestic <- "prepareDomesticConsumptionDemand"
DemandVectorFunctionRegistry$Consumption$Household <- "prepareHouseholdDemand"

#' Sums across sectors for a given set of codes/cols in a given final demand df
#' @param Y, a model Demand df 
#' @param codes, sector code(s) for a subset of Final Demand cols
#' @return A named vector with model sectors and demand amounts
sumDemandCols <- function(Y,codes) {
  if (length(codes)>1) {
    y <- rowSums(Y[,codes])
  } else {
    y <- Y[,codes]
    names(y) <- rownames(Y)
  }
  return(y)
}

#' Sums the demand cols representing final consumption, i.e. household, investment, and government
#' Complete national consumption formula: y_c <-  Y_h + Y_v + Y_g 
#' Domestic portion of national consumption: y_dc <- Y_dh + Y_dv + Y_dg
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param Y, a model Demand df.
#' @param location, str of location code for demand vector
#' @return A named vector with model sectors and demand amounts
sumforConsumption <- function(model, Y, location) {
  codes <- model$FinalDemandMeta[model$FinalDemandMeta$Group%in%c("Household", "Investment", "Government") &
                                 grepl(location, model$FinalDemandMeta$Code_Loc),
                                 "Code_Loc"]
  y_c <- sumDemandCols(Y, codes)
  return (y_c) 
}

#' Prepares a demand vector representing production
#' Formula for production vector: y_p <- y_c + y_e + y_m + y_delta
#' where y_c = consumption, y_e = exports, y_m = imports, y_delta = change in inventories
#' y_m values are generally negative in the BEA data and thus are added (whereas when positive they are subtracted)
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareProductionDemand <- function(model, location) {
  if (model$specs$IODataSource == "stateior") {
    y_p <- prepare2RDemand(model, location, domestic = FALSE)
  } else {
    loc <- grepl(location, model$FinalDemandMeta$Code_Loc)
    export_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Export" & loc, "Code_Loc"]
    changeinventories_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="ChangeInventories" & loc, "Code_Loc"]
    import_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Import" & loc, "Code_Loc"]
    y_c <- sumforConsumption(model, model$FinalDemand, location)
    y_e <- sumDemandCols(model$FinalDemand, export_code)
    y_m <- sumDemandCols(model$FinalDemand, import_code)
    y_delta <- sumDemandCols(model$FinalDemand, changeinventories_code)
    y_p <- y_c + y_e + y_m + y_delta
  }
  return(y_p)
}

#' Prepares a demand vector representing domestic production
#' Formula for production vector: y_p <- y_dc + y_e + y_d_delta + mu
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareDomesticProductionDemand <- function(model, location) {
  if (model$specs$IODataSource == "stateior") {
    y_d_p <- prepare2RDemand(model, location, domestic = TRUE) 
  } else {
    loc <- grepl(location, model$FinalDemandMeta$Code_Loc)
    export_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Export" & loc, "Code_Loc"]
    changeinventories_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="ChangeInventories" & loc, "Code_Loc"]
    y_d_c <- sumforConsumption(model, model$DomesticFinalDemand, location)
    y_d_e <- sumDemandCols(model$DomesticFinalDemand, export_code)
    y_d_delta <- sumDemandCols(model$DomesticFinalDemand, changeinventories_code)
    mu <- model$InternationalTradeAdjustment
    y_d_p <- y_d_c + y_d_e + y_d_delta + mu
  }
  return(y_d_p)
}

#' Prepares a demand vector representing Import production
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareImportProductionDemand <- function(model, location) {
  if (model$specs$IODataSource == "stateior") {
    y_m_p <- prepare2RDemand(model, location, domestic = FALSE)
    stop("Import production demand not yet implemented for 2R models.")
  } else {
    # Note that model$mu (i.e., ITA) is not included in import production demand
    # because it is included in Domestic Production Demand
    loc <- grepl(location, model$FinalDemandMeta$Code_Loc)
    export_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Export" & loc, "Code_Loc"]
    changeinventories_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="ChangeInventories" & loc, "Code_Loc"]
    import_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Import" & loc, "Code_Loc"]
    # Including InternationalTradeAdjustment in DomesticFinalDemand for import factors calculations
    ImportFinalDemand <- model$ImportMatrix[, which(colnames(model$ImportMatrix) %in% model$FinalDemandMeta$Code_Loc)]
    y_m_c <- sumforConsumption(model, ImportFinalDemand, location)
    y_m_e <- sumDemandCols(ImportFinalDemand, export_code)
    y_m_i <- sumDemandCols(ImportFinalDemand, import_code)
    y_m_delta <- sumDemandCols(ImportFinalDemand, changeinventories_code)

    y_m_p <- y_m_c + y_m_e + y_m_i + y_m_delta 
  }
  return(y_m_p)
}

#' Prepares a demand vector representing consumption
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return a named vector with demand
prepareConsumptionDemand <- function(model, location) {
  if (model$specs$IODataSource == "stateior") {
    y_c <- prepare2RDemand(model, location, domestic = FALSE, demand_type = "Consumption")
  } else {
    y_c <- sumforConsumption(model, model$FinalDemand, location)
  }
  return(y_c)
}

#' Prepares a demand vector representing Import consumption
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return a named vector with demand
prepareImportConsumptionDemand <- function(model, location) {
  if (model$specs$IODataSource == "stateior") {
    ImportMatrix <- model$U - model$U_d
    ImportMatrix <- head(ImportMatrix, -6) # drop value add rows; TODO update this
    ImportFinalDemand <- ImportMatrix[, which(colnames(ImportMatrix) %in% model$FinalDemandMeta$Code_Loc)]
    y_c <- sumforConsumption(model, ImportFinalDemand, location)
  } else {
    # Including InternationalTradeAdjustment in DomesticFinalDemand for import factors calculations
    ImportFinalDemand <- model$ImportMatrix[, which(colnames(model$ImportMatrix) %in% model$FinalDemandMeta$Code_Loc)]
    y_c <- sumforConsumption(model, ImportFinalDemand, location)
  }
  return(y_c)
}


#' Prepares a demand vector representing domestic consumption
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareDomesticConsumptionDemand <- function(model, location) {
  if (model$specs$IODataSource == "stateior") {
    y_c_d <- prepare2RDemand(model, location, domestic = TRUE, demand_type = "Consumption")
  } else {
    y_c_d <- sumforConsumption(model, model$DomesticFinalDemand, location)
  } 
  return(y_c_d)
}

#' Prepares a demand vector representing household consumption
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareHouseholdDemand <- function(model, location) {
  Y <- model$FinalDemand
  household_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Household" &
                                            grepl(location, model$FinalDemandMeta$Code_Loc), "Code_Loc"]
  y_h <- sumDemandCols(Y, household_code)
  return(y_h)
}

#' A function to validate a user provided demand vector
#' @param dv a user provided demand vector
#' @param L, the L matrix for the given model, used as a reference
#' @return A logical value indicating demand vector is valid or not.
isDemandVectorValid <- function(dv, L){
  # dv should be a named numeric vector
  # names of dv should be part of sectors in L
  is_valid <- all(is.numeric(dv), all(names(dv)%in%rownames(L)))
  return(is_valid)
}

#' Format a named demand vector with partial sectors to have all the rows and ordering needed
#' @param dv, a user provided demand vector. See calculateEEIOModel()
#' @param L, the L matrix for the given model, used as a reference
#' @return A named vector with values for all names in L and ordered like L
#' @export
formatDemandVector <- function(dv,L) {
  #create a named vector using the first col of L
  d <- c(L[,1])
  #Set all values to 0
  d[0:nrow(L)] <- 0
  #merge in names from dv
  #replace the values from d
  d[match(names(dv),names(d))] <- dv
  return(d)
}

#' Read demand vector from a csv file and format for use in model calculations
#' @param file_path str, path to csv file containing demand data
#' @param demand_name str, name of demand data as field header
#' @param model, a model
#' @return a demand vector formatted for use in calculating model results
#' @export
extractAndFormatDemandVector <- function(file_path, demand_name, model){
  demand_df <- read.csv(file_path, stringsAsFactors = FALSE)
  row.names(demand_df) <- demand_df$Code
  demand_df$Code <- NULL
  y <- na.omit(demand_df[,demand_name])
  names(y) <- row.names(demand_df)
  if(isDemandVectorValid(y, model$L)) {
    y <- formatDemandVector(y, model$L)
  } else {
    stop("Format of the demand vector is invalid.")
  }
  return(y)
}
