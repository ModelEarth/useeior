# Get state number of establishment and employee
getBLSQCEW<- function (year) {
  # Create the placeholder file
  BLSQCEWzip <- paste0("inst/extdata/BLS_QCEW_", year, ".zip")
  # Download all BLS QCEW tables into the placeholder file
  if(!file.exists(BLSQCEWzip)) {
    download.file(paste0("https://data.bls.gov/cew/data/files/", year,
                         "/csv/", year, "_annual_by_area.zip"), BLSQCEWzip, mode = "wb")
    # Get the name of all files in the zip archive
    fname <- unzip(BLSQCEWzip, list = TRUE)[unzip(BLSQCEWzip, list = TRUE)$Length > 0, ]$Name
    # Unzip the file to the designated directory
    unzip(BLSQCEWzip, files = fname, exdir = paste0("inst/extdata/BLS_QCEW_", year), overwrite = TRUE)
  }
  filenames <- list.files(paste0("inst/extdata/BLS_QCEW_", year, "/", year, ".annual.by_area/"))
  filenames <- filenames[endsWith(filenames, "-- Statewide.csv")]
  QCEW <- data.frame()
  for (filename in filenames) {
    # Load state data
    StateQCEW <- utils::read.table(paste0("inst/extdata/BLSQCEW/", year, ".annual.by_area/", filename),
                                   sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE)
    # Aggregate by owner_code = 1, 2, 3, 5
    StateQCEW <- StateQCEW[StateQCEW$own_code%in%c(1, 2, 3, 5), ]
    StateQCEW <- stats::aggregate(StateQCEW[, c("annual_avg_estabs_count", "annual_avg_emplvl")],
                                  by = list(StateQCEW$area_fips, StateQCEW$area_title, StateQCEW$industry_code, StateQCEW$year), sum)
    # Rename columns
    colnames(StateQCEW) = c("GeoFips", "GeoName", "NAICS", "Year", "ESTAB", "EMP")
    # Modify GeoNam
    StateQCEW$GeoName <- gsub(" -- Statewide", "", StateQCEW$GeoName)
    # Append StateQCEW together
    QCEW <- rbind(QCEW, StateQCEW)
  }
  return(QCEW)
}

BLS_QCEW_2013 <- getBLSQCEW(2013)
usethis::use_data(BLS_QCEW_2013, overwrite = TRUE)
