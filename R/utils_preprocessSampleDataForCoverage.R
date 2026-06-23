
#' Internal function to prepare sample data for coveragesXXX functions
#' NOT WORKING, to be check.
#' @param dataToPlot An RDBESDataObject
#' @param (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return an RDBESEstObject
#'
preprocessSampleDataForCoverage <- function(dataToPlot, verbose) {
  if (verbose) {
    print("Preparing sample data")
  }

  # We'll convert the CS data into a RDBESEstObject to
  # make it easier to handle here
  hierarchiesInData <- unique(dataToPlot[["DE"]]$DEhierarchy)
  if (length(hierarchiesInData) != 1) {
    stop(paste0(
      "This function will only work if there is a single hierarchy",
      "in dataToPlot"
    ))
  }
  datatoPlot_EstOb <- RDBEScore::createRDBESEstObject(dataToPlot,
                                                      hierarchiesInData,
                                                      verbose = verbose
  )

  # Check the RDBESEstObject is valid
  RDBEScore::validateRDBESEstObject(datatoPlot_EstOb, verbose = verbose)

  sa <- datatoPlot_EstOb

  # Join to VD to get Vessel flag country
  sa <- dplyr::left_join(sa, dataToPlot[["VD"]], by = "VDid")

  # Get the year and quarter of the sample from FO
  sa$SAyear <-
    as.integer(format(as.Date(sa$FOendDate, format = "%Y-%m-%d"), "%Y"))
  sa$SAquar <-
    as.integer(lubridate::quarter(as.Date(sa$FOendDate, format = "%Y-%m-%d")))
  sa$SAmonth <-
    as.integer(lubridate::month(as.Date(sa$FOendDate, format = "%Y-%m-%d")))

  # Get only necessary columns from the sample data

  # Find the first SA columns - we are only dealing with the top level SA data
  # TODO - should we be able to plot sub-samples as well?
  colsToCheck <-
    names(datatoPlot_EstOb)[grep("^su.table$", names(datatoPlot_EstOb))]
  correctCol <- NA
  suNumber <- NA
  for (myCol in colsToCheck) {
    myColValues <- unique(datatoPlot_EstOb[, myCol, with = FALSE])[[1]]
    myColValues <- myColValues[!is.na(myColValues)]
    if (myColValues == "SA") {
      correctCol <- myCol
      suNumber <- gsub("su", "", correctCol)
      suNumber <- gsub("table", "", suNumber)
      suNumber <- as.integer(suNumber)
      break
    }
  }
  if (is.na(correctCol)) {
    stop("Sample data could not be found - cannot continue")
  }

  # Rename the suXnumTotal and suXnumSamp columns to SAnumTotal and SAnumSamp
  sa <- sa %>% dplyr::rename("SAnumTotal" = paste0("su", suNumber, "numTotal"))
  sa <- sa %>% dplyr::rename("SAnumSamp" = paste0("su", suNumber, "numSamp"))

  # Get the columns we want
  sa <- sa[, c(
    "SAstatRect", "SAyear", "SAquar", "SAmonth", "SAmetier5",
    "SAmetier6", "SAgear", "SAtotalWtLive", "SAsampWtLive",
    "SAnumTotal", "SAnumSamp", "SAtotalWtMes", "SAsampWtMes",
    "SAcatchCat", "SAspeCode", "SAspeCodeFAO", "VDflgCtry", "SAid"
  )]
  sa$year <- sa$SAyear
  sa$quarter <- sa$SAquar
  sa$month <- sa$SAmonth

  # remove any duplicates (could be present because we have removed the FM
  # and BV data)
  if (length(which(duplicated(sa))) > 0) {
    sa <- sa[-which(duplicated(sa)), ]
  }

  # Remove any rows with SAid = NA, then get rid of the SAid column
  sa <- sa[!is.na(sa$SAid), ]
  sa <- dplyr::select(sa, -SAid)


  # Append the species names

  # Ensure specode is an integer
  sa$SAspeCode <- as.integer(sa$SAspeCode)
  # Add an extra column called "specCode" to be consisten with CL
  sa$SAspecCode <- sa$SAspeCode
  full_name <- RDBESvisualise::wormsSpecies
  full_name <- dplyr::distinct(full_name, Key, .keep_all = TRUE)
  sa <-
    dplyr::left_join(sa, full_name, by = c("SAspeCode" = "Key"))
  names(sa)[names(sa) == "Description"] <- "SAspeciesName"


  # Return our sample data as an RDBESEstObject
  sa
}

testData <- RDBEScore::createRDBESDataObject(
  input = "data-raw/exampleData/wszystko/2025_10_14_125156.zip",Hierarchy = 1)

preSample_utils = preprocessSampleDataForCoverage(testData, verbose = FALSE)
