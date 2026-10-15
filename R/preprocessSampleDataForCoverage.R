#' Internal function to prepare sample data for coverageXXX functions
#'
#' Flattens the sample data with [RDBEScore::createRDBESEstObject()] and
#' returns one row per top level sample (SA) with the columns used by the
#' coverageXXX functions and [produceSamplingCoverage()].
#'
#' The date of a sample is taken from the first of `FOendDate`, `FTarvDate`,
#' `LEdate` and `OSsamDate` that has a value, so that hierarchies without the
#' FO table can be used (#34). Some hierarchies (e.g. 8, 9, 11 and 12) have no
#' mandatory date. For samples without any date the year is taken from
#' `DEyear`, the quarter and month are `NA` and a warning is given (#19).
#'
#' The gear, rectangle and area are taken from `SAgear`, `SAstatRect` and
#' `SAarea`, or from the same fields of FO or LE if they are missing in SA
#' (#33). This also allows spatial plots of the samples in hierarchies
#' without FO.
#'
#' @param dataToPlot An RDBESDataObject containing a single hierarchy
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A data table with one row per sample
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' - Notes/scope: Tested with the RDBEScore 0.4.0 example data for
#'   hierarchies 1, 5, 7 and 8 and with the H1 test data in
#'   `tests/testthat/h1_v_20250211`. Replaces the two
#'   `preprocessSamplingDataForCoverage()` functions (#25).
#' @keywords internal
preprocessSampleDataForCoverage <- function(dataToPlot, verbose = FALSE) {

  # Check that VD is present - stop with an error if not
  if (is.null(dataToPlot[["VD"]])) {
    stop("No VD data present in sample data - this is required")
  }

  if (verbose) {
    print("Preparing sample data")
  }

  # We'll convert the CS data into a RDBESEstObject to
  # make it easier to handle here
  hierarchiesInData <- unique(dataToPlot[["DE"]]$DEhierarchy)
  if (length(hierarchiesInData) != 1) {
    stop(paste0(
      "This function will only work if there is a single hierarchy ",
      "in dataToPlot"
    ))
  }
  datatoPlot_EstOb <- RDBEScore::createRDBESEstObject(dataToPlot,
    hierarchiesInData,
    verbose = verbose
  )

  # Check the RDBESEstObject is valid
  RDBEScore::validateRDBESEstObject(datatoPlot_EstOb, verbose = verbose)

  # Join to VD to get Vessel flag country
  sa <- dplyr::left_join(datatoPlot_EstOb, dataToPlot[["VD"]], by = "VDid")

  # Get the date of each sample from the first table that has one
  sampleDate <- as.Date(rep(NA_character_, nrow(sa)))
  dateColumns <- c("FOendDate", "FTarvDate", "LEdate", "OSsamDate")
  for (dateColumn in intersect(dateColumns, names(sa))) {
    noDate <- is.na(sampleDate)
    sampleDate[noDate] <- as.Date(as.character(sa[[dateColumn]][noDate]),
                                  format = "%Y-%m-%d")
  }
  sa$SAyear <- as.integer(format(sampleDate, "%Y"))
  sa$SAquar <- as.integer(lubridate::quarter(sampleDate))
  sa$SAmonth <- as.integer(lubridate::month(sampleDate))
  # Without a date only the year is known
  sa$SAyear[is.na(sampleDate)] <-
    as.integer(as.character(sa$DEyear[is.na(sampleDate)]))

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
    if (length(myColValues) == 1 && myColValues == "SA") {
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

  # Use the gear, rectangle and area of FO, or else LE, when they are NA or
  # empty in SA
  for (field in c("gear", "statRect", "area")) {
    saColumn <- paste0("SA", field)
    sa[[saColumn]] <- as.character(sa[[saColumn]])
    for (upperColumn in intersect(paste0(c("FO", "LE"), field), names(sa))) {
      sa[[saColumn]] <- ifelse(is.na(sa[[saColumn]]) | sa[[saColumn]] == "",
                               as.character(sa[[upperColumn]]),
                               sa[[saColumn]])
    }
  }

  # Get the columns we want
  sa <- sa[, c(
    "SAstatRect", "SAarea", "SAyear", "SAquar", "SAmonth", "SAmetier5",
    "SAmetier6", "SAgear", "SAtotalWtLive", "SAsampWtLive",
    "SAnumTotal", "SAnumSamp", "SAtotalWtMes", "SAsampWtMes",
    "SAcatchCat", "SAspeCode", "SAspeCodeFAO", "VDflgCtry", "VDlenCat", "SAid"
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

  nNoDate <- sum(is.na(sa$SAquar))
  if (nNoDate > 0) {
    warning("No sample date found for ", nNoDate, " of ", nrow(sa),
            " samples; their year is taken from DEyear and their quarter ",
            "and month are NA", call. = FALSE)
  }

  # Append the species names

  # Ensure specode is an integer
  sa$SAspeCode <- as.integer(as.character(sa$SAspeCode))
  # Add an extra column called "specCode" to be consisten with CL
  sa$SAspecCode <- sa$SAspeCode
  full_name <- RDBESvisualise::wormsSpecies
  full_name <- dplyr::distinct(full_name, Key, .keep_all = TRUE)
  sa <-
    dplyr::left_join(sa, full_name, by = c("SAspeCode" = "Key"))
  names(sa)[names(sa) == "Description"] <- "SAspeciesName"

  sa
}
