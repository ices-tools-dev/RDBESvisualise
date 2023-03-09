#' Provides graphical outputs which compare the statistical
#' rectangle where landings occured to the statistical rectangles where
#' sampling occured.  The relative amounts of landings/samples are
#' shown by the use of different colours for the rectangle.
#'
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param commercialVariable Landings variable to be assessed
#' @param samplingVariable Sampling Variable to be assessed
#' @param catchCat Sampling catch category - "Lan", "Dis",or "Catch"
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A tagList of ggplot2 plots
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   myH1RawObject <- RDBEScore::createRDBESDataObject(
#'                   rdbesExtractPath = "../tests/testthat/h1_v_1_19_13")
#'
#'   myYear = 1965
#'   myvesselFlag = "ZW"
#'
#'   coverageLandingsSpatialBivariate(
#'     dataToPlot = myH1RawObject,
#'     year = myYear,
#'     vesselFlag = myvesselFlag,
#'     catchCat = "Lan",
#'     commercialVariable = "CLoffWeight",
#'     samplingVariable = "SAsampWtLive"
#'     )
#'
#' }
coverageLandingsSpatialBivariate <- function(dataToPlot,
                                             year = NA,
                                             vesselFlag = NA,
                                             commercialVariable = c(
                                               "CLoffWeight",
                                               "CLsciWeight"
                                             ),
                                             samplingVariable = c(
                                               "SAsampWtLive",
                                               "SAnumSamp",
                                               "SAsampWtMes"
                                             ),
                                             catchCat = c(
                                               "Lan",
                                               "Dis",
                                               "Catch"
                                             ),
                                             verbose = FALSE){


  # STEP 0) VALIDATE INPUTS

  # check the parameters are valid before we do anything
  if (verbose) {
    print("Validating input parameters")
  }

  if (length(catchCat) == 3) {
    stop("You must provide a Catch Category")
  } else if (length(catchCat) == 2) {
    stop("Only one Catch Category can be provided")
  }

  if (length(vesselFlag) > 1) {
    stop("Only one vessel flag country can be provided")
  }

  if (length(commercialVariable) > 1 ||
      length(samplingVariable) > 1) {
    stop("You must provide  commercialVariable and  samplingVariable")
  }

  if (is.na(year) == TRUE) {
    stop("You must provide  the year")
  }

  if (length(catchCat) == 1 && !catchCat %in% c("Lan", "Dis", "Catch")) {
    stop(paste0("Invalid catchCat value:", catchCat))
  }

  # Check the input data is valid
  RDBEScore::validateRDBESDataObject(dataToPlot, verbose = verbose)

  # STEP 1) PREPARE THE DATA

  ld <- preprocessLandingsDataForCoverage(dataToPlot, verbose = verbose)
  sa <- preprocessSampleDataForCoverage(dataToPlot, verbose = verbose)

  # STEP 2) FILTER THE DATA BASED ON THE INPUT PARAMETERS

  ld1 <- filterLandingsDataForCoverage(ld,
                                       year = year,
                                       quarter = NA,
                                       vesselFlag = vesselFlag,
                                       verbose = verbose)
  sa1 <- filterSampleDataForCoverage(sa,
                                     year = year,
                                     quarter = NA,
                                     vesselFlag = vesselFlag,
                                     catchCat = catchCat,
                                     verbose = verbose)


  # STEP 3) Plot the data

  if (verbose) {
    print("Preparing plots")
  }

  plotsToPrint <- bivariatePlot(
    landingsData = ld1,
    sampleData = sa1,
    vesselFlag = vesselFlag,
    catchCat = catchCat,
    commercialVariable = commercialVariable,
    samplingVariable = samplingVariable
  )

  plotsToPrint


}
