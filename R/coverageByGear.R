#' Provides graphical outputs to compare the fishing gears
#' in effort to those in the sample data.
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4.
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Sampling catch category - "Lan", "Dis",or "Catch"
#' @param includeLandings (Optional) Set to TRUE to include landings.
#' Default is TRUE
#' #' @param includeEffort (Optional) Set to TRUE to include effort
#' Default is TRUE
#' #' @param includeSamples (Optional) Set to TRUE to include samples.
#' Default is TRUE
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A tagList of plotly plots
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
#'   myPlots <- coverageLandingsByGear(
#'     dataToPlot = myH1RawObject,
#'     year = myYear,
#'     vesselFlag = myvesselFlag,
#'     catchCat = "Lan"
#'     )
#'
#'   myPlots[1]
#'
#' }
coverageByGear <- function(dataToPlot,
                                   year = NA,
                                   quarter = NA,
                                   vesselFlag = NA,
                                   catchCat = c(
                                     "Lan",
                                     "Dis",
                                     "Catch"
                                   ),
                                  includeLandings = TRUE,
                                  includeEffort = TRUE,
                                  includeSamples = TRUE,
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

  if (length(catchCat) == 1 && !catchCat %in% c("Lan", "Dis", "Catch")) {
    stop(paste0("Invalid catchCat value:", catchCat))
  }

  # Check the input data is valid
  RDBEScore::validateRDBESDataObject(dataToPlot, verbose = verbose)

  # STEP 1) PREPARE AND FILTER THE DATA

  # Landings
  if (includeLandings){
    ld <- preprocessLandingsDataForCoverage(dataToPlot, verbose = verbose)
    ld1 <- filterLandingsDataForCoverage(ld,
                                         year = year,
                                         quarter = quarter,
                                         vesselFlag = vesselFlag,
                                         verbose = verbose)
  } else {
    ld1 <- NA
  }

  # Effort
  if (includeEffort){
    ef <- preprocessEffortDataForCoverage(dataToPlot, verbose = verbose)
    ef1 <- filterEffortDataForCoverage(ef,
                                       year = year,
                                       quarter = quarter,
                                       vesselFlag = vesselFlag,
                                       verbose = verbose)
  } else {
    ef1 <- NA
  }

  # Samples
  if (includeSamples){
    sa <- preprocessSampleDataForCoverage(dataToPlot, verbose = verbose)
    sa1 <- filterSampleDataForCoverage(sa,
                                       year = year,
                                       quarter = quarter,
                                       vesselFlag = vesselFlag,
                                       catchCat = catchCat,
                                       verbose = verbose)
  } else {
    sa1 <- NA
  }


  # STEP 2) Plot the data

  if (verbose) {
    print("Preparing plots")
  }

  plotsToPrint <- gearPlot(
    landingsData = ld1,
    effortData = ef1,
    sampleData = sa1,
    vesselFlag = vesselFlag,
    catchCat = catchCat,
    quarter = quarter
  )

  plotsToPrint


}
