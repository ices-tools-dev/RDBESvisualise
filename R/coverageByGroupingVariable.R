#' Provides graphical outputs to compare the fishing gears
#' in effort to those in the sample data.
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4.
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param landingsVariable Landings variable to be assessed
#' @param effortVariable Effort variable to be assessed
#' @param samplingVariable Sampling Variable to be assessed
#' @param groupingVariable The variable from the landings/effort/sample data
#' that we want to group by
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
#' myH1RawObject <- RDBEScore::createRDBESDataObject(
#'   rdbesExtractPath = "../tests/testthat/h1_v_1_19_13"
#' )
#'
#' myYear <- 1965
#' myvesselFlag <- "ZW"
#'
#' myPlots <- coverageLandingsByGear(
#'   dataToPlot = myH1RawObject,
#'   year = myYear,
#'   vesselFlag = myvesselFlag,
#'   catchCat = "Lan"
#' )
#'
#' myPlots[1]
#' }
coverageByGroupingVariable <- function(dataToPlot,
                           year = NA,
                           quarter = NA,
                           vesselFlag = NA,
                           landingsVariable = c(
                             "CLoffWeight",
                             "CLsciWeight"
                           ),
                           effortVariable = c(
                             "CEnumFracTrips",
                             "CEnumDomTrip"
                           ),
                           samplingVariable = c(
                             "SAsampWtLive",
                             "SAnumSamp",
                             "SAsampWtMes"
                           ),
                           groupingVariable,
                           catchCat = c(
                             "Lan",
                             "Dis",
                             "Catch"
                           ),
                           includeLandings = TRUE,
                           includeEffort = TRUE,
                           includeSamples = TRUE,
                           topN = NA,
                           plotQuarters = FALSE,
                           verbose = FALSE) {
  # STEP 0) VALIDATE INPUTS

  # check the parameters are valid before we do anything
  if (verbose) {
    print("Validating input parameters")
  }

  validateCoverageParameters(year = year,
                             quarter = quarter,
                             vesselFlag = vesselFlag,
                             landingsVariable = landingsVariable,
                             effortVariable = effortVariable,
                             samplingVariable = samplingVariable,
                             groupingVariable = NA,
                             catchCat = catchCat,
                             includeLandings = includeLandings,
                             includeEffort = includeEffort,
                             includeSamples = includeSamples,
                             topN = topN)


  # Check the input data is valid
  RDBEScore::validateRDBESDataObject(dataToPlot, verbose = verbose)

  # STEP 1) PREPARE AND FILTER THE DATA

  # Landings
  if (includeLandings) {
    ld <- preprocessLandingsDataForCoverage(dataToPlot, verbose = verbose)
    ld1 <- filterLandingsDataForCoverage(ld,
      year = year,
      quarter = quarter,
      vesselFlag = vesselFlag,
      verbose = verbose
    )
  } else {
    ld1 <- NA
  }

  # Effort
  if (includeEffort) {
    ef <- preprocessEffortDataForCoverage(dataToPlot, verbose = verbose)
    ef1 <- filterEffortDataForCoverage(ef,
      year = year,
      quarter = quarter,
      vesselFlag = vesselFlag,
      verbose = verbose
    )
  } else {
    ef1 <- NA
  }

  # Samples
  if (includeSamples) {
    sa <- preprocessSampleDataForCoverage(dataToPlot, verbose = verbose)
    sa1 <- filterSampleDataForCoverage(sa,
      year = year,
      quarter = quarter,
      vesselFlag = vesselFlag,
      catchCat = catchCat,
      verbose = verbose
    )
  } else {
    sa1 <- NA
  }


  # STEP 2) Plot the data

  if (verbose) {
    print("Preparing plots")
  }

  plotsToPrint <- barPlotsByGroupingVariable(landingsData = ld1,
                             effortData = ef1,
                             sampleData = sa1,
                             vesselFlag = vesselFlag,
                             catchCat = catchCat,
                             quarter = quarter,
                             landingsVariable = landingsVariable,
                             effortVariable = effortVariable,
                             samplingVariable = samplingVariable,
                             groupingVariable = groupingVariable,
                             topN = topN,
                             plotQuarters = plotQuarters)

  plotsToPrint
}

