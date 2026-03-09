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
coverageByGear <- function(dataToPlot,
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
                           catchCat = c(
                             "Lan",
                             "Dis",
                             "Catch"
                           ),
                           includeLandings = TRUE,
                           includeEffort = TRUE,
                           includeSamples = TRUE,
                           verbose = FALSE) {

  # Call the underlying function using a groupingVariable of "gear"
  plotsToPrint <- coverageByGroupingVariable(dataToPlot = dataToPlot,
                             year = year,
                             quarter = quarter,
                             vesselFlag = vesselFlag,
                             landingsVariable = landingsVariable,
                             effortVariable = effortVariable,
                             samplingVariable = samplingVariable,
                             groupingVariable = "gear",
                             catchCat = catchCat,
                             includeLandings = includeLandings,
                             includeEffort = includeEffort,
                             includeSamples = includeSamples,
                             verbose = verbose)

  plotsToPrint

}

