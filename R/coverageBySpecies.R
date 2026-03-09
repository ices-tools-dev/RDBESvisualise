#' Provides graphical outputs to compare the species in
#' landings to those in the sample data.
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param landingsVariable Landings variable to be assessed
#' @param samplingVariable Sampling Variable to be assessed
#' @param catchCat Sampling catch category - "Lan", "Dis",or "Catch"
#' @param includeLandings (Optional) Set to TRUE to include landings in the
#' plot - default is TRUE.
#' @param includeSamples (Optional) Set to TRUE to include samples in the
#' plot - default is TRUE.
#' @param topN (Optional) Set to a posiive integer N to limit the data that is
#' plotted to the top N species each year. Default is 10.
#' @param plotQuarters (Optional) Set to TRUE to colour the data in the
#' plots by quarter - default is FALSE.
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
#'   rdbesExtractPath = "./tests/testthat/h1_v_1_19_13"
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
coverageBySpecies <- function(dataToPlot,
                              year = NA,
                              vesselFlag = NA,
                              landingsVariable = c(
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
                              includeLandings = TRUE,
                              includeSamples = TRUE,
                              topN = 10,
                              plotQuarters = FALSE,
                              verbose = FALSE) {

  # Call the underlying function using a groupingVariable of "speciesName"
  plotsToPrint <- coverageByGroupingVariable(dataToPlot = dataToPlot,
                                             year = year,
                                             quarter = NA,
                                             vesselFlag = vesselFlag,
                                             landingsVariable = landingsVariable,
                                             effortVariable = NA,
                                             samplingVariable = samplingVariable,
                                             groupingVariable = "speciesName",
                                             catchCat = catchCat,
                                             includeLandings = includeLandings,
                                             includeEffort = FALSE,
                                             includeSamples = includeSamples,
                                             topN = topN,
                                             plotQuarters = plotQuarters,
                                             verbose = verbose)

  plotsToPrint


}
