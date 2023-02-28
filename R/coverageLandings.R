#' Provides graphical outputs to compare sampled data against ladnings in
#' using the RDBES format.
#'
#' @param dataToPlot RDBES data to be plotted as an RDBESEstObject
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4.
#' @param Vessel_flag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param var Variable of interest - "gear" or "Statrec"
#' @param CommercialVariable Landings variable to be assessed
#' @param SamplingVariable Sampling Variable to be assessed
#' @param CatchCat Sampling catch category - landings, catch or discards
#' @param SpatialPlot Type of Spatial plot to return - bivariate or points
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   createRDBESDataObject(rdbesExtractPath = "tests/testthat/h1_v_1_19_13")
#' myH1EstObj <- createRDBESEstObject(myH1RawObject, 1)
#'
#' # Example 1:
#' biasLandings(
#'   dataToPlot = myH1EstObj,
#'   year = 2018,
#'   Vessel_flag = "FR",
#'   var = "species",
#'   CatchCat = "Lan"
#' )
#'
#' }
coverageLandings <- function(dataToPlot,
                             year = NA,
                             quarter = NA,
                             Vessel_flag = NA,
                             var = c("species", "gear", "Statrec"),
                             CommercialVariable = c(
                               "CLoffWeight",
                               "CLsciWeight"
                             ),
                             SamplingVariable = c(
                               "SAsampWtLive",
                               "SAnumSamp",
                               "SAsampWtMes"
                             ),
                             CatchCat = c(
                               "Lan",
                               "Dis",
                               "Catch"
                             ),
                             SpatialPlot = c(
                               "Bivariate",
                               "Points"
                             )) {

  # For testing
  myH1RawObject <-
    RDBEScore::createRDBESDataObject(rdbesExtractPath = "tests/testthat/h1_v_1_19_13")
  myH1EstObj <- RDBEScore::createRDBESEstObject(myH1RawObject, 1)
  dataToPlot = myH1EstObj
  year = 1965
  Vessel_flag = "ZW"
  var = "species"
  CatchCat = "Lan"


  # check the parameters are valid before we do anything

  if (length(CatchCat) == 3) {
    stop("You must provide a Catch Category")
  } else if (length(CatchCat) == 2) {
    stop("Only one Catch Category can be provided")
  } else {
    CatchCat
  }

  if (length(Vessel_flag) > 1) {
    stop("Only one vessel flag country can be provided")
  }
  if (var == "Statrec" || length(var) > 1) {
    if (length(CommercialVariable) > 1 ||
        length(SamplingVariable) > 1) {
      stop("You must provide  CommercialVariable and  SamplingVariable")
    }
  }

  if (var == "Statrec" && length(SpatialPlot) > 1) {
    stop("You must choose a Spatial Plot")
  }

  if (var == "Statrec" && is.na(quarter) == FALSE) {
    stop("Unused argument, no quarters avialable for  var Statrec")
  }

  if (var == "Statrec" && is.na(year) == TRUE) {
    stop("You must provide  the year")
  }

  if (var == "gear" && is.na(quarter) == FALSE) {
    stop("Unused argument, no quarters available for  var gear")
  }

  # Check the input data is valid
  RDBEScore::validateRDBESEstObject(dataToPlot)


  # TODO - everything else :-)


}
