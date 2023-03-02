#' Species names and codes.
#'
#' A data frame containing the FAO code, FAO description, Aphia ID, and
#' scientific name for a selection of species
#'
#' @format A data frame containing the species information
#' \describe{
#'   \item{FAOCode}{Three letter FAO code for species}
#'   \item{FAODescription}{Text description of the FAO code}
#'   \item{ScientificName}{Scientific name of the species}
#'   \item{AphiaID}{Aphia ID of the species}
#' }
#' @source David Currie
"speciesNamesAndCodes"
#' ICES statistical rectangles
#'
#' An sf object containing spatial information for ICES statistical rectangles
#'
#' @format A data frame containing spatial information
#' \describe{
#'   \item{ICESNAME}{Name of the stat rectangle}...
#' }
#' @source ICES
"icesRectSF"
#' Shoreline spatial data
#'
#' A data frame containing spatial information for shorelines
#'
#' @format A SpatialPolygonsDataFrame
#' @source GSHHG
"shoreline"
#' Shoreline spatial data
#'
#' A SpatialPolygonsDataFrame data frame containing spatial information for
#' ICES statistical rectangles
#'
#' @format A SpatialPolygonsDataFrame
#' @source ICES
"icesRectSpatialPolygon"
