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
#' Species WORMS aphiaids and descriptions.
#'
#' A data frame containing the WORMS aphiaid, and
#' scientific name for a selection of species.  See
#' https://www.marinespecies.org/
#'
#'
#' @format A data frame containing the species information
#' \describe{
#'   \item{Key}{WORMS aphiaid}
#'   \item{Description}{Species name}
#' }
#' @source ICES
"wormsSpecies"
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
#' Allowed CatchCat variable names
#'
#' A vector containing the allowed names
#'
#' @format A character vector
#' @source WGRDBES-EST
"allowedCatchCat"
#' Allowed effortVariable names
#'
#' A vector containing the allowed effortVariable names
#'
#' @format A character vector
#' @source WGRDBES-EST
"allowedEffortVariable"
#' Allowed landingsVariable names
#'
#' A vector containing the allowed landingVariable names
#'
#' @format A character vector
#' @source WGRDBES-EST
"allowedLandingsVariable"
#' Allowed samplingVariable names
#'
#' A vector containing the allowed samplingVariable names
#'
#' @format A character vector
#' @source WGRDBES-EST
"allowedSamplingVariable"
