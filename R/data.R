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
#' ICES statistical areas
#'
#' An sf object containing spatial information for ICES statistical areas
#'
#' @format A data frame containing spatial information
#' \describe{
#'   \item{Area_Full}{Name of the ICES statistical area}...
#' }
#' @source ICES: https://gis.ices.dk/geonetwork/srv/api/records/c784a0a3-752f-4b50-b02f-f225f6c815eb
"icesAreaSF"
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
#' ICES statistical rectangles with their ICES areas and RCG regions
#'
#' One row per ICES statistical rectangle. Built from [icesRectSF] and
#' [icesAreaSF] by `data-raw/icesStatRecLookup.R`.
#'
#' @format A data frame:
#' \describe{
#'   \item{StatRec}{ICES statistical rectangle, e.g. "41F6"}
#'   \item{Area}{ICES area covering most of the rectangle, e.g. "27.4.b"}
#'   \item{SubDivision}{e.g. "27.3.d.28"; NA where the area has no subdivision}
#'   \item{Division}{e.g. "27.3.d"}
#'   \item{SubArea}{e.g. "27.3"}
#'   \item{FAOArea}{"27"}
#'   \item{Region}{RCG region: "BA", "NSEA" or "NAtl"}
#'   \item{AreasList}{All ICES areas the rectangle overlaps, e.g. "7.d, 7.e"}
#' }
#' @source ICES Spatial Facility, via icesRectSF and icesAreaSF
"icesStatRecLookup"
