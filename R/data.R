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
#' @format An sf object with 19 variables:
#' \describe{
#'   \item{OBJECTID}{Object identifier from the source GIS layer.}
#'   \item{ID}{Internal identifier.}
#'   \item{ICESNAME}{ICES statistical rectangle code.}
#'   \item{SOUTH}{Southern latitude boundary.}
#'   \item{WEST}{Western longitude boundary.}
#'   \item{NORTH}{Northern latitude boundary.}
#'   \item{EAST}{Eastern longitude boundary.}
#'   \item{AREA_KM2}{Area in square kilometres.}
#'   \item{stat_x}{X coordinate of the rectangle centroid.}
#'   \item{stat_y}{Y coordinate of the rectangle centroid.}
#'   \item{Area_27}{ICES Area 27 code.}
#'   \item{Perc}{Percentage overlap with associated ICES area.}
#'   \item{MaxPer}{Maximum overlap percentage.}
#'   \item{RNDMaxPer}{Rounded maximum overlap percentage.}
#'   \item{AreasList}{List of overlapping ICES areas.}
#'   \item{Shape_Leng}{Polygon perimeter length from the source GIS layer.}
#'   \item{Shape_Le_1}{Additional perimeter length field from the source GIS layer.}
#'   \item{Shape_Area}{Polygon area from the source GIS layer.}
#'   \item{geometry}{sf geometry column containing ICES statistical rectangle polygons.}
#' }
#' @source ICES
"icesRectSF"
#' ICES statistical areas
#'
#' An sf object containing spatial information for ICES statistical areas
#'
#' @format An sf object with 11 variables:
#' \describe{
#'   \item{OBJECTID_1}{Object identifier from the source GIS layer.}
#'   \item{OBJECTID}{Object identifier from the source GIS layer.}
#'   \item{Major_FA}{Major FAO fishing area.}
#'   \item{SubArea}{ICES Subarea.}
#'   \item{Division}{ICES Division.}
#'   \item{SubDivisio}{ICES Subdivision.}
#'   \item{Unit}{ICES assessment or management unit.}
#'   \item{Area_Full}{Full name of the ICES area.}
#'   \item{Area_27}{ICES Area 27 code.}
#'   \item{Area_km2}{Area in square kilometres.}
#'   \item{geometry}{sf geometry column containing ICES area polygons.}
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
#' Colour palette for countries
#'
#' A named character vector of colours (hex codes) used in the plots of the
#' package. Names are the country codes (e.g. `"BE"`, `"DK"`, `"PL"`).
#'
#' @format A named character vector of hex colour codes.
#' @source Created with `data-raw/palette.R`.
"colourPalette"
#' ICES Statistical Rectangle Lookup Table
#'
#' Lookup table linking ICES statistical rectangles to ICES areas and
#' higher-level spatial regions.
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
