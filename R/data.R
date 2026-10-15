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
#' (EPSG:4326). Some rectangles have more than one row.
#'
#' @format An sf data frame with the fields of the ICES shapefile:
#' \describe{
#'   \item{OBJECTID, ID}{Identifiers in the shapefile}
#'   \item{ICESNAME}{Name of the stat rectangle, e.g. "41F6"}
#'   \item{SOUTH, WEST, NORTH, EAST}{Limits of the rectangle in degrees}
#'   \item{AREA_KM2}{Area of the rectangle in km2}
#'   \item{stat_x, stat_y}{Longitude and latitude of the centre of the
#'   rectangle}
#'   \item{Area_27}{ICES area covering most of the rectangle, without the
#'   "27." prefix, e.g. "4.b"}
#'   \item{Perc}{Percentage of the rectangle in each area of `AreasList`}
#'   \item{MaxPer, RNDMaxPer}{Largest value of `Perc`, and rounded}
#'   \item{AreasList}{All ICES areas the rectangle overlaps}
#'   \item{Shape_Leng, Shape_Le_1, Shape_Area}{Perimeter and area in degrees}
#'   \item{geometry}{Polygon of the rectangle}
#' }
#' @source ICES
"icesRectSF"
#' ICES statistical areas
#'
#' An sf object containing spatial information for ICES statistical areas
#' (EPSG:4326, reprojected from EPSG:3857 by `data-raw/icesAreaSF.R`).
#'
#' @format An sf data frame with the fields of the ICES shapefile:
#' \describe{
#'   \item{OBJECTID_1, OBJECTID}{Identifiers in the shapefile}
#'   \item{Major_FA}{FAO major fishing area, "27"}
#'   \item{SubArea, Division, SubDivisio, Unit}{Parts of the area code}
#'   \item{Area_Full}{Code of the ICES statistical area, e.g. "27.3.d.27"}
#'   \item{Area_27}{Area code without the "27." prefix}
#'   \item{Area_km2}{Area in km2}
#'   \item{geometry}{Polygon of the area}
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
#' Colour palette for countries
#'
#' A named vector of 47 colours, one for each country code, so that a country
#' gets the same colour in all plots. The plotting functions also use these
#' colours, after the RCG palette, when there are more than 19 groups to plot.
#' Built by `data-raw/palette.R`.
#'
#' @format A named character vector of hexadecimal colours, named by
#' ISO 3166-1 alpha-2 country code
#' @source RColorBrewer palettes Set1, Set3, Set2, Dark2 and Pastel1
#' @examples
#' colourPalette[c("EE", "PL", "SE")]
"colourPalette"
