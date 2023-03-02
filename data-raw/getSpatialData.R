
icesRectSF <- sf::read_sf("./data-raw/Maps/shapefiles/ICESrect.shp")
#class(icesRectSF)
usethis::use_data(icesRectSF, overwrite = TRUE)

icesRectSpatialPolygon <- rgdal::readOGR(
  dsn = paste0(getwd(), "./data-raw/Maps/shapefiles"),
  layer = "ICESrect",
  verbose = FALSE
)
#class(icesRectSpatialPolygon)
usethis::use_data(icesRectSpatialPolygon, overwrite = TRUE)

shoreline <- rgdal::readOGR(
  dsn = "./data-raw/Maps/shapefiles/GSHHG/gshhg-shp-2.3.7/GSHHS_shp/l",
  layer = "GSHHS_l_L1",
  verbose = FALSE
)
usethis::use_data(shoreline, overwrite = TRUE)
