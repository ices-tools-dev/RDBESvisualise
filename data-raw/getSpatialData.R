library(sf)

icesRects <- read_sf("./data-raw/Maps/shapefiles/ICESrect.shp")
usethis::use_data(icesRects, overwrite = TRUE)

shoreline <- rgdal::readOGR(
  dsn = "./data-raw/Maps/shapefiles/GSHHG/gshhg-shp-2.3.7/GSHHS_shp/l",
  layer = "GSHHS_l_L1",
  verbose = FALSE
)
usethis::use_data(shoreline, overwrite = TRUE)
