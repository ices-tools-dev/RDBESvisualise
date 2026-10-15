## icesAreaSF was stored in EPSG:3857 (web Mercator), while icesRectSF and
## shoreline are in EPSG:4326. Reproject it so the layers can be combined. This
## also removes the non-ASCII characters of the EPSG:3857 description, which
## R CMD check reports.

load("data/icesAreaSF.rda")
icesAreaSF <- sf::st_transform(icesAreaSF, 4326)
usethis::use_data(icesAreaSF, overwrite = TRUE)
