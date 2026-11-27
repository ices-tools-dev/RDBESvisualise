## Builds data/icesStatRecLookup.rda from icesRectSF and icesAreaSF.
## Re-run after updating either shapefile.

load("data/icesRectSF.rda")
load("data/icesAreaSF.rda")

areaCodes <- unique(as.character(sf::st_drop_geometry(icesAreaSF)$Area_Full))

# RCG region from the finest ICES area code, following the RCG regions as
# used in the RCG regional overviews:
#   BA   = 27.3.b, 27.3.c, 27.3.d
#   NSEA = 27.1, 27.2, 27.3.a, 27.4, 27.5.a, 27.7.d, 27.12, 27.14 (and NAFO 21)
#   NAtl = 27.5 to 27.10, except 27.5.a and 27.7.d
# Codes not in `validAreas` give NA.
icesAreaToRcgRegion <- function(area, validAreas) {
  region <- rep(NA_character_, length(area))
  region[grepl("^27\\.(5|6|7|8|9|10)(\\.|$)", area)] <- "NAtl"
  region[grepl("^27\\.(1|2|4|12|14)(\\.|$)|^27\\.3\\.a(\\.|$)|^27\\.5\\.a(\\.|$)|^27\\.7\\.d(\\.|$)|^21(\\.|$)",
               area)] <- "NSEA"
  region[grepl("^27\\.3\\.[bcd](\\.|$)", area)] <- "BA"
  region[!area %in% validAreas] <- NA_character_
  region
}

# First n parts of a code: "27.3.d.28.1" -> n = 3 -> "27.3.d"
codeLevel <- function(code, n) {
  vapply(strsplit(code, ".", fixed = TRUE), function(p) {
    if (length(p) < n) NA_character_ else paste(p[seq_len(n)], collapse = ".")
  }, character(1))
}

# The rectangle layer holds some rectangles twice
rects <- unique(sf::st_drop_geometry(icesRectSF)[, c("ICESNAME", "Area_27", "AreasList")])

icesStatRecLookup <- data.frame(
  StatRec   = as.character(rects$ICESNAME),
  Area      = paste0("27.", rects$Area_27),
  AreasList = as.character(rects$AreasList),
  stringsAsFactors = FALSE
)
icesStatRecLookup$SubDivision <- codeLevel(icesStatRecLookup$Area, 4)
icesStatRecLookup$Division    <- codeLevel(icesStatRecLookup$Area, 3)
icesStatRecLookup$SubArea     <- codeLevel(icesStatRecLookup$Area, 2)
icesStatRecLookup$FAOArea     <- codeLevel(icesStatRecLookup$Area, 1)
icesStatRecLookup$Region      <- icesAreaToRcgRegion(icesStatRecLookup$Area, areaCodes)

icesStatRecLookup <- icesStatRecLookup[order(icesStatRecLookup$StatRec),
                                       c("StatRec", "Area", "SubDivision", "Division", "SubArea",
                                         "FAOArea", "Region", "AreasList")]
rownames(icesStatRecLookup) <- NULL

# Checks
stopifnot(
  !anyDuplicated(icesStatRecLookup$StatRec),
  all(icesStatRecLookup$Area %in% areaCodes),   # every rectangle area is a real ICES area
  all(areaCodes %in% icesStatRecLookup$Area),   # every ICES area is reachable
  !anyNA(icesStatRecLookup$Region)
)

usethis::use_data(icesStatRecLookup, overwrite = TRUE)
