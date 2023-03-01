library(openxlsx)

# Read spreadsheet with species codes and names and make it available in
# the package
speciesFile <- "./data-raw/SpeciesCodes.xlsx"
speciesNamesAndCodes <- read.xlsx(speciesFile)
usethis::use_data(speciesNamesAndCodes, overwrite = TRUE)
