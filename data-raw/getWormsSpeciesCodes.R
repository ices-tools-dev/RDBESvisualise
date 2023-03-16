library(icesVocab)
wormsSpecies <- icesVocab::getCodeList("SpecWoRMS")
wormsSpeciesCurrent <- wormsSpecies[!wormsSpecies$Deprecated,]
wormsSpeciesCurrent$Deprecated <- NULL
wormsSpeciesCurrent$Modified <- NULL
wormsSpeciesCurrent$LongDescription <- NULL
#wormsSpeciesCurrent$Key <- as.character(wormsSpeciesCurrent$Key)
wormsSpecies <- wormsSpeciesCurrent
usethis::use_data(wormsSpecies, overwrite = TRUE)
