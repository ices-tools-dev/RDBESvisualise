################################################################################
# 
# Please note that following script updates the wormsSpecies data 
# (see data/wormsSpecies.rda) in order to take into account possible updates on 
# the server side. Hence, it should be run during the pre-release process. 
#
# Inquiries: David Currie, Marine Institute (Ireland) @RDBESvisualise
#
################################################################################

library(icesVocab)
wormsSpecies <- icesVocab::getCodeList("SpecWoRMS")
wormsSpeciesCurrent <- wormsSpecies[!wormsSpecies$Deprecated,]
wormsSpeciesCurrent$Deprecated <- NULL
wormsSpeciesCurrent$Modified <- NULL
wormsSpeciesCurrent$LongDescription <- NULL
#wormsSpeciesCurrent$Key <- as.character(wormsSpeciesCurrent$Key)
wormsSpecies <- wormsSpeciesCurrent
usethis::use_data(wormsSpecies, overwrite = TRUE)