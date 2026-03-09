# This is a file to store general internal utility functions


# Deal with "no visible binding for global variable.." warnings in R CMD CHECK
usedVars <- c('.SD', '.data','.N','..var')


globalVariables(unique(c(usedVars)))
