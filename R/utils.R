# This is a file to store general internal utility functions


# Deal with "no visible binding for global variable.." warnings in R CMD CHECK
globalVariables(c("CL", "cl", "CLlanCou", "CLmonth", "CLstatRect", "CLspecCode",
                  "CLcatchCat", "CLmetier6", "CLoffWeight", "CLsciWeight",
                  "CLvesFlagCou","CLyear", "CLquar", "CLGear", "CLGearCount",
                  "CLSpeCount",
                  "sa","SA","SAstatRect", "SAyear", "SAquar", "SAmonth", "SAid",
                  "SAcatchCat","SAspeCode", "SASpeCount", "SAgear",
                  "SAGearCount", "SamplingGearCount", "SAspeCount",
                  "SamplingCount", "SamplingGearCountQuar",
                  "SamplingGearCountYear",
                  "VDvesFlsgCou", "VDflgCtry",
                  "LandingCountYear", "LandingCount", "LandingCountAll",
                  "LandingsGearCount", "LandingsGearCountQuar",
                  "totalSpeCountAll", "SamplingCountYear", "totalGearYear",
                  "long", "lat", "group", "mean_quartiles_land",
                  "mean_quantiles_land","X", "Y", "bi_class"))


#' as.integer.or.dbl
#'
#' This function checks if any value in a vector is above 2e+09, and if so runs
#' `round(as.double())` on it. If not it uses `as.integer()` instead. This is to
#' get around the 32-bit limitation in R that integers cannot be larger than
#' around 2e+09, in which case `as.integer` would return an `NA`.
#'
#' @param x vector to be coerced to integers or doubles
#'
#' @return a vector of integers or doubles
#' @importFrom stats na.omit
#' @keywords internal

as.integer.or.dbl <- function(x){

  # we apply as.numeric in case it is a character vector
  # we apply as.omit because that causes an error
  if(any(as.numeric(na.omit(x)) > 2e+09)) out <- round(as.double(x)) else
    out <- as.integer(x)

  return(out)
}


#' Internal function to filter landings data for coverageXXX functions
#'
#' @param landingsData A data table of landings data ("CL")
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR"
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A data table of landings data ("CL")
#'
filterLandingsDataForCoverage <- function(landingsData,
                                          year,
                                          quarter,
                                          vesselFlag,
                                          verbose){


  if (verbose) {
    print("Filtering landings data")
  }

  ld <- landingsData

  # Filter the data based on the function's input parameters
  if (is.na(year) == TRUE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld
    } else {
      ld1 <- ld %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld %>% dplyr::filter(CLyear %in% year)
    } else {
      ld1 <- ld %>% dplyr::filter(CLyear %in% year)
      ld1 <- ld1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == TRUE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld %>% dplyr::filter(CLquar %in% quarter)
    } else {
      ld1 <- ld %>% dplyr::filter(CLquar %in% quarter)
      ld1 <- ld1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld %>% dplyr::filter(CLyear %in% year)
      ld1 <- ld1 %>% dplyr::filter(CLquar %in% quarter)
    } else {
      ld1 <- ld %>% dplyr::filter(CLyear %in% year)
      ld1 <- ld1 %>% dplyr::filter(CLquar %in% quarter)
      ld1 <- ld1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
    }
  }

  # Return filtered data
  ld1

}
#' Internal function to filter sample data for coverageXXX functions
#'
#' @param sampleData An RDBESEstObject
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Sampling catch category - landings, catch or discards
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return An RDBESEstObject
#'
filterSampleDataForCoverage <- function(sampleData,
                                        year,
                                        quarter,
                                        vesselFlag,
                                        catchCat,
                                        verbose){


  if (verbose) {
    print("Filtering sample data")
  }

  sa <- sampleData

  # Filter the data based on the function's input parameters
  if (is.na(year) == TRUE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(SAyear %in% year)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(SAyear %in% year)
      sa1 <- sa1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(year) == TRUE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(SAquar %in% quarter)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(SAquar %in% quarter)
      sa1 <- sa1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(SAyear %in% year)
      sa1 <- sa1 %>% dplyr::filter(SAquar %in% quarter)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(SAyear %in% year)
      sa1 <- sa1 %>% dplyr::filter(SAquar %in% quarter)
      sa1 <- sa1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  }

  # Return filtered data
  sa1

}

#' Internal function to filter effort data for coverageXXX functions
#'
#' @param effortData A data table of effort data ("CE")
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR"
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A data table of effort data ("CE")
#'
filterEffortDataForCoverage <- function(effortData,
                                          year,
                                          quarter,
                                          vesselFlag,
                                          verbose){

  if (verbose) {
    print("Filtering effort data")
  }

  ef <- effortData

  if (is.na(year) == TRUE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef
    } else {
      ef1 <- ef %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
    } else {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == TRUE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(CEquar %in% quarter)
    } else {
      ef1 <- ef %>% dplyr::filter(CEquar %in% quarter)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
      ef1 <- ef1 %>% dplyr::filter(CEquar %in% quarter)
    } else {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
      ef1 <- ef1 %>% dplyr::filter(CEquar %in% quarter)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  }

  # Return filtered data
  ef1

}

#' Internal function to prepare sample data for coveragesXXX functions
#'
#' @param dataToPlot An RDBESDataObject
#' @param (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return an RDBESEstObject
#'
preprocessSampleDataForCoverage <- function(dataToPlot, verbose){

  if (verbose) {
    print("Preparing sample data")
  }

  # We'll convert the CS data into a RDBESEstObject to
  # make it easier to handle here
  hierarchiesInData <- unique(dataToPlot[["DE"]]$DEhierarchy)
  if (length(hierarchiesInData) != 1) {
    stop(paste0("This function will only work if there is a single hierarchy",
                "in dataToPlot"))
  }
  datatoPlot_EstOb <- RDBEScore::createRDBESEstObject(dataToPlot,
                                                      hierarchiesInData,
                                                      verbose = verbose
  )

  # Check the RDBESEstObject is valid
  RDBEScore::validateRDBESEstObject(datatoPlot_EstOb, verbose = verbose)

  sa <- datatoPlot_EstOb

  # Join to VD to get Vessel flag country
  sa <- dplyr::left_join(sa, dataToPlot[["VD"]], by = "VDid")

  # Get the year and quarter of the sample from FO
  sa$SAyear <-
    as.integer(format(as.Date(sa$FOendDate, format = "%Y-%m-%d"), "%Y"))
  sa$SAquar <-
    as.integer(lubridate::quarter(as.Date(sa$FOendDate, format = "%Y-%m-%d")))
  sa$SAmonth <-
    as.integer(lubridate::month(as.Date(sa$FOendDate, format = "%Y-%m-%d")))

  # Get only necessary columns from the sample data

  # Find the first SA columns - we are only dealing with the top level SA data
  # TODO - should we be able to plot sub-samples as well?
  colsToCheck <-
    names(datatoPlot_EstOb)[grep("^su.table$", names(datatoPlot_EstOb))]
  correctCol <- NA
  suNumber <- NA
  for (myCol in colsToCheck) {
    myColValues <- unique(datatoPlot_EstOb[, myCol, with = FALSE])[[1]]
    myColValues <- myColValues[!is.na(myColValues)]
    if (myColValues == "SA") {
      correctCol <- myCol
      suNumber <- gsub("su", "", correctCol)
      suNumber <- gsub("table", "", suNumber)
      suNumber <- as.integer(suNumber)
      break
    }
  }
  if (is.na(correctCol)) {
    stop("Sample data could not be found - cannot continue")
  }

  # Rename the suXnumTotal and suXnumSamp columns to SAnumTotal and SAnumSamp
  sa <- sa %>% dplyr::rename("SAnumTotal" = paste0("su", suNumber, "numTotal"))
  sa <- sa %>% dplyr::rename("SAnumSamp" = paste0("su", suNumber, "numSamp"))

  # Get the columns we want
  sa <- sa %>%
    dplyr::select(
      c(
        "SAmetier5", "SAmetier6", "SAgear", "SAtotalWtLive",
        "SAsampWtLive",
        "SAnumTotal", "SAnumSamp",
        "SAtotalWtMes", "SAsampWtMes", "SAyear", "SAquar", "SAmonth",
        "SAcatchCat", "SAspeCode", "SAspeCodeFAO", "SAstatRect",
        "VDflgCtry",
        "SAid"
      )
    ) %>%
    dplyr::relocate(SAstatRect, SAyear, SAquar, SAmonth)

  # remove any duplicates (could be present because we have removed the FM
  # and BV data)
  if (length(which(duplicated(sa))) > 0) {
    sa <- sa[-which(duplicated(sa)), ]
  }

  # Remove any rows with SAid = NA, then get rid of the SAid column
  sa <- sa[!is.na(sa$SAid), ]
  sa <- dplyr::select(sa, -SAid)

  # Ensure specode is an integer
  sa$SAspeCode <- as.integer(sa$SAspeCode)

  # Return our sample data as an RDBESEstObject
  sa

}
#' Internal function to prepare landings data for coverageXXX functions
#'
#' @param dataToPlot An RDBESDataObject
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A data table of landings data ("CL")
#'
preprocessLandingsDataForCoverage <- function(dataToPlot, verbose){

  if (verbose) {
    print("Preparing landings data")
  }

  ld <- dataToPlot[["CL"]] %>%
    dplyr::select(
      CLlanCou:CLmonth,
      CLstatRect,
      CLspecCode:CLcatchCat,
      CLmetier6,
      CLoffWeight:CLsciWeight
    )
  ld$CLGear <- substr(ld$CLmetier6, 0, 3)

  # return our landings data
  ld

}

#' Internal function to prepare effort data for coverageXXX functions
#'
#' @param dataToPlot An RDBESDataObject
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A data table of effort data ("CE")
#'
preprocessEffortDataForCoverage <- function(dataToPlot, verbose){

  if (verbose) {
    print("Preparing effort data")
  }

  # get effort
  ef <- dataToPlot[["CE"]] %>%
    dplyr::select(
      CEvesFlagCou:CEMonth,
      CEstatRect,
      CEmetier6,
      CEvesLenCat,
      CEnumFracTrips:CEnumUniqVes
    )
  ef$CEGear <- substr(ef$CEmetier6, 0, 3)

  # return our effort data
  ef

}






