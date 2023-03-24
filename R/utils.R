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
                                          yearToFilter,
                                          quarterToFilter,
                                          vesselFlag,
                                          verbose){


  if (verbose) {
    print("Filtering landings data")
  }

  ld <- landingsData

  # Filter the data based on the function's input parameters
  if (is.na(yearToFilter) == TRUE && is.na(quarterToFilter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld
    } else {
      ld1 <- ld %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(yearToFilter) == FALSE && is.na(quarterToFilter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld %>% dplyr::filter(year %in% yearToFilter)
    } else {
      ld1 <- ld %>% dplyr::filter(year %in% yearToFilter)
      ld1 <- ld1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(yearToFilter) == TRUE && is.na(quarterToFilter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld %>% dplyr::filter(quarter %in% quarterToFilter)
    } else {
      ld1 <- ld %>% dplyr::filter(quarter %in% quarterToFilter)
      ld1 <- ld1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(yearToFilter) == FALSE && is.na(quarterToFilter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ld1 <- ld %>% dplyr::filter(year %in% yearToFilter)
      ld1 <- ld1 %>% dplyr::filter(quarter %in% quarterToFilter)
    } else {
      ld1 <- ld %>% dplyr::filter(year %in% yearToFilter)
      print(unique(ld1$quarter))
      ld1 <- ld1 %>% dplyr::filter(quarter %in% quarterToFilter)
      print(unique(ld1$quarter))
      ld1 <- ld1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
      print(unique(ld1$quarter))
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
                                        yearToFilter,
                                        quarterToFilter,
                                        vesselFlag,
                                        catchCat,
                                        verbose){


  if (verbose) {
    print("Filtering sample data")
  }

  sa <- sampleData

  # Filter the data based on the function's input parameters
  if (is.na(yearToFilter) == TRUE && is.na(quarterToFilter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(yearToFilter) == FALSE && is.na(quarterToFilter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(year %in% yearToFilter)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(year %in% yearToFilter)
      sa1 <- sa1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(yearToFilter) == TRUE && is.na(quarterToFilter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(SAquar %in% quarterToFilter)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(quarter %in% quarterToFilter)
      sa1 <- sa1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(yearToFilter) == FALSE && is.na(quarterToFilter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      sa1 <- sa %>% dplyr::filter(year %in% yearToFilter)
      sa1 <- sa1 %>% dplyr::filter(quarter %in% quarterToFilter)
      sa1 <- sa1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      sa1 <- sa %>% dplyr::filter(year %in% yearToFilter)
      sa1 <- sa1 %>% dplyr::filter(quarter %in% quarterToFilter)
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
                                          yearToFilter,
                                          quarterToFilter,
                                          vesselFlag,
                                          verbose){

  if (verbose) {
    print("Filtering effort data")
  }

  ef <- effortData

  if (is.na(yearToFilter) == TRUE && is.na(quarterToFilter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef
    } else {
      ef1 <- ef %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(yearToFilter) == FALSE && is.na(quarterToFilter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(year %in% yearToFilter)
    } else {
      ef1 <- ef %>% dplyr::filter(year %in% yearToFilter)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(yearToFilter) == TRUE && is.na(quarterToFilter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(quarter %in% quarterToFilter)
    } else {
      ef1 <- ef %>% dplyr::filter(quarter %in% quarterToFilter)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(yearToFilter) == FALSE && is.na(quarterToFilter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(year %in% yearToFilter)
      ef1 <- ef1 %>% dplyr::filter(quarter %in% quarterToFilter)
    } else {
      ef1 <- ef %>% dplyr::filter(year %in% yearToFilter)
      ef1 <- ef1 %>% dplyr::filter(quarter %in% quarterToFilter)
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
  sa <- sa[,c("SAstatRect", "SAyear", "SAquar", "SAmonth", "SAmetier5",
          "SAmetier6", "SAgear", "SAtotalWtLive", "SAsampWtLive",
          "SAnumTotal", "SAnumSamp", "SAtotalWtMes", "SAsampWtMes",
          "SAcatchCat", "SAspeCode", "SAspeCodeFAO", "VDflgCtry", "SAid")]
  sa$year <- sa$SAyear
  sa$quarter <- sa$SAquar
  sa$month <- sa$SAmonth

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

  # get landings
  ld <- dataToPlot[["CL"]][,c("CLlanCou", "CLvesFlagCou", "CLyear", "CLquar",
      "CLmonth", "CLstatRect", "CLspecCode", "CLspecFAO", "CLlandCat",
      "CLcatchCat", "CLmetier6", "CLoffWeight", "CLsciWeight")]

  ld$CLGear <- substr(ld$CLmetier6, 0, 3)
  ld$year <- ld$CLyear
  ld$quarter <- ld$CLquar
  ld$month <- ld$CLmonth

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
  ef <- dataToPlot[["CE"]][,c("CEvesFlagCou","CEyear","CEquar","CEMonth",
      "CEstatRect","CEmetier6", "CEvesLenCat", "CEnumFracTrips",
      "CEnumDomTrip", "CEoffDaySea", "CESciDaySea", "CEoffFishDay",
      "CEsciFishDay", "CEoffNumHaulSet", "CEsciNumHaulSet", "CEoffVesFishHour",
      "CEsciVesFishHour", "CEoffSoakMeterHour", "CEsciSoakMeterHour",
      "CEoffkWDaySea", "CEscikWDaySea", "CEoffkWFishDay",
      "CEscikWFishDay", "CEoffkWFishHour", "CEscikWFishHour", "CEgTDaySea",
      "CEgTFishDay", "CEgTFishHour", "CEnumUniqVes")]

  ef$CEGear <- substr(ef$CEmetier6, 0, 3)
  ef$year <- ef$CEyear
  ef$quarter <- ef$CEquar
  ef$month <- ef$CEMonth

  # return our effort data
  ef

}
#' Internal function to return a list of barplots which compare the
#' groupingVariable in landings, effort, and sample data.
#'
#' @param landingsData Landings data
#' @param effortData Effort data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param quarter Quarter of year
#' @param landingsVariable Variable from CL to plot
#' @param landingsVariable Variable from CE to plot
#' @param samplingVariable variable from SA to plot
#'
#' @return A tagList of plotly plots
#'
barPlotsByGroupingVariable <- function(landingsData = NA,
                     effortData = NA,
                     sampleData = NA,
                     vesselFlag,
                     catchCat,
                     quarter,
                     landingsVariable,
                     effortVariable,
                     samplingVariable,
                     groupingVariable) {


  #groupingVariable <- "gear"
  regExToFind <- paste0("^..",groupingVariable,"$")

  #names(landingsData)
  #test <- c("GEAR","sagear", "CLGEAR", "CEGEARS")



  # see what data we've been given
  if (length(landingsData) == 1 && is.na(landingsData)) {
    landings <- FALSE
  } else {
    landings <- TRUE
    # create a new column with the name we want to group by
    CLcolName <- names(landingsData)[grepl(regExToFind,names(landingsData), ignore.case = TRUE)]
    if (CLcolName != groupingVariable) {
      landingsData[,groupingVariable] <- landingsData[[CLcolName]]
    }
  }
  if (length(effortData) == 1 && is.na(effortData)) {
    effort <- FALSE
  } else {
    effort <- TRUE
    # create a new column with the name we want to group by
    CEcolName <- names(effortData)[grepl(regExToFind,names(effortData), ignore.case = TRUE)]
    if (CEcolName != groupingVariable) {
      effortData[,groupingVariable] <- effortData[[CEcolName]]
    }
  }
  if (length(sampleData) == 1 && is.na(sampleData)) {
    samples <- FALSE
  } else {
    samples <- TRUE
    # create a new column with the name we want to group by
    SAcolName <- names(sampleData)[grepl(regExToFind,names(sampleData), ignore.case = TRUE)]
    if (SAcolName != groupingVariable) {
      sampleData[,groupingVariable] <- sampleData[[SAcolName]]
    }
  }


  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }


  if (landings) {

    # Sum landingsVariable by year
    df1 <- na.omit(
      landingsData %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(CLSumForYear = sum(!!rlang::sym(landingsVariable))) %>%
        dplyr::mutate(CLSumTotal = sum(CLSumForYear))
    )

    # Sum landingsVariable by year, and groupingVariable
    d1 <- na.omit(
      landingsData %>%
        dplyr::group_by(year, !!rlang::sym(groupingVariable)) %>%
        dplyr::summarize(LandingsByGroup = sum(!!rlang::sym(landingsVariable)))
    )

    d1 <- dplyr::left_join(d1, df1, by = "year") %>%
      dplyr::mutate(
        relativeValues =
          LandingsByGroup / CLSumForYear
        #LandingsByGroup / CLSumTotal
      )
  }

  # Samples
  if (samples) {

    # Sum samplingVariable by year
    df2 <- na.omit(
      sampleData %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(SASumForYear = sum(!!rlang::sym(samplingVariable))) %>%
        dplyr::mutate(SASumTotal = sum(SASumForYear))
    )

    # Sum samplingVariable by year, and groupingVariable
    d2 <- na.omit(
      sampleData %>%
        dplyr::group_by(year, !!rlang::sym(groupingVariable)) %>%
        dplyr::summarize(SAByGroup = sum(!!rlang::sym(samplingVariable)))
    )

    d2 <- dplyr::left_join(d2, df2, by = "year") %>%
      dplyr::mutate(
        relativeValues =
          SAByGroup / SASumTotal
      )
  }


  # Effort
  if (effort) {

    # Sum effortVariable by year
    df3 <- na.omit(
      effortData %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(CESumForYear = sum(!!rlang::sym(effortVariable))) %>%
        dplyr::mutate(CESumTotal = sum(CESumForYear))
    )

    # Sum effortVariable by year, and groupingVariable
    d3 <- na.omit(
      effortData %>%
        #dplyr::group_by(CEyear, CEquar, CEGear) %>%
        dplyr::group_by(year, !!rlang::sym(groupingVariable)) %>%
        dplyr::summarize(EffortByGroup = sum(!!rlang::sym(effortVariable)))
    )

    d3 <- dplyr::left_join(d3, df3, by = "year") %>%
      dplyr::mutate(
        relativeValues =
          EffortByGroup / CESumTotal
      )
  }


  # if (is.na(quarter) == FALSE) {
  #   # Landings
  #   if (landings) {
  #
  #     # Sum landingsVariable by year and quarter
  #     df1 <- na.omit(
  #       landingsData %>%
  #         dplyr::group_by(year, quarter) %>%
  #         dplyr::summarize(CLSumForYear = sum(!!rlang::sym(landingsVariable))) %>%
  #         dplyr::mutate(CLSumTotal = sum(CLSumForYear))
  #     )
  #
  #     # Sum landingsVariable by year, quarter, and groupingVariable
  #     d1 <- na.omit(
  #       landingsData %>%
  #         #dplyr::group_by(CLyear, CLquar, CLGear) %>%
  #         dplyr::group_by(year, quarter, !!rlang::sym(groupingVariable)) %>%
  #         dplyr::summarize(LandingsByGroup = sum(!!rlang::sym(landingsVariable)))
  #     )
  #
  #     d1 <- dplyr::left_join(d1, df1, by = "year", "quarter") %>%
  #       dplyr::mutate(
  #         relativeValues =
  #           LandingsByGroup / CLSumForYear
  #           #LandingsByGroup / CLSumTotal
  #       )
  #   }
  #
  #   # Samples
  #   if (samples) {
  #
  #     # Sum samplingVariable by year and quarter
  #     df2 <- na.omit(
  #       sampleData %>%
  #         dplyr::group_by(year, quarter) %>%
  #         dplyr::summarize(SASumForYear = sum(!!rlang::sym(samplingVariable))) %>%
  #         dplyr::mutate(SASumTotal = sum(SASumForYear))
  #     )
  #
  #     # Sum samplingVariable by year, and gear, and groupingVariable
  #     d2 <- na.omit(
  #       sampleData %>%
  #         #dplyr::group_by(SAyear, SAquar, SAgear) %>%
  #         dplyr::group_by(year, quarter, !!rlang::sym(groupingVariable)) %>%
  #         dplyr::summarize(SAByGroup = sum(!!rlang::sym(samplingVariable)))
  #     )
  #
  #     d2 <- dplyr::left_join(d2, df2, by = "year", "quarter") %>%
  #       dplyr::mutate(
  #         relativeValues =
  #           SAByGroup / SASumTotal
  #       )
  #   }
  #
  #
  #   # Effort
  #   if (effort) {
  #
  #     # Sum effortVariable by year
  #     df3 <- na.omit(
  #       effortData %>%
  #         dplyr::group_by(year, quarter) %>%
  #         dplyr::summarize(CESumForYear = sum(!!rlang::sym(effortVariable))) %>%
  #         dplyr::mutate(CESumTotal = sum(CESumForYear))
  #     )
  #
  #     # Sum effortVariable by year, and gear and groupingVariable
  #     d3 <- na.omit(
  #       effortData %>%
  #         #dplyr::group_by(CEyear, CEquar, CEGear) %>%
  #         dplyr::group_by(year, quarter, !!rlang::sym(groupingVariable)) %>%
  #         dplyr::summarize(EffortByGroup = sum(!!rlang::sym(effortVariable)))
  #     )
  #
  #     d3 <- dplyr::left_join(d3, df3, by = "year", "quarter") %>%
  #       dplyr::mutate(
  #         relativeValues =
  #           EffortByGroup / CESumTotal
  #       )
  #   }
  # } else {
  #   # Landings
  #   if (landings) {
  #
  #     # Sum landingsVariable by year
  #     df1 <- na.omit(
  #       landingsData %>%
  #         dplyr::group_by(year) %>%
  #         dplyr::summarize(CLSumForYear = sum(!!rlang::sym(landingsVariable))) %>%
  #         dplyr::mutate(CLSumTotal = sum(CLSumForYear))
  #     )
  #
  #     # Sum landingsVariable by year, and groupingVariable
  #     d1 <- na.omit(
  #       landingsData %>%
  #         dplyr::group_by(year, !!rlang::sym(groupingVariable)) %>%
  #         dplyr::summarize(LandingsByGroup = sum(!!rlang::sym(landingsVariable)))
  #     )
  #
  #     d1 <- dplyr::left_join(d1, df1, by = "year") %>%
  #       dplyr::mutate(relativeValues = LandingsByGroup / CLSumTotal)
  #   }
  #
  #   # Samples
  #   if (samples) {
  #
  #     # Sum samplingVariable by year
  #     df2 <- na.omit(
  #       sampleData %>%
  #         dplyr::group_by(year) %>%
  #         dplyr::summarize(SASumForYear = sum(!!rlang::sym(samplingVariable))) %>%
  #         dplyr::mutate(SASumTotal = sum(SASumForYear))
  #     )
  #
  #     # Sum samplingVariable by year, and groupingVariable
  #     d2 <- na.omit(
  #       sampleData %>%
  #         #dplyr::group_by(SAyear, SAgear) %>%
  #         dplyr::group_by(year, !!rlang::sym(groupingVariable)) %>%
  #         dplyr::summarize(SAByGroup = sum(!!rlang::sym(samplingVariable)))
  #     )
  #
  #     d2 <- dplyr::left_join(d2, df2, by = "year") %>%
  #       dplyr::mutate(
  #         relativeValues =
  #           SAByGroup / SASumTotal
  #       )
  #   }
  #
  #   # Effort
  #   if (effort) {
  #
  #     # Sum effortVariable by year
  #     df3 <- na.omit(
  #       effortData %>%
  #         dplyr::group_by(year) %>%
  #         dplyr::summarize(CESumForYear = sum(!!rlang::sym(effortVariable))) %>%
  #         dplyr::mutate(CESumTotal = sum(CESumForYear))
  #     )
  #
  #     # Sum effortVariable by year, and groupingVariable
  #     d3 <- na.omit(
  #       effortData %>%
  #         #dplyr::group_by(CEyear, CEGear) %>%
  #         dplyr::group_by(year, !!rlang::sym(groupingVariable)) %>%
  #         dplyr::summarize(EffortByGroup = sum(!!rlang::sym(effortVariable)))
  #     )
  #
  #     d3 <- dplyr::left_join(d3, df3, by = "year") %>%
  #       dplyr::mutate(relativeValues = EffortByGroup / CESumTotal)
  #   }
  # }


  # Get the years we want plot
  y <- c()
  if (landings) {
    y <- c(y, unique(d1$year))
  }
  if (samples) {
    y <- c(y, unique(d2$year))
  }
  if (effort) {
    y <- c(y, unique(d3$year))
  }
  y <- sort(unique(y))


  all_plot <- htmltools::tagList()

  formulaForGraph <- paste0('~as.character(', groupingVariable,')')

  for (i in seq_along(length(y))) {

    # Landings
    if (landings) {
      dd <- d1 %>% dplyr::filter(year == y[i])
      dd <- dd[-1]
      p1 <- createBarPlot(dataToPlot = dd,
                          formulaForGraph = formulaForGraph,
                          title = landingsVariable)

    } else {
      p1 <- plotly::plotly_empty(type = "bar")
    }

    # Samples
    if (samples) {
      ds <- d2 %>% dplyr::filter(year == y[i])
      ds <- ds[-1]
      p2 <- createBarPlot(dataToPlot = ds,
                          formulaForGraph = formulaForGraph,
                          title = samplingVariable)

    } else {
      p2 <- plotly::plotly_empty(type = "bar")
    }

    # Effort
    if (effort) {
      dde <- d3 %>% dplyr::filter(year == y[i])
      dde <- dde[-1]
      p3 <- createBarPlot(dataToPlot = dde,
                          formulaForGraph = formulaForGraph,
                          title = effortVariable)
    } else {
      p3 <- plotly::plotly_empty(type = "bar")
    }

    # Create the overall plot title
    myTitle <- paste0("Vessel Flag ", flagLabel,
                      " : ", groupingVariable, " "
    )
    if (samples){
      myTitle <- paste0(myTitle, "(", catchCat, ") ")
    }
    myTitle <- paste0(myTitle, "- Relative Values per Plot \n in ")
    if (!is.na(quarter)){
      myTitle <- paste0(myTitle, "Q", quarter, ", " )
    }
    myTitle <- paste0(myTitle, y[i])

    # Combine our plots
    all_plot[[i]] <- plotly::subplot(p1, p3, p2, titleY = TRUE, nrows = 3) %>%
      plotly::layout(title = myTitle)
  }
  all_plot
}
#' Internal function to create a bar plot
#'
#' @param dataToPlot The data to plot
#' @param formulaForGraph The formula for the x axis value
#' @param title  Title to use for the plot
#'
#' @return A plotly plot
#'
createBarPlot <- function(dataToPlot, formulaForGraph, title){

  p1 <- plotly::plot_ly(
    dataToPlot,
    x = as.formula(formulaForGraph),
    y = ~ relativeValues,
    color = as.formula(formulaForGraph),
    type = "bar",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      yaxis = list(
        title = title,
        titlefont = list(size = 12)
      ),
      xaxis = list(categoryorder = "total descending"),
      barmode = "stack"
    )

  p1

}






