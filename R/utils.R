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
      ld1 <- ld1 %>% dplyr::filter(quarter %in% quarterToFilter)
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


  # Append the species names

  # Ensure specode is an integer
  sa$SAspeCode <- as.integer(sa$SAspeCode)
  # Add an extra column called "specCode" to be consisten with CL
  sa$SAspecCode <- sa$SAspeCode
  full_name <- RDBESvisualise::wormsSpecies
  full_name <- dplyr::distinct(full_name, Key, .keep_all = TRUE)
  sa <-
    dplyr::left_join(sa, full_name, by = c("SAspeCode" = "Key"))
  names(sa)[names(sa) == "Description"] <- "SAspeciesName"


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

  # Ensure specode is an integer
  ld$CLspecCode <- as.integer(ld$CLspecCode)
  full_name <- RDBESvisualise::wormsSpecies
  full_name <- dplyr::distinct(full_name, Key, .keep_all = TRUE)
  ld <-
    dplyr::left_join(ld, full_name, by = c("CLspecCode" = "Key"))
  names(ld)[names(ld) == "Description"] <- "CLspeciesName"

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
                     groupingVariable,
                     topN = NA,
                     plotQuarters = FALSE) {



  regExToFind <- paste0("^..",groupingVariable,"$")

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  # see what data we've been given
  if (length(landingsData) == 1 && is.na(landingsData)) {
    landings <- FALSE
  } else {
    landings <- TRUE
    # create a new column with the name we want to group by
    CLcolName <- names(landingsData)[grepl(regExToFind,names(landingsData), ignore.case = TRUE)]
    if (length(CLcolName) == 0){
      stop(paste0("Error - could not find ",
                  groupingVariable,
                  " in the landings data columns"))
    } else if (length(CLcolName) > 1) {
      stop(paste0("Error - mutiple matches for ",
                  groupingVariable,
                  " in the landings data columns"))
    }
    # Create new columns to make life easier
    landingsData$groupingVariable <- landingsData[[CLcolName]]
    landingsData$variableToSum <- landingsData[[rlang::sym(landingsVariable)]]

    # Format the data ready for plotting
    d1 <- formatDataForBarPlotsByGroupingVariable(landingsData, topN)

  }

  if (length(effortData) == 1 && is.na(effortData)) {
    effort <- FALSE
  } else {
    effort <- TRUE
    # create a new column with the name we want to group by
    CEcolName <- names(effortData)[grepl(regExToFind,names(effortData), ignore.case = TRUE)]
    if (length(CEcolName) == 0){
      stop(paste0("Error - could not find ",
                  groupingVariable,
                  " in the effort data columns"))
    } else if (length(CEcolName) > 1) {
      stop(paste0("Error - mutiple matches for ",
                  groupingVariable,
                  " in the effort data columns"))
    }
    effortData$groupingVariable <- effortData[[CEcolName]]
    effortData$variableToSum <- effortData[[rlang::sym(effortVariable)]]

    # Format the data ready for plotting
    d3 <- formatDataForBarPlotsByGroupingVariable(effortData, topN)

  }

  if (length(sampleData) == 1 && is.na(sampleData)) {
    samples <- FALSE
  } else {
    samples <- TRUE
    # create a new column with the name we want to group by
    SAcolName <- names(sampleData)[grepl(regExToFind,names(sampleData), ignore.case = TRUE)]
    if (length(SAcolName) == 0){
      stop(paste0("Error - could not find ",
                  groupingVariable,
                  " in the sample data columns"))
    } else if (length(SAcolName) > 1) {
      stop(paste0("Error - mutiple matches for ",
                  groupingVariable,
                  " in the sample data columns"))
    }
    sampleData$groupingVariable <- sampleData[[SAcolName]]
    sampleData$variableToSum <- sampleData[[rlang::sym(samplingVariable)]]

    # Format the data ready for plotting
    d2 <- formatDataForBarPlotsByGroupingVariable(sampleData, topN)
  }

  # Get the years we want to plot
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

  for (i in seq_along(length(y))) {

    # Just show legend for first plot when we display quarters
    if (plotQuarters){
      showPlotLegend <- TRUE
    } else {
      showPlotLegend <- FALSE
    }

    # Landings
    if (landings) {
      dd <- d1 %>% dplyr::filter(year == y[i])
      dd <- dd[-1]

      p1 <- createBarPlot(dataToPlot = dd,
                          title = landingsVariable,
                          plotQuarters = plotQuarters,
                          showLegend = showPlotLegend)

      showPlotLegend <- FALSE

    } else {
      p1 <- plotly::plotly_empty(type = "bar")
    }

    # Samples
    if (samples) {
      ds <- d2 %>% dplyr::filter(year == y[i])
      ds <- ds[-1]

      p2 <- createBarPlot(dataToPlot = ds,
                          title = samplingVariable,
                          plotQuarters = plotQuarters,
                          showLegend = showPlotLegend)

      showPlotLegend <- FALSE

    } else {
      p2 <- plotly::plotly_empty(type = "bar")
    }

    # Effort
    if (effort) {
      dde <- d3 %>% dplyr::filter(year == y[i])
      dde <- dde[-1]

      p3 <- createBarPlot(dataToPlot = dde,
                          title = effortVariable,
                          plotQuarters = plotQuarters,
                          showLegend = showPlotLegend)

      showPlotLegend <- FALSE

    } else {
      p3 <- plotly::plotly_empty(type = "bar")
    }

    # Create the overall plot title
    myTitle <- paste0("Vessel Flag ", flagLabel, " ")
    if (!is.na(topN)){
      myTitle <- paste0(myTitle, "Top ", topN, " values of ")
    }
    if (samples){
      myTitle <- paste0(myTitle, groupingVariable, " (", catchCat, ") ")
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
#' @param title  Title to use for the plot
#' @param plotQuarters Show bars coloured by quarter?
#' @param showLegend Show the legend?
#'
#' @return A plotly plot
#'
createBarPlot <- function(dataToPlot,
                          title,
                          plotQuarters = FALSE,
                          showLegend = FALSE){

  if (plotQuarters){
    formulaForColour <- "~ as.character(quarter)"
    legendText <- "<b> Quarter: </b>"
  } else {
    formulaForColour <- "~ groupingVariable"
    legendText <- ""
  }

  myPalette <-  c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                  "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                  "#E5C494", "#B15928", "#FDDAEC", "#E7298A", "#FFFFCC",
                  "#FFED6F", "#F2F2F2", "#AAAAAA", "#666666")

  p1 <- plotly::plot_ly(
    dataToPlot,
    x = ~ groupingVariable,
    y = ~ relativeValue,
    color = as.formula(formulaForColour),
    type = "bar",
    showlegend = showLegend,
    colors = myPalette
  ) %>%
    plotly::layout(
      yaxis = list(
        title = title,
        titlefont = list(size = 12)
      ),
      xaxis = list(categoryorder = "total descending"),
      barmode = "stack",
      legend = list(title = list(text = legendText))
    )

  p1

}

#' Internal function to fromat the data ready for the createBarPlot()
#' function.
#'
#' @param dataToFormat The data to plot
#' @param topN A value to limit the data plotted to the top N values
#'
#' @return A data frame ready for createBarPlot()
#'
formatDataForBarPlotsByGroupingVariable <- function(dataToFormat, topN){

  # Calculate sum per group,year,quarter
  d1 <- dataToFormat %>%
    dplyr::group_by(year, quarter, groupingVariable) %>%
    dplyr::mutate(sumByGroupYearQuarter = sum(variableToSum))
  # Calculate sum per group,year
  d1 <- d1 %>%
    dplyr::group_by(year, groupingVariable) %>%
    dplyr::mutate(sumByGroupYear = sum(variableToSum))
  # Calculate sum per year
  d1 <- d1 %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(sumByYear = sum(variableToSum))
  # Calculate a relative figure
  d1 <- d1 %>%
    dplyr::mutate(relativeValue =
                    sumByGroupYearQuarter / sumByYear
    )

  # Just keep the distinct columns we need
  d1 <- d1[,c("year", "quarter", "groupingVariable",
              "sumByGroupYearQuarter","sumByGroupYear","sumByYear",
              "relativeValue")] %>%
    dplyr::distinct(year, quarter, groupingVariable, .keep_all = TRUE)

  # Add a sort order column
  # TODO - there must be a simpler way of doing this!
  sortOrder <- d1[,c("year", "groupingVariable", "sumByGroupYear")]
  sortOrder$temp <- 1
  sortOrder <- sortOrder %>%
    dplyr::distinct(year, groupingVariable, .keep_all = TRUE) %>%
    dplyr::arrange(desc(sumByGroupYear), .by_group = TRUE) %>%
    dplyr::mutate("sortOrder" = cumsum(temp)) %>%
    dplyr::select(-temp)
  d1 <- dplyr::left_join(d1,
                         sortOrder[, c("year",
                                       "groupingVariable",
                                       "sortOrder")],
                         by = c("year", "groupingVariable"))
  # Add the quarter/10 to the sort order so its unqiue for each row
  d1$sortOrder <- d1$sortOrder + (d1$quarter/10.0)

  # Restrict to top N groups (if we need to)
  if(!is.na(topN)){
    d1 <- d1[d1$sortOrder <= topN + 0.5,]
  }

  d1

}






