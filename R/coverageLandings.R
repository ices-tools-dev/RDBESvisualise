#' Provides graphical outputs to compare sampled data against ladnings in
#' using the RDBES format.
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4.
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param var Variable of interest - "gear" or "Statrec"
#' @param commercialVariable Landings variable to be assessed
#' @param samplingVariable Sampling Variable to be assessed
#' @param catchCat Sampling catch category - landings, catch or discards
#' @param spatialPlot Type of Spatial plot to return - bivariate or points
#'
#' @return
#' @export
#'
#' @importFrom magrittr "%<>%"
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   createRDBESDataObject(rdbesExtractPath = "tests/testthat/h1_v_1_19_13")
#' myH1EstObj <- createRDBESEstObject(myH1RawObject, 1)
#'
#' # Example 1:
#' coverageLandings(
#'   dataToPlot = myH1EstObj,
#'   year = 2018,
#'   vesselFlag = "FR",
#'   var = "species",
#'   catchCat = "Lan"
#' )
#'
#' }
coverageLandings <- function(dataToPlot,
                             year = NA,
                             quarter = NA,
                             vesselFlag = NA,
                             var = c("species", "gear", "Statrec"),
                             commercialVariable = c(
                               "CLoffWeight",
                               "CLsciWeight"
                             ),
                             samplingVariable = c(
                               "SAsampWtLive",
                               "SAnumSamp",
                               "SAsampWtMes"
                             ),
                             catchCat = c(
                               "Lan",
                               "Dis",
                               "Catch"
                             ),
                             spatialPlot = c(
                               "Bivariate",
                               "Points"
                             )) {

  # For testing
  # myH1RawObject <-
  # RDBEScore::createRDBESDataObject(rdbesExtractPath = "tests/testthat/h1_v_1_19_13")
  # # Generate some quarters for CL (test data is all Q1)
  # set.seed(1)
  # myH1RawObject[['CL']]$CLquar <- as.integer(runif(nrow(myH1RawObject[['CL']]), min = 1, max= 4.99))
  # # Generate some values for SAsampWtLive (test data is all blank)
  # set.seed(1)
  # myH1RawObject[['SA']]$SAsampWtLive <- as.integer(runif(nrow(myH1RawObject[['SA']]), min = 1, max= 200))
  # #View(myH1RawObject[['CL']])
  #
  # dataToPlot = myH1RawObject
  # year = 1965
  # quarter = NA
  # vesselFlag = "ZW"
  # #var = "species"
  # var = "Statrec"
  # #var = c("species", "gear", "Statrec")
  # catchCat = "Lan"
  # #spatialPlot = c("Bivariate","Points")
  # spatialPlot = "Points"
  # commercialVariable = "CLoffWeight"
  # samplingVariable = "SAsampWtLive"


  # STEP 0) VALIDATE INPUTS

  # check the parameters are valid before we do anything

  print("Validating input parameters")

  if (length(catchCat) == 3) {
    stop("You must provide a Catch Category")
  } else if (length(catchCat) == 2) {
    stop("Only one Catch Category can be provided")
  }

  if (length(vesselFlag) > 1) {
    stop("Only one vessel flag country can be provided")
  }

  if (length(var) > 1 || var == "Statrec" ) {
    if (length(commercialVariable) > 1 ||
        length(samplingVariable) > 1) {
      stop("You must provide  commercialVariable and  samplingVariable")
    }
  }

  if (length(var) == 1 && var == "Statrec" && length(spatialPlot) > 1) {
    stop("You must choose a Spatial Plot")
  }

  if (length(var) == 1 && var == "Statrec" && is.na(quarter) == FALSE) {
    stop("Unused argument, no quarters avialable for var Statrec")
  }

  if (length(var) == 1 && var == "Statrec" && is.na(year) == TRUE) {
    stop("You must provide  the year")
  }

  if (length(var) == 1 && var == "gear" && is.na(quarter) == FALSE) {
    stop("Unused argument, no quarters available for  var gear")
  }

  if (length(spatialPlot)==1 && !spatialPlot %in% c("Bivariate","Points")){
    stop(paste0("Invalid spatialPlot value:",spatialPlot))
  }

  if (length(catchCat)==1 && !catchCat %in% c("Lan","Dis","Catch")){
    stop(paste0("Invalid catchCat value:",catchCat))
  }

  if (length(var)==1 && !var %in% c("species", "gear", "Statrec")){
    stop(paste0("Invalid var value:",var))
  }

  # Check the input data is valid
  RDBEScore::validateRDBESDataObject(dataToPlot)


  # STEP 1) PREPARE THE DATA

  print("Preparing data")

  # 1a) Get landings data

  LD <- dataToPlot[["CL"]] %>%
    dplyr::select(
      CLlanCou:CLmonth,
      CLstatRect,
      CLspecCode:CLcatchCat,
      CLmetier6,
      CLoffWeight:CLsciWeight
    )
  LD$CLGear <- substr(LD$CLmetier6, 0, 3)

  # 1b) Get sample data

  # We'll convert the CS data into a RDBESEstObject to
  # make it easier to handle here

  hierarchiesInData <- unique(dataToPlot[["DE"]]$DEhierarchy)
  if (length(hierarchiesInData)!=1) {
    stop("This function will only work if there is a single hierarchy in dataToPlot")
  }
  datatoPlot_EstOb <- RDBEScore::createRDBESEstObject(dataToPlot, hierarchiesInData)

  # Check the RDBESEstObject is valid
  RDBEScore::validateRDBESEstObject(datatoPlot_EstOb)

  SA <- datatoPlot_EstOb

  # Join to VD to get Vessel flag country
  SA <- dplyr::left_join(SA, dataToPlot[["VD"]], by = "VDid")

  # Get the year and quarter of the sample from FO
  SA$SAyear <-
    as.integer(format(as.Date(SA$FOendDate, format = "%Y-%m-%d"), "%Y"))
  SA$SAquar <-
    as.integer(lubridate::quarter(as.Date(SA$FOendDate, format = "%Y-%m-%d")))
  SA$SAmonth <-
    as.integer(lubridate::month(as.Date(SA$FOendDate, format = "%Y-%m-%d")))

  # Get only necessary columns from the sample data

  # Find the first SA columns - we are only dealing with the top level SA data
  # TODO - should we be able to plot sub-samples as well?
  colsToCheck <-
    names(datatoPlot_EstOb)[grep("^su.table$",names(datatoPlot_EstOb))]
  correctCol <- NA
  suNumber <- NA
  for (myCol in colsToCheck){
    #myColValues <- unique(datatoPlot_EstOb[,..myCol])[[1]]
    myColValues <- unique(datatoPlot_EstOb[,myCol, with = FALSE])[[1]]
    myColValues <- myColValues[!is.na(myColValues)]
    if (myColValues == "SA"){
      correctCol <- myCol
      suNumber <- gsub("su","",correctCol)
      suNumber <- gsub("table","",suNumber)
      suNumber <- as.integer(suNumber)
      break
    }
  }
  if (is.na(correctCol)) {
    stop("Sample data could not be found - cannot continue")
  }

  # Rename the suXnumTotal and suXnumSamp columns to SAnumTotal and SAnumSamp
  SA <- SA %>% dplyr::rename("SAnumTotal" = paste0("su",suNumber,"numTotal"))
  SA <- SA %>% dplyr::rename("SAnumSamp" = paste0("su",suNumber,"numSamp"))

  # Get the columns we want
  SA <- SA %>%
    dplyr::select(
      c("SAmetier5", "SAmetier6", "SAgear", "SAtotalWtLive",
        "SAsampWtLive",
        "SAnumTotal","SAnumSamp",
        "SAtotalWtMes", "SAsampWtMes", "SAyear", "SAquar", "SAmonth",
        "SAcatchCat", "SAspeCode", "SAspeCodeFAO", "SAstatRect",
        "VDflgCtry",
        "SAid")
    )%>%
    dplyr::relocate(SAstatRect, SAyear, SAquar, SAmonth)

  # remove any duplicates (could be present because we have removed the FM
  # and BV data)
  if (length(which(duplicated(SA))) > 0) {
    SA <- SA[-which(duplicated(SA)), ]
  }

  # Remove any rows with SAid = NA, then get rid of the SAid column
  SA <- SA[!is.na(SA$SAid),]
  SA <- dplyr::select(SA,-SAid)

  # Ensure specode is an integer
  SA$SAspeCode <- as.integer(SA$SAspeCode)


  # STEP 2) FILTER THE DATA BASED ON THE INPUT PARAMETERS

  print("Filtering data")

  # Filter the data based on the function's input parameters
  if (is.na(year) == TRUE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      LD1 <- LD
      SA1 <- SA %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      LD1 <- LD %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
      SA1 <- SA %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      SA1 <- SA1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      LD1 <- LD %>% dplyr::filter(CLyear %in% year)
      SA1 <- SA %>% dplyr::filter(SAyear %in% year)
      SA1 <- SA1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      LD1 <- LD %>% dplyr::filter(CLyear %in% year)
      SA1 <- SA %>% dplyr::filter(SAyear %in% year)
      LD1 <- LD1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
      SA1 <- SA1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      SA1 <- SA1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(year) == TRUE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      LD1 <- LD %>% dplyr::filter(CLquar %in% quarter)
      SA1 <- SA %>% dplyr::filter(SAquar %in% quarter)
      SA1 <- SA1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      LD1 <- LD %>% dplyr::filter(CLquar %in% quarter)
      SA1 <- SA %>% dplyr::filter(SAquar %in% quarter)
      LD1 <- LD1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
      SA1 <- SA1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      SA1 <- SA1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      LD1 <- LD %>% dplyr::filter(CLyear %in% year)
      SA1 <- SA %>% dplyr::filter(SAyear %in% year)
      LD1 <- LD1 %>% dplyr::filter(CLquar %in% quarter)
      SA1 <- SA1 %>% dplyr::filter(SAquar %in% quarter)
      SA1 <- SA1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    } else {
      LD1 <- LD %>% dplyr::filter(CLyear %in% year)
      SA1 <- SA %>% dplyr::filter(SAyear %in% year)
      LD1 <- LD1 %>% dplyr::filter(CLquar %in% quarter)
      SA1 <- SA1 %>% dplyr::filter(SAquar %in% quarter)
      LD1 <- LD1 %>% dplyr::filter(CLvesFlagCou %in% vesselFlag)
      SA1 <- SA1 %>% dplyr::filter(VDflgCtry %in% vesselFlag)
      SA1 <- SA1 %>% dplyr::filter(SAcatchCat %in% catchCat)
    }
  }

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  # STEP 3) PREPARE THE PLOTS

  print("Preparing plots")

  plotsToPrint <- NA

  if (length(var) > 1) {
    ########################## TEMPORAL ########################
    plotsToPrint <- temporalPlot(landingsData = LD1,
                                sampleData = SA1,
                                flagLabel = flagLabel,
                                catchCat = catchCat,
                                commercialVariable = commercialVariable,
                                samplingVariable = samplingVariable)
  } else if (var == "species") {
    ###################### species #############################
    plotsToPrint <- speciesPlot(landingsData = LD1,
                sampleData = SA1,
                flagLabel = flagLabel,
                catchCat = catchCat)
  } else if (var == "gear") {
    ####################### gear ###############################
    plotsToPrint <- gearPlot(landingsData= LD1,
                             sampleData= SA1,
                             flagLabel = flagLabel,
                             catchCat = catchCat,
                             quarter = quarter)
  ################ spatial #####################
  } else if (var == "Statrec") {
    if (spatialPlot == "Bivariate") {
    ############## bivariate plot###########
      # TODO
      stop ("Bivariate plot not implemented yet")
    } else if (spatialPlot == "Points"){
    ############# points plot###############
      # TODO
      stop ("Points plot not implemented yet")
    } else {
      stop ("Spatial plot - invalid type")
    }
  } else {
    print(var)
    print(spatialPlot)
    stop ("Don't know what to do with these parameters...")
  }

  plotsToPrint
  # TODO - everything else :-)

}


#' Internal function to return a list of plots which compare the species in
#' landings to those in the sample data.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param flagLabel Text to use for vessel flag description
#' @param catchCat Catch category
#' @param (Optional) Number of species to plot.  Default is 10.
#'
#' @return
#'
#' @examples
speciesPlot <- function(landingsData, sampleData, flagLabel, catchCat, topN = 10){

  # for testing
  #landingsData <- LD1
  #sampleData <- SA1

  # Get the species names
  full_name <- RDBESvisualise::speciesNamesAndCodes
  full_name <- full_name[-which(duplicated(full_name$AphiaID)), ]

  # Landings data
  # Group by year, and year and quarter, and count the number of species
  df1 <- na.omit(
    landingsData %>% dplyr::group_by(CLyear) %>%
      dplyr::add_count(CLspecCode, name = "CLSpeCount") %>%
      dplyr::summarise(LandingCountYear = sum(CLSpeCount))
  ) %>%
    dplyr::mutate(totalSpeCountAll = sum(LandingCountYear))
  d1 <- na.omit(
    landingsData %>% dplyr::group_by(CLyear, CLquar, CLspecCode) %>%
      dplyr::add_count(CLspecCode, name = "CLSpeCount") %>%
      dplyr::summarise(LandingCount = sum(CLSpeCount))
  )
  d1_species <-
    dplyr::left_join(d1, full_name, by = c("CLspecCode" = "AphiaID"))

  d1_species <- dplyr::left_join(d1_species, df1, by = "CLyear") %>%
    dplyr::mutate(relativeValuesYear = LandingCount / LandingCountYear) %>%
    dplyr::mutate(relativeValuesAll = LandingCount / totalSpeCountAll) %>%
    dplyr::top_n(topN)

  # Sample data
  # Group by year, and year and quarter, and count the number of species

  # add df to calculate total species for year
  df2 <- na.omit(
    sampleData %>% dplyr::group_by(SAyear) %>%
      dplyr::add_count(SAspeCode, name = "SASpeCount") %>%
      dplyr::summarise(SamplingCountYear = sum(SASpeCount))
  ) %>%
    dplyr::mutate(totalSpeCountAll = sum(SamplingCountYear))

  d2 <- na.omit(
    sampleData %>% dplyr::group_by(SAyear, SAquar, SAspeCode) %>%
      dplyr::add_count(SAspeCode, name = "SASpeCount") %>%
      dplyr::summarise(SamplingCount = sum(SASpeCount))
  )
  d2_species <-
    dplyr::left_join(d2, full_name, by = c("SAspeCode" = "AphiaID"))

  d2_species <- dplyr::left_join(d2_species, df2, by = "SAyear") %>%
    dplyr::mutate(relSamplingYear = SamplingCount / SamplingCountYear) %>%
    dplyr::mutate(relSamplingAll = SamplingCount / totalSpeCountAll) %>%
    dplyr::top_n(topN)

  y <- unique(d1_species$CLyear)

  # Generate plots for the data

  all_plot <- htmltools::tagList()
  for (i in 1:length(y)) {
    t1 <- d1_species %>% dplyr::filter(CLyear == y[i])
    t2 <- d2_species %>% dplyr::filter(SAyear == y[i])
    # Landings plot
    p1 <- plotly::plot_ly(
      t1,
      x = ~ as.character(FAODescription),
      y = ~relativeValuesYear,
      color = ~ as.character(CLquar),
      type = "bar",
      showlegend = F
    ) %>%
      plotly::layout(
        title = paste0("Vessel Flag ",
                       flagLabel,
                       ": Top Landings Species in",
                       y[i]),
        yaxis = list(title = "Landings"),
        xaxis = list(categoryorder = "total descending"),
        barmode = "stack"
      )
    # sample data plot
    p2 <- plotly::plot_ly(
      t2,
      x = ~ as.character(FAODescription),
      y = ~relSamplingYear,
      color = ~ as.character(SAquar),
      type = "bar",
      showlegend = T
    ) %>%
      plotly::layout(
        title = paste0(
          "Vessel Flag ",
          flagLabel,
          " : Top Landings and Sampling (",
          catchCat,
          ") Species \nRelative Values per Plot in ",
          y[i]
        ),
        yaxis = list(title = "Sampling"),
        xaxis = list(categoryorder = "total descending"),
        barmode = "stack",
        legend = list(title = list(text = "<b> Quarter: </b>"))
      )


    all_plot[[i]] <- plotly::subplot(p1, p2, titleY = TRUE, nrows = 2)
  }
  all_plot
  #all_plot[[1]]
}


#' Internal function to return a list of plots which compare the relative
#' amount of the values of commercialVariable and samplingVariable by quarter
#' landings to those in the sample data.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param flagLabel Text to use for vessel flag description
#' @param catchCat Catch category
#' @param commercialVariable Variable from CL to plot
#' @param samplingVariable variable from SA to plot
#'
#' @return
#' @export
#'
#' @examples
temporalPlot <- function(landingsData, sampleData, flagLabel, catchCat, commercialVariable, samplingVariable){

  # for testing
  # landingsData <- LD1
  # sampleData <- SA1

    d1 <- na.omit(landingsData %>% dplyr::group_by(CLyear, CLquar) %>%
                    dplyr::summarize(CL = sum(!!rlang::sym(
                      commercialVariable
                    )))) %>%
      dplyr::mutate(relCL = CL / sum(CL))
    d2 <- na.omit(sampleData %>% dplyr::group_by(SAyear, SAquar) %>%
                    dplyr::summarize(SA = sum(!!rlang::sym(
                      samplingVariable
                    )))) %>%
      dplyr::mutate(relSA = SA / sum(SA))

    df <-
      dplyr::left_join(d1, d2, by = c("CLyear" = "SAyear", "CLquar" = "SAquar"))

    y <- unique(df$CLyear)


    all_plot <- htmltools::tagList()
    # all_plot<-list()
    for (i in 1:length(y)) {
      set <- df %>% dplyr::filter(CLyear == y[i])
      show_legend <- if (i == 1) {
        TRUE
      } else {
        FALSE
      }
      all_plot[[i]] <- plotly::plot_ly(
        set,
        x = ~CLquar,
        y = ~relCL,
        type = "bar",
        alpha = 0.7,
        name = "Landings",
        hovertemplate = paste(
          "%{yaxis.title.text}:  %{y}<br>",
          "%{xaxis.title.text}: %{x}<br>"
        ),
        showlegend = show_legend,
        marker = list(
          color = "rgb(168, 74, 50)",
          line = list(color = "rgb(8,48,107)", width = 1.5)
        )
      ) %>%
        plotly::add_trace(
          y = ~relSA,
          name = "Sampling",
          alpha = 0.7,
          showlegend = show_legend,
          marker = list(
            color = "rgb(158,202,225)",
            line = list(color = "rgb(15,48,107)", width = 1.5)
          )
        ) %>%
        plotly::layout(
          title = paste0(
            "Vessel Flag ",
            flagLabel,
            " | Landings: ",
            commercialVariable,
            " vs Sampling: ",
            samplingVariable,
            " - (",
            catchCat,
            ") in ",
            y[i]
          ),
          xaxis = list(title = "Quarter"),
          yaxis = list(title = "Relative Values")
        )
    }
    all_plot
    #all_plot[[i]]
}

#' Title
#'
#' @param landingsData
#' @param sampleData
#' @param flagLabel
#' @param catchCat
#' @param quarter
#'
#' @return
#' @export
#'
#' @examples
gearPlot <- function(landingsData, sampleData, flagLabel, catchCat, quarter){


  if (is.na(quarter) == FALSE) {
    df1 <- na.omit(
      landingsData %>% dplyr::group_by(CLyear, CLquar) %>%
        dplyr::add_count(CLGear, name = "CLGearCount") %>%
        dplyr::summarise(LandingsGearCountQuar = sum(CLGearCount))
    )

    d1 <- na.omit(
      landingsData %>% dplyr::group_by(CLyear, CLquar, CLGear) %>%
        dplyr::add_count(CLGear, name = "CLGearCount") %>%
        dplyr::summarise(LandingsGearCount = sum(CLGearCount))
    )

    d1 <- left_join(d1, df1, by = "CEyear", "CEquar") %>%
      mutate(relativeValuesL = LandingsGearCount / LandingsGearCountQuar)

    df2 <- na.omit(
      sampleData %>% dplyr::group_by(SAyear, SAquar) %>%
        dplyr::add_count(SAgear, name = "SAGearCount") %>%
        dplyr::summarise(SamplingGearCountQuar = sum(SAGearCount))
    )

    d2 <- na.omit(
      sampleData %>% dplyr::group_by(SAyear, SAquar, SAgear) %>%
        dplyr::add_count(SAgear, name = "SAGearCount") %>%
        dplyr::summarise(SamplingGearCount = sum(SAGearCount))
    )

    d2 <- left_join(d2, df2, by = "SAyear", "SAquar") %>%
      dplyr::mutate(relativeValuesS = SamplingGearCount / SamplingGearCountQuar)
  } else {
    df1 <- na.omit(
      landingsData %>% dplyr::group_by(CLyear) %>%
        dplyr::add_count(CLGear, name = "CLGearCount") %>%
        dplyr::summarise(totalGearYear = sum(CLGearCount))
    )

    d1 <- na.omit(
      landingsData %>% dplyr::group_by(CLyear, CLGear) %>%
        dplyr::add_count(CLGear, name = "CLGearCount") %>%
        dplyr::summarise(LandingsGearCount = sum(CLGearCount))
    )

    d1 <- dplyr::left_join(d1, df1, by = "CLyear") %>%
      dplyr::mutate(relativeValuesL = LandingsGearCount / totalGearYear)

    df2 <- na.omit(
      sampleData %>% dplyr::group_by(SAyear) %>%
        dplyr::add_count(SAgear, name = "SAGearCount") %>%
        dplyr::summarise(SamplingGearCountYear = sum(SAGearCount))
    )

    d2 <- na.omit(
      sampleData %>% dplyr::group_by(SAyear, SAgear) %>%
        dplyr::add_count(SAgear, name = "SAGearCount") %>%
        dplyr::summarise(SamplingGearCount = sum(SAGearCount))
    )

    d2 <- dplyr::left_join(d2, df2, by = "SAyear") %>%
      dplyr::mutate(relativeValuesS = SamplingGearCount / SamplingGearCountYear)
  }


  df <-
    dplyr::left_join(d1, d2, by = c("CLyear" = "SAyear", "CLGear" = "SAgear"))
  # df = df %>% select(-c(relativeValuesE, totalSamplingYear))
  y <- unique(df$CLyear)

  all_plot <- htmltools::tagList()

  for (i in 1:length(y)) {
    dd <- d1 %>% dplyr::filter(CLyear == y[i])
    dd <- dd[-1]
    ds <- d2 %>% dplyr::filter(SAyear == y[i])
    ds <- ds[-1]
    p1 <- plotly::plot_ly(
      dd,
      x = ~ as.character(CLGear),
      y = ~relativeValuesL,
      color = ~ as.character(CLGear),
      type = "bar",
      showlegend = F
    ) %>%
      plotly::layout(
        title = paste0("Vessel Flag ",
                       flagLabel,
                       " : Top Landings Gear - Relative Values per Plot \n in",
                       y[i]),
        yaxis = list(title = "Landings"),
        xaxis = list(categoryorder = "total descending"),
        barmode = "stack"
      )
    p2 <- plotly::plot_ly(
      ds,
      x = ~ as.character(SAgear),
      y = ~relativeValuesS,
      color = ~ as.character(SAgear),
      type = "bar",
      showlegend = F
    ) %>%
      plotly::layout(
        title = paste0(
          "Vessel Flag ", flagLabel,
          " : Top Landings and Sampling Gear (",
          catchCat,
          ")\n Relative Values per Plot in ",
          y[i]
        ),
        yaxis = list(title = "Sampling"),
        xaxis = list(categoryorder = "total descending"),
        barmode = "stack"
      )

    all_plot[[i]] <- plotly::subplot(p1, p2, titleY = TRUE, nrows = 2)
  }
  all_plot

}


pointsPlot <- function(landingsData, sampleData, flagLabel, catchCat, commercialVariable, samplingVariable ){

  # for testing
  #landingsData <- LD1
  #sampleData <- SA1

  d1 <- na.omit(landingsData %>% dplyr::group_by(CLyear, CLstatRect) %>%
                  dplyr::summarize(CL = sum(!!rlang::sym(
                    commercialVariable
                  ))))
  d2 <- na.omit(sampleData %>% dplyr::group_by(SAyear, SAstatRect) %>%
                  dplyr::summarize(SA = sum(!!rlang::sym(
                    samplingVariable
                  ))))

  df <-
    dplyr::left_join(d1,
              d2,
              by = c("CLyear" = "SAyear", "CLstatRect" = "SAstatRect"))

  y <- unique(df$CLyear)

  all_plot <- htmltools::tagList()

  for (i in 1:length(y)) {

    dd <- df %>% dplyr::filter(CLyear == y[i])

    # get ices shp
    #ices_rects <- sf::read_sf("Data/Maps/shapefiles/ICESrect.shp")
    ices_rects <- RDBESvisualise::icesRects

    ices_rects %<>%
      dplyr::left_join(dd, by = c("ICESNAME" = "CLstatRect"))

    # get extent of plot

    No_NA <- ices_rects[ices_rects$CL != "NA", ]
    xlim1 <- st_bbox(No_NA)[1]
    ylim2 <- st_bbox(No_NA)[2]
    xlim3 <- st_bbox(No_NA)[3]
    ylim4 <- st_bbox(No_NA)[4]

    # define number of classes
    no_classes <- 6

    # extract quantiles
    quantiles <- ices_rects %>%
      pull(CL) %>%
      quantile(
        probs = seq(0, 1, length.out = no_classes + 1),
        na.rm = TRUE
      ) %>%
      as.vector() # to remove names of quantiles, so idx below is numeric
    quantiles <- round(quantiles, 0)

    # create custom labels
    labels <- purrr::imap_chr(quantiles, function(., idx) {
      return(paste0(
        round(quantiles[idx], 10),
        " - ",
        round(quantiles[idx + 1], 10)
      ))
    })

    # remove last label that includes NA
    labels <- labels[1:length(labels) - 1]

    # create new variable with quantiles - landings
    ices_rects %<>%
      dplyr::mutate(mean_quantiles_land = cut(
        CL,
        breaks = quantiles,
        labels = labels,
        include.lowest = T
      ))


    # create point on surface
    points <- st_coordinates(st_point_on_surface(ices_rects))
    points <- as.data.frame(points)
    points$SA <- ices_rects$SA

    # plot univariate map with points
    gg <- ggplot2::ggplot(data = ices_rects) +
      ggplot2::geom_polygon(
        data = RDBESvisualise::shoreline,
        ggplot2::aes(x = long, y = lat, group = group),
        color = "white",
        fill = "gray",
        show.legend = FALSE
      ) +
      ggiraph::geom_sf_interactive(
        ggplot2::aes(
          fill = mean_quantiles_land,
          tooltip = paste("Landings:", CL)
        ),
        color = "white",
        size = 0.1
      ) +
      ggiraph::scale_fill_brewer_interactive(
        type = "seq",
        palette = "RdYlBu",
        name = "Landings Variable",
        direction = -1,
        guide = ggplot2::guide_legend(
          keyheight = ggplot2::unit(5, units = "mm"),
          title.position = "top",
          reverse = T
        )
      ) +
      ggiraph::geom_point_interactive(
        data = points,
        ggplot2::aes(
          x = X,
          y = Y,
          size = SA,
          tooltip = paste("Sampling: ", SA)
        ),
        # color = "bisque4",
        shape = 1,
        color = "black",
        alpha = 0.5
      ) +
      ggplot2::coord_sf(
        xlim = c(xlim1, xlim3),
        ylim = c(ylim2, ylim4)
      ) +
      # add titles
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = paste0("Vessel Flag:  ", flagLabel),
        subtitle = paste0(
          "Sampling - ",
          catchCat, " (",
          samplingVariable,
          ")  vs Landings (",
          commercialVariable,
          ") in ",
          y[i]
        ),
        size = "Sampling Variable"
      ) +
      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(color = "gray"),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank()
      )



    x <- ggiraph::girafe(ggobj = gg, width_svg = 6, height_svg = 6)
    x <- ggiraph::girafe_options(
      x,
      ggiraph::opts_zoom(min = .4, max = 2)
    )

    all_plot[[i]] <- x
  }

all_plot

}


