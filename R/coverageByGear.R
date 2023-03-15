#' Provides graphical outputs to compare the fishing gears
#' in effort to those in the sample data.
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4.
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Sampling catch category - "Lan", "Dis",or "Catch"
#' @param includeLandings (Optional) Set to TRUE to include landings.
#' Default is TRUE
#' #' @param includeEffort (Optional) Set to TRUE to include effort
#' Default is TRUE
#' #' @param includeSamples (Optional) Set to TRUE to include samples.
#' Default is TRUE
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A tagList of plotly plots
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <- RDBEScore::createRDBESDataObject(
#'   rdbesExtractPath = "../tests/testthat/h1_v_1_19_13"
#' )
#'
#' myYear <- 1965
#' myvesselFlag <- "ZW"
#'
#' myPlots <- coverageLandingsByGear(
#'   dataToPlot = myH1RawObject,
#'   year = myYear,
#'   vesselFlag = myvesselFlag,
#'   catchCat = "Lan"
#' )
#'
#' myPlots[1]
#' }
coverageByGear <- function(dataToPlot,
                           year = NA,
                           quarter = NA,
                           vesselFlag = NA,
                           catchCat = c(
                             "Lan",
                             "Dis",
                             "Catch"
                           ),
                           includeLandings = TRUE,
                           includeEffort = TRUE,
                           includeSamples = TRUE,
                           verbose = FALSE) {
  # STEP 0) VALIDATE INPUTS

  # check the parameters are valid before we do anything
  if (verbose) {
    print("Validating input parameters")
  }

  if (length(catchCat) == 3) {
    stop("You must provide a Catch Category")
  } else if (length(catchCat) == 2) {
    stop("Only one Catch Category can be provided")
  }

  if (length(vesselFlag) > 1) {
    stop("Only one vessel flag country can be provided")
  }

  if (length(catchCat) == 1 && !catchCat %in% c("Lan", "Dis", "Catch")) {
    stop(paste0("Invalid catchCat value:", catchCat))
  }

  # Check the input data is valid
  RDBEScore::validateRDBESDataObject(dataToPlot, verbose = verbose)

  # STEP 1) PREPARE AND FILTER THE DATA

  # Landings
  if (includeLandings) {
    ld <- preprocessLandingsDataForCoverage(dataToPlot, verbose = verbose)
    ld1 <- filterLandingsDataForCoverage(ld,
      year = year,
      quarter = quarter,
      vesselFlag = vesselFlag,
      verbose = verbose
    )
  } else {
    ld1 <- NA
  }

  # Effort
  if (includeEffort) {
    ef <- preprocessEffortDataForCoverage(dataToPlot, verbose = verbose)
    ef1 <- filterEffortDataForCoverage(ef,
      year = year,
      quarter = quarter,
      vesselFlag = vesselFlag,
      verbose = verbose
    )
  } else {
    ef1 <- NA
  }

  # Samples
  if (includeSamples) {
    sa <- preprocessSampleDataForCoverage(dataToPlot, verbose = verbose)
    sa1 <- filterSampleDataForCoverage(sa,
      year = year,
      quarter = quarter,
      vesselFlag = vesselFlag,
      catchCat = catchCat,
      verbose = verbose
    )
  } else {
    sa1 <- NA
  }


  # STEP 2) Plot the data

  if (verbose) {
    print("Preparing plots")
  }

  plotsToPrint <- gearPlot(
    landingsData = ld1,
    effortData = ef1,
    sampleData = sa1,
    vesselFlag = vesselFlag,
    catchCat = catchCat,
    quarter = quarter
  )

  plotsToPrint
}

#' Internal function to return a list of plots which compare the fishing gears
#' in landings, effort, and sample data.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param quarter Quarter of year
#'
#' @return A tagList of plotly plots
#'
gearPlot <- function(landingsData = NA,
                     effortData = NA,
                     sampleData = NA,
                     vesselFlag,
                     catchCat,
                     quarter) {
  # see what data we've been given
  if (length(landingsData) == 1 && is.na(landingsData)) {
    landings <- FALSE
  } else {
    landings <- TRUE
  }
  if (length(effortData) == 1 && is.na(effortData)) {
    effort <- FALSE
  } else {
    effort <- TRUE
  }
  if (length(sampleData) == 1 && is.na(sampleData)) {
    samples <- FALSE
  } else {
    samples <- TRUE
  }


  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }


  if (is.na(quarter) == FALSE) {
    # Landings
    if (landings) {
      df1 <- na.omit(
        landingsData %>%
          dplyr::group_by(CLyear, CLquar) %>%
          dplyr::add_count(CLGear, name = "CLGearCount") %>%
          dplyr::summarise(LandingsGearCountQuar = sum(CLGearCount))
      )

      d1 <- na.omit(
        landingsData %>%
          dplyr::group_by(CLyear, CLquar, CLGear) %>%
          dplyr::add_count(CLGear, name = "CLGearCount") %>%
          dplyr::summarise(LandingsGearCount = sum(CLGearCount))
      )

      d1 <- dplyr::left_join(d1, df1, by = "CEyear", "CEquar") %>%
        dplyr::mutate(
          relativeValuesL =
            LandingsGearCount / LandingsGearCountQuar
        )
    }

    # Samples
    if (samples) {
      df2 <- na.omit(
        sampleData %>%
          dplyr::group_by(SAyear, SAquar) %>%
          dplyr::add_count(SAgear, name = "SAGearCount") %>%
          dplyr::summarise(SamplingGearCountQuar = sum(SAGearCount))
      )

      d2 <- na.omit(
        sampleData %>%
          dplyr::group_by(SAyear, SAquar, SAgear) %>%
          dplyr::add_count(SAgear, name = "SAGearCount") %>%
          dplyr::summarise(SamplingGearCount = sum(SAGearCount))
      )

      d2 <- dplyr::left_join(d2, df2, by = "SAyear", "SAquar") %>%
        dplyr::mutate(
          relativeValuesS =
            SamplingGearCount / SamplingGearCountQuar
        )
    }


    # Effort
    if (effort) {
      df3 <- na.omit(
        effortData %>%
          dplyr::group_by(CEyear, CEquar) %>%
          dplyr::add_count(CEGear, name = "CEGearCount") %>%
          dplyr::summarise(EffortGearCountQuar = sum(CEGearCount))
      )

      d3 <- na.omit(
        effortData %>%
          dplyr::group_by(CEyear, CEquar, CEGear) %>%
          dplyr::add_count(CEGear, name = "CEGearCount") %>%
          dplyr::summarise(EffortGearCount = sum(CEGearCount))
      )

      d3 <- dplyr::left_join(d3, df3, by = "CEyear", "CEquar") %>%
        dplyr::mutate(
          relativeValuesE =
            EffortGearCount / EffortsGearCountQuar
        )
    }
  } else {
    # Landings
    if (landings) {
      df1 <- na.omit(
        landingsData %>%
          dplyr::group_by(CLyear) %>%
          dplyr::add_count(CLGear, name = "CLGearCount") %>%
          dplyr::summarise(totalGearYear = sum(CLGearCount))
      )

      d1 <- na.omit(
        landingsData %>%
          dplyr::group_by(CLyear, CLGear) %>%
          dplyr::add_count(CLGear, name = "CLGearCount") %>%
          dplyr::summarise(LandingsGearCount = sum(CLGearCount))
      )

      d1 <- dplyr::left_join(d1, df1, by = "CLyear") %>%
        dplyr::mutate(relativeValuesL = LandingsGearCount / totalGearYear)
    }

    # Samples
    if (samples) {
      df2 <- na.omit(
        sampleData %>%
          dplyr::group_by(SAyear) %>%
          dplyr::add_count(SAgear, name = "SAGearCount") %>%
          dplyr::summarise(SamplingGearCountYear = sum(SAGearCount))
      )

      d2 <- na.omit(
        sampleData %>%
          dplyr::group_by(SAyear, SAgear) %>%
          dplyr::add_count(SAgear, name = "SAGearCount") %>%
          dplyr::summarise(SamplingGearCount = sum(SAGearCount))
      )

      d2 <- dplyr::left_join(d2, df2, by = "SAyear") %>%
        dplyr::mutate(
          relativeValuesS =
            SamplingGearCount / SamplingGearCountYear
        )
    }

    # Effort
    if (effort) {
      df3 <- na.omit(
        effortData %>%
          dplyr::group_by(CEyear) %>%
          dplyr::add_count(CEGear, name = "CEGearCount") %>%
          dplyr::summarise(totalGearYear = sum(CEGearCount))
      )

      d3 <- na.omit(
        effortData %>%
          dplyr::group_by(CEyear, CEGear) %>%
          dplyr::add_count(CEGear, name = "CEGearCount") %>%
          dplyr::summarise(EffortGearCount = sum(CEGearCount))
      )

      d3 <- dplyr::left_join(d3, df3, by = "CEyear") %>%
        dplyr::mutate(relativeValuesE = EffortGearCount / totalGearYear)
    }
  }


  # Get the years we want plot
  y <- c()
  if (landings) {
    y <- c(y, unique(d1$CLyear))
  }
  if (samples) {
    y <- c(y, unique(d2$SAyear))
  }
  if (effort) {
    y <- c(y, unique(d3$CEyear))
  }
  y <- sort(unique(y))


  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {
    # Landings
    if (landings) {
      dd <- d1 %>% dplyr::filter(CLyear == y[i])
      dd <- dd[-1]
      p1 <- plotly::plot_ly(
        dd,
        x = ~ as.character(CLGear),
        y = ~relativeValuesL,
        color = ~ as.character(CLGear),
        type = "bar",
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = paste0(
            "Vessel Flag ",
            flagLabel,
            " : Top Gear - Relative Values per Plot \n in",
            y[i]
          ),
          yaxis = list(title = "Landings"),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack"
        )
    } else {
      p1 <- plotly::plotly_empty(type = "bar")
    }

    # Samples
    if (samples) {
      ds <- d2 %>% dplyr::filter(SAyear == y[i])
      ds <- ds[-1]
      p2 <- plotly::plot_ly(
        ds,
        x = ~ as.character(SAgear),
        y = ~relativeValuesS,
        color = ~ as.character(SAgear),
        type = "bar",
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = paste0(
            "Vessel Flag ", flagLabel,
            " : Top Gear (",
            catchCat,
            ")\n Relative Values per Plot in ",
            y[i]
          ),
          yaxis = list(title = "Sampling"),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack"
        )
    } else {
      p2 <- plotly::plotly_empty(type = "bar")
    }

    # Effort
    if (effort) {
      dde <- d3 %>% dplyr::filter(CEyear == y[i])
      dde <- dde[-1]
      p3 <- plotly::plot_ly(
        dde,
        x = ~ as.character(CEGear),
        y = ~relativeValuesE,
        color = ~ as.character(CEGear),
        type = "bar",
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = paste0(
            "Vessel Flag ",
            flagLabel,
            " : Top Gear - Relative Values per Plot \n in ",
            y[i]
          ),
          yaxis = list(title = "Effort"),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack"
        )
    } else {
      p3 <- plotly::plotly_empty(type = "bar")
    }


    all_plot[[i]] <- plotly::subplot(p1, p3, p2, titleY = TRUE, nrows = 3)
  }
  all_plot
}
