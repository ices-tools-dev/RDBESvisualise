#' Provides graphical outputs to compare the relative
#' amount of the values of landingsVariable and samplingVariable by quarter
#' landings to those in the sample data.
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4.
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param landingsVariable Landings variable to be assessed
#' @param effortVariable Effort variable to be assessed
#' @param samplingVariable Sampling Variable to be assessed
#' @param catchCat Sampling catch category - "Lan", "Dis",or "Catch"
#' @param includeLandings (Optional) Set to TRUE to include landings in the
#' plot - default is TRUE.
#' @param includeEffort (Optional) Set to TRUE to include effort in the
#' plot - default is TRUE.
#' @param includeSamples (Optional) Set to TRUE to include samples in the
#' plot - default is TRUE.
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
#'   rdbesExtractPath = "./tests/testthat/h1_v_1_19_13"
#' )
#'
#' myYear <- 1965
#' myvesselFlag <- "ZW"
#'
#' myPlots <- coverageLandingsTemporal(
#'   dataToPlot = myH1RawObject,
#'   year = myYear,
#'   vesselFlag = myvesselFlag,
#'   catchCat = "Lan",
#'   landingsVariable = "CLoffWeight",
#'   samplingVariable = "SAsampWtLive"
#' )
#'
#' myPlots[1]
#' }
coverageTemporal <- function(dataToPlot,
                             year = NA,
                             quarter = NA,
                             vesselFlag = NA,
                             landingsVariable = c(
                               "CLoffWeight",
                               "CLsciWeight"
                             ),
                             effortVariable = c(
                               "CEnumFracTrips",
                               "CEnumDomTrip"
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
                             includeLandings = TRUE,
                             includeEffort = TRUE,
                             includeSamples = TRUE,
                             verbose = FALSE) {
  # STEP 0) VALIDATE INPUTS

  # check the parameters are valid before we do anything
  if (verbose) {
    print("Validating input parameters")
  }

  validateCoverageParameters(year = year,
                             quarter = quarter,
                             vesselFlag = vesselFlag,
                             landingsVariable = landingsVariable,
                             effortVariable = effortVariable,
                             samplingVariable = samplingVariable,
                             groupingVariable = NA,
                             catchCat = catchCat,
                             includeLandings = includeLandings,
                             includeEffort = includeEffort,
                             includeSamples = includeSamples,
                             topN = NA)


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


  # STEP 3) Plot the data

  if (verbose) {
    print("Preparing plots")
  }

  plotsToPrint <- temporalPlot(
    landingsData = ld1,
    effortData = ef1,
    sampleData = sa1,
    vesselFlag = vesselFlag,
    catchCat = catchCat,
    landingsVariable = landingsVariable,
    effortVariable = effortVariable,
    samplingVariable = samplingVariable
  )

  plotsToPrint
}

#' Internal function to return a list of plots which compare the relative
#' amount of the values of landingVariable, effortVariable and samplingVariable
#' by quarter.
#'
#' @param landingsData Landings data
#' @param effortData Effort data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param landingsVariable Variable from CL to plot
#' @param landingsVariable Variable from CE to plot
#' @param samplingVariable variable from SA to plot
#'
#' @return A tagList of plotly plots
#'
temporalPlot <- function(landingsData = NA,
                         effortData = NA,
                         sampleData = NA,
                         vesselFlag,
                         catchCat,
                         landingsVariable,
                         effortVariable,
                         samplingVariable) {
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

  # Landings
  if (landings) {
    d1 <- na.omit(landingsData %>%
      dplyr::group_by(CLyear, CLquar) %>%
      dplyr::summarize(CL = sum(!!rlang::sym(
        landingsVariable
      )))) %>%
      dplyr::mutate(relCL = CL / sum(CL))
  }

  # Samples
  if (samples) {
    d2 <- na.omit(sampleData %>%
      dplyr::group_by(SAyear, SAquar) %>%
      dplyr::summarize(sa = sum(!!rlang::sym(
        samplingVariable
      )))) %>%
      dplyr::mutate(relSA = sa / sum(sa))
  }

  # Effort
  if (effort) {
    d3 <- na.omit(effortData %>%
      dplyr::group_by(CEyear, CEquar) %>%
      dplyr::summarize(CE = sum(!!rlang::sym(
        effortVariable
      )))) %>%
      dplyr::mutate(relCE = CE / sum(CE))
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
    show_legend <- if (i == 1) {
      TRUE
    } else {
      FALSE
    }

    plotTitle <- paste0("Vessel Flag ", flagLabel)

    # Create an empty data set to use as a base for our plots
    emptyData <- data.frame(quarters = c(1, 2, 3, 4), values = c(NA, NA, NA, NA))
    p0 <- plotly::plot_ly(
      emptyData,
      x = ~quarters,
      y = ~values,
      type = "bar",
      alpha = 0.7
    )
    # p0 <- plotly::plotly_empty()

    # Landings
    if (landings) {
      dd <- d1 %>% dplyr::filter(CLyear == y[i])
      dd <- dd[-1]

      p0 <- p0 %>%
        plotly::add_trace(
          data = dd,
          x = ~CLquar,
          y = ~relCL,
          type = "bar",
          alpha = 0.7,
          name = "Landings",
          showlegend = show_legend,
          marker = list(
            color = "rgb(158,202,225)",
            line = list(color = "rgb(15,48,107)", width = 1.5)
          )
        )

      plotTitle <- paste0(plotTitle, " | Landings: ", landingsVariable)
    }

    # Effort
    if (effort) {
      dde <- d3 %>% dplyr::filter(CEyear == y[i])
      dde <- dde[-1]

      p0 <- p0 %>%
        plotly::add_trace(
          data = dde,
          x = ~CEquar,
          y = ~relCE,
          type = "bar",
          alpha = 0.7,
          name = "Effort",
          showlegend = show_legend,
          marker = list(
            color = "rgb(79, 164, 64)",
            line = list(color = "rgb(8,48,107)", width = 1.5)
          )
        )

      plotTitle <- paste0(plotTitle, " | Effort: ", effortVariable)
    }

    # Samples
    if (samples) {
      ds <- d2 %>% dplyr::filter(SAyear == y[i])
      ds <- ds[-1]

      p0 <- p0 %>%
        plotly::add_trace(
          data = ds,
          x = ~SAquar,
          y = ~relSA,
          type = "bar",
          alpha = 0.7,
          name = "Samples",
          showlegend = show_legend,
          marker = list(
            color = "rgb(168, 74, 50)",
            line = list(color = "rgb(8,48,107)", width = 1.5)
          )
        )

      plotTitle <- paste0(
        plotTitle,
        " | Sampling: ",
        samplingVariable,
        " (",
        catchCat,
        ") "
      )
    }

    plotTitle <- paste0(plotTitle, " in ", y[i])

    # Add a title
    p0 <- p0 %>%
      plotly::layout(
        title = list(text = plotTitle, font = list(size = 12)),
        xaxis = list(title = "Quarter"),
        yaxis = list(title = "Relative Values")
      )
    all_plot[[i]] <- p0
  }
  all_plot
}
