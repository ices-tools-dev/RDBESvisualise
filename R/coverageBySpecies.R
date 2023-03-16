#' Provides graphical outputs to compare the species in
#' landings to those in the sample data.
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param landingsVariable Landings variable to be assessed
#' @param samplingVariable Sampling Variable to be assessed
#' @param catchCat Sampling catch category - "Lan", "Dis",or "Catch"
#' @param includeLandings (Optional) Set to TRUE to include landings in the
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
#' myPlots <- coverageLandingsByGear(
#'   dataToPlot = myH1RawObject,
#'   year = myYear,
#'   vesselFlag = myvesselFlag,
#'   catchCat = "Lan"
#' )
#'
#' myPlots[1]
#' }
coverageBySpecies <- function(dataToPlot,
                              year = NA,
                              vesselFlag = NA,
                              landingsVariable = c(
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
                              includeLandings = TRUE,
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

  if (includeLandings && length(landingsVariable) > 1) {
    stop("You must provide landingsVariable if you want to include landings data")
  }

  if (includeSamples && length(samplingVariable) > 1) {
    stop("You must provide samplingVariable if you want to include sample data")
  }

  if (includeLandings &&
    length(landingsVariable) == 1 &&
    !landingsVariable %in% RDBESvisualise::allowedLandingsVariable) {
    stop(paste0("Invalid landingsVariable value:", landingsVariable))
  }

  if (includeSamples &&
    length(samplingVariable) == 1 &&
    !samplingVariable %in% RDBESvisualise::allowedSamplingVariable) {
    stop(paste0("Invalid samplingVariable value:", samplingVariable))
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
      quarter = NA,
      vesselFlag = vesselFlag,
      verbose = verbose
    )
  } else {
    ld1 <- NA
  }

  # Samples
  if (includeSamples) {
    sa <- preprocessSampleDataForCoverage(dataToPlot, verbose = verbose)
    sa1 <- filterSampleDataForCoverage(sa,
      year = year,
      quarter = NA,
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

  plotsToPrint <- speciesPlot(
    landingsData = ld1,
    sampleData = sa1,
    vesselFlag = vesselFlag,
    catchCat = catchCat,
    landingsVariable = landingsVariable,
    samplingVariable = samplingVariable
  )

  plotsToPrint
}

#' Internal function to return a list of plots which compare the species in
#' landings to those in the sample data.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param landingsVariable Variable from CL to plot
#' @param samplingVariable variable from SA to plot
#' @param topN (Optional) The number of species to include - default is 10
#'
#' @return A tagList of plotly plots
#'
speciesPlot <- function(landingsData = NA,
                        sampleData = NA,
                        vesselFlag,
                        catchCat,
                        landingsVariable,
                        samplingVariable,
                        topN = 10) {
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
  }
  if (length(sampleData) == 1 && is.na(sampleData)) {
    samples <- FALSE
  } else {
    samples <- TRUE
  }

  # Get the species names
  full_name <- RDBESvisualise::wormsSpecies
  full_name <- dplyr::distinct(full_name, Key, .keep_all = TRUE)


  if (landings) {
    # Landings data

    # Sum landingsVariable by year
    df1 <- na.omit(
      landingsData %>%
        dplyr::group_by(CLyear) %>%
        dplyr::summarize(CLSumForYear = sum(!!rlang::sym(landingsVariable))) %>%
        dplyr::mutate(CLSumTotal = sum(CLSumForYear))
    )

    # Sum landingsVariable by year, quarter, and species
    d1 <- na.omit(
      landingsData %>%
        dplyr::group_by(CLyear, CLquar, CLspecCode) %>%
        dplyr::summarize(CLSpeSum = sum(!!rlang::sym(landingsVariable)))
    )

    d1Species <-
      dplyr::left_join(d1, full_name, by = c("CLspecCode" = "Key"))

    # Check for unmatched species codes
    if (nrow(d1Species[is.na(d1Species$Description), c("CLspecCode")]) > 0) {
      unmatchedSpeciesd1 <- paste(
        unique(d1Species[is.na(d1Species$Description), c("CLspecCode")]),
        collapse = ", "
      )
      warning(paste0(
        "Not all values of CLspecCode matched to a species ",
        " record. The following unmatched values will not be plotted: ",
        unmatchedSpeciesd1
      ))
      d1Species <- d1Species[!is.na(d1Species$Description), ]
    }

    # Get relative values
    d1SpeciesRelative <- dplyr::left_join(d1Species, df1, by = "CLyear") %>%
      dplyr::mutate(relativeValuesYear = CLSpeSum / CLSumForYear)

    # Get topN species based on their total value of landingsVariable
    topNSpecies_l <- d1SpeciesRelative %>%
      dplyr::group_by(CLspecCode) %>%
      dplyr::summarize(CLtotalSpeSum = sum(CLSpeSum)) %>%
      dplyr::slice_max(order_by = CLtotalSpeSum, n = topN)

    # Filter relative values to topN species
    d1SpeciesRelative <- d1SpeciesRelative[
      d1SpeciesRelative$CLspecCode %in% topNSpecies_l$CLspecCode,
    ]
  }

  if (samples) {
    # Sample data

    # Sum samplingVariable by year
    df2 <- na.omit(
      sampleData %>%
        dplyr::group_by(SAyear) %>%
        dplyr::summarize(SASumForYear = sum(!!rlang::sym(samplingVariable))) %>%
        dplyr::mutate(SASumTotal = sum(SASumForYear))
    )

    # Sum samplingVariable by year, quarter, and species
    d2 <- na.omit(
      sampleData %>%
        dplyr::group_by(SAyear, SAquar, SAspeCode) %>%
        dplyr::summarize(SASpeSum = sum(!!rlang::sym(samplingVariable)))
    )

    d2Species <-
      dplyr::left_join(d2, full_name, by = c("SAspeCode" = "Key"))

    # Check for unmatched species codes
    if (nrow(d2Species[is.na(d2Species$Description), c("SAspeCode")]) > 0) {
      unmatchedSpeciesd2 <- paste(
        unique(d2Species[is.na(d2Species$Description), c("SAspeCode")]),
        collapse = ", "
      )
      warning(paste0(
        "Not all values of SAspeCode matched to a species ",
        " record. The following unmatched values will not be plotted: ",
        unmatchedSpeciesd2
      ))
      d2Species <- d2Species[!is.na(d2Species$Description), ]
    }

    # Get relative values
    d2SpeciesRelative <- dplyr::left_join(d2Species, df2, by = "SAyear") %>%
      dplyr::mutate(relativeValuesYear = SASpeSum / SASumForYear)

    # Get topN species based on their total value of landingsVariable
    topNSpecies_l <- d2SpeciesRelative %>%
      dplyr::group_by(SAspeCode) %>%
      dplyr::summarize(SAtotalSpeSum = sum(SASpeSum)) %>%
      dplyr::slice_max(order_by = SAtotalSpeSum, n = topN)

    # Filter relative values to topN species
    d2SpeciesRelative <- d2SpeciesRelative[
      d2SpeciesRelative$SAspeCode %in% topNSpecies_l$SAspeCode,
    ]
  }

  # Get the years we want plot
  y <- c()
  if (landings) {
    y <- c(y, unique(d1$CLyear))
  }
  if (samples) {
    y <- c(y, unique(d2$SAyear))
  }
  y <- sort(unique(y))

  # Generate plots for the data

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {
    if (landings) {
      t1 <- d1SpeciesRelative %>% dplyr::filter(CLyear == y[i])
      # Landings plot
      p1 <- plotly::plot_ly(
        t1,
        x = ~ as.character(Description),
        y = ~relativeValuesYear,
        color = ~ as.character(CLquar),
        type = "bar",
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = paste0(
            "Vessel Flag ",
            flagLabel,
            ": Top ", topN, " Landings Species in",
            y[i]
          ),
          yaxis = list(
            title = paste0("Relative ", landingsVariable),
            titlefont = list(size = 12)
          ),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack"
        )
    } else {
      p1 <- plotly::plotly_empty(type = "bar")
    }

    if (samples) {
      t2 <- d2SpeciesRelative %>% dplyr::filter(SAyear == y[i])
      # sample data plot
      p2 <- plotly::plot_ly(
        t2,
        x = ~ as.character(Description),
        y = ~relativeValuesYear,
        color = ~ as.character(SAquar),
        type = "bar",
        showlegend = TRUE
      ) %>%
        plotly::layout(
          title = paste0(
            "Vessel Flag ",
            flagLabel,
            " : Top ", topN, " Landings and Sampling (",
            catchCat,
            ")\n species in ",
            y[i]
          ),
          yaxis = list(
            title = paste0("Relative ", samplingVariable),
            titlefont = list(size = 12)
          ),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack",
          legend = list(title = list(text = "<b> Quarter: </b>"))
        )
    } else {
      p2 <- plotly::plotly_empty(type = "bar")
    }

    all_plot[[i]] <- plotly::subplot(p1, p2, titleY = TRUE, nrows = 2)
  }
  all_plot
}
