#' Provides graphical outputs which compare the statistical
#' rectangle where landings/effort occurred to the statistical rectangles where
#' sampling occurred. The relative amounts of landings/samples are
#' shown by i) coloured rectangles for landings, and ii) circles for
#' sampling.
#'
#'
#' @param dataToPlot RDBES data to be plotted (as an RDBESDataObject)
#' @param year Year to be assessed e.g 2021
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param commercialVariable Landings variable to be assessed
#' @param samplingVariable Sampling Variable to be assessed
#' @param catchCat Sampling catch category - "Lan", "Dis",or "Catch"
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A tagList of ggplot2 plots
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  myH1RawObject <- RDBEScore::createRDBESDataObject(input = "tests/testthat/h1_v_20250211")
#'
#'  myPlots <- coverageSpatial(
#'    dataToPlot = myH1RawObject,
#'    year = 1965,
#'    vesselFlag = "ZW",
#'    catchCat = "Lan",
#'    landingsVariable = "CLoffWeight",
#'    effortVariable = "CEnumFracTrips",
#'    samplingVariable = "SAsampWtMes",
#'    includeLandings = TRUE,
#'    includeEffort = TRUE,
#'    includeSamples = TRUE
#'  )
#'  myPlots[1]
#' }
coverageSpatial <- function(dataToPlot,
                            year,
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

  # For testing
  # myH1RawObject <- RDBEScore::createRDBESDataObject(input = "./tests/testthat/h1_v_20250211")
  # dataToPlot <- myH1RawObject
  # year <- 1965
  # quarter <- NA
  # vesselFlag <- "ZW"
  # landingsVariable <- "CLoffWeight"
  # effortVariable <- "CEnumFracTrips"
  # samplingVariable <- "SAsampWtMes"
  # groupingVariable <- "speciesName"
  # catchCat <- "Lan"
  # includeLandings <- TRUE
  # includeEffort <- FALSE
  # includeSamples <- TRUE
  # verbose <- FALSE

  # STEP 0) VALIDATE INPUTS

  # check the parameters are valid before we do anything
  if (verbose) {
    print("Validating input parameters")
  }

  # This function requires a value for year - the other ones generally allow NA
  if (is.na(year) == TRUE) {
    stop("You must provide  the year")
  }

  validateCoverageParameters(year = year,
                             quarter = NA,
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
  coverageData <- prepareCoverageData(dataToPlot = dataToPlot,
                                      year = year,
                                      quarter = NA,
                                      vesselFlag = vesselFlag,
                                      catchCat = catchCat,
                                      landingsVariable = landingsVariable,
                                      effortVariable = effortVariable,
                                      samplingVariable = samplingVariable,
                                      includeLandings = includeLandings,
                                      includeEffort = includeEffort,
                                      includeSamples = includeSamples,
                                      verbose = verbose)

  # STEP 2) Plot the data

  if (verbose) {
    print("Preparing plots")
  }

  plotsToPrint <- pointsPlot(
    landingsData = coverageData$landings,
    effortData = coverageData$effort,
    sampleData = coverageData$samples,
    vesselFlag = vesselFlag,
    catchCat = catchCat,
    landingsVariable = landingsVariable,
    effortVariable = effortVariable,
    samplingVariable = samplingVariable
  )

  plotsToPrint
}

#' Internal function to return a list of plots which compare the statistical
#' rectangle where landings occured to the statistical rectangles where
#' sampling occured. The relative amounts of landings/samples are
#' shown by i) coloured rectangles for landings, and ii) circles for
#' sampling.
#'
#' @param landingsData Landings data
#' @param effortData Effort data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param landingsVariable The variable from the landings data to plot
#' @param effortVariable The variable from the effort data to plot
#' @param samplingVariable The variable from the sample data to plot
#'
#' @return A tagList of ggplot2 plots
#'
pointsPlot <- function(landingsData = NA,
                       effortData = NA,
                       sampleData = NA,
                       vesselFlag,
                       catchCat,
                       landingsVariable,
                       effortVariable,
                       samplingVariable) {

  # For testing
  # landingsData = ld1
  # effortData = ef1
  # sampleData = sa1
  # sampleData$SAstatRect <- 2
  # vesselFlag = vesselFlag
  # catchCat = catchCat
  # landingsVariable = landingsVariable
  # effortVariable = effortVariable
  # samplingVariable = samplingVariable

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

  if (landings) {

    # change any blank stat rects ("") to NA
    landingsData[!is.na(landingsData$CLstatRect) & landingsData$CLstatRect == "",
                 "CLstatRect"] <- NA

    d1 <- na.omit(landingsData %>%
      dplyr::group_by(CLyear, CLstatRect) %>%
      dplyr::summarize(cl = sum(!!rlang::sym(
        landingsVariable
      ), na.rm = TRUE)))

    # Check we still have some landings data left to plot after removing NAs
    # - if we don't have then we can't plot landings data
    if (nrow(d1) == 0){
      warning(paste0("No non-NA landings data to plot - check the values ",
                     "of CLyear, CLstatRect and ", landingsVariable,
                     " in your input data"))
      landings <- FALSE
    }
  }

  if (samples) {

    # change any blank stat rects ("") to NA
    sampleData[!is.na(sampleData$SAstatRect) & sampleData$SAstatRect == "",
                 "SAstatRect"] <- NA

    d2 <- na.omit(sampleData %>%
      dplyr::group_by(SAyear, SAstatRect) %>%
      dplyr::summarize(sa = sum(!!rlang::sym(
        samplingVariable
      ), na.rm = TRUE)))

    # Check we still have some sample data left to plot after removing NAs
    # - if we don't have then we can't plot sample data
    if (nrow(d2) == 0){
      warning(paste0("No non-NA sample data to plot - check the values ",
                     "of SAyear, SAstatRect and ", samplingVariable,
                     " in your input data"))
      samples <- FALSE
    }
  }

  if (effort) {

    # change any blank stat rects ("") to NA
    effortData[!is.na(effortData$CEstatRect) & effortData$CEstatRect == "",
               "CEstatRect"] <- NA

    d3 <- na.omit(effortData %>%
      dplyr::group_by(CEyear, CEstatRect) %>%
      dplyr::summarize(ce = sum(!!rlang::sym(
        effortVariable
      ), na.rm = TRUE)))

    # Check we still have some effort data left to plot after removing NAs
    # - if we don't have then we can't plot effort data
    if (nrow(d3) == 0){
      warning(paste0("No non-NA effort data to plot - check the values ",
                     "of CEyear, CEstatRect and ", effortVariable,
                     " in your input data"))
      effort <- FALSE
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


  # get ices shp
  ices_rects <- RDBESvisualise::icesRectSF
  # define number of classes
  no_classes <- 6

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {
    # i <- 1
    if (landings) {
      dd <- d1 %>% dplyr::filter(CLyear == y[i])

      # Note: I received an error saying "all columns in a tibble must
      # be vectors".  This seemed to be a problem due to an old version of sf.
      # Looked at https://github.com/r-spatial/sf/issues/1381 , then added
      # sf to the "Depends" list for the package so that library(sf) gets
      # called when the package is loaded and that fixed the problem.
      ices_rects_l <- ices_rects %>%
        dplyr::left_join(dd, by = c("ICESNAME" = "CLstatRect"))

      # get quantiles
      ices_rects_l$var1 <- ices_rects_l$cl
      ices_rects_l <- getQuantiles(ices_rects_l, numberOfClasses = no_classes)
    }


    if (effort) {
      de <- d3 %>% dplyr::filter(CEyear == y[i])

      ices_rects_e <- ices_rects %>%
        dplyr::left_join(de, by = c("ICESNAME" = "CEstatRect"))

      # get quantiles
      ices_rects_e$var1 <- ices_rects_e$ce
      ices_rects_e <- getQuantiles(ices_rects_e, numberOfClasses = no_classes)
    }


    if (samples) {
      ds <- d2 %>% dplyr::filter(SAyear == y[i])

      ices_rects_s <- ices_rects %>%
        dplyr::left_join(ds, by = c("ICESNAME" = "SAstatRect"))

      # get quantiles
      ices_rects_s$var1 <- ices_rects_s$sa
      ices_rects_s <- getQuantiles(ices_rects_s, numberOfClasses = no_classes)

      # create point on surface
      # Note this line always produced a warning saying "st_point_on_surface
      # assumes attributes are constant over geometries of x" so I suppresed
      # it - there's probaby a way to fix the problem rather than
      # just hide it...
      suppressWarnings(points <-
                         sf::st_coordinates(
                           sf::st_point_on_surface(ices_rects_s)))
      points <- as.data.frame(points)
      points$sa <- ices_rects_s$sa
    }


    myPlots <- htmltools::tagList()

    if (landings) {
      # rename some variables
      # ices_rects_l$var1 <- ices_rects_l$cl

      if (samples) {
        # rename some variables
        pointsDataToPlot <- points
        pointsDataToPlot$var2 <- pointsDataToPlot$sa

        pointsTooltipText <- "Sampling: "
        subtitle <- paste0(
          "Sampling - ",
          catchCat, " (",
          samplingVariable,
          ")  vs Landings (",
          landingsVariable,
          ") in ",
          y[i]
        )
      } else {
        pointsDataToPlot <- NA
        pointsTooltipText <- ""
        subtitle <- paste0(
          "Landings (",
          landingsVariable,
          ") in ",
          y[i]
        )
      }

      # plot univariate map with points (if needed)
      x_l <- spatialPlotCreateGraph(ices_rects_l,
        pointsDataToPlot = pointsDataToPlot,
        tooltipText = "Landings: ",
        pointsTooltipText = pointsTooltipText,
        legendText = "Landings Variable",
        pointsLegendText = "Sampling Variable",
        title = paste0("Vessel Flag:  ", flagLabel),
        subtitle = subtitle
      )


      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_l
    }


    if (effort) {
      # rename some variables
      # ices_rects_e$var1 <- ices_rects_e$ce

      if (samples) {
        # rename some variables
        pointsDataToPlot <- points
        pointsDataToPlot$var2 <- pointsDataToPlot$sa

        pointsTooltipText <- "Sampling: "
        subtitle <- paste0(
          "Sampling - ",
          catchCat, " (",
          samplingVariable,
          ")  vs Effort (",
          effortVariable,
          ") in ",
          y[i]
        )
      } else {
        pointsDataToPlot <- NA
        pointsTooltipText <- ""
        subtitle <- paste0(
          "Effort (",
          effortVariable,
          ") in ",
          y[i]
        )
      }

      # plot univariate map with points (if needed)
      x_e <- spatialPlotCreateGraph(ices_rects_e,
        pointsDataToPlot = pointsDataToPlot,
        tooltipText = "Effort: ",
        pointsTooltipText = pointsTooltipText,
        legendText = "Effort Variable",
        pointsLegendText = "Sampling Variable",
        title = paste0("Vessel Flag:  ", flagLabel),
        subtitle = subtitle
      )


      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_e
    }


    if (samples) {
      # rename some variables
      # ices_rects_s$var1 <- ices_rects_s$sa


      pointsDataToPlot <- NA
      pointsTooltipText <- ""
      subtitle <- paste0(
        "Sampling - ",
        catchCat, " (",
        samplingVariable,
        ") in ",
        y[i]
      )


      # plot univariate map
      x_s <- spatialPlotCreateGraph(ices_rects_s,
        pointsDataToPlot = pointsDataToPlot,
        tooltipText = "Samples: ",
        pointsTooltipText = pointsTooltipText,
        legendText = "Sampling Variable",
        pointsLegendText = NA,
        title = paste0("Vessel Flag:  ", flagLabel),
        subtitle = subtitle
      )

      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_s
    }
  }

  all_plot
}

#' Internal function to get quantiles - we'll use this for the scale when
#' plotting the spatial graph
#'
#' @param dataToProcess The data to process
#' @param numberOfClasses The number of classes we want in the quantiles
#'
#' @return The data with mean_quantiles appended
getQuantiles <- function(dataToProcess, numberOfClasses = 6) {

  # For testing
  # dataToProcess <- ices_rects_s
  # numberOfClasses <- 6

  # Handle the case when we've requested more classes than the numerical range
  # of the variable
  maxValue <- dataToProcess %>%
    dplyr::pull(var1) %>%
    max(na.rm = TRUE)

  minValue <- dataToProcess %>%
    dplyr::pull(var1) %>%
    min(na.rm = TRUE)

  if (maxValue - minValue < numberOfClasses){
    numberOfClasses <- maxValue - minValue
  }

  # extract quantiles
  myQuantiles <- dataToProcess %>%
    dplyr::pull(var1) %>%
    quantile(
      probs = seq(0, 1, length.out = numberOfClasses + 1),
      na.rm = TRUE
    ) %>%
    as.vector() # to remove names of quantiles, so idx below is numeric
  myQuantiles <- round(myQuantiles, 0)

  # if all the values are the same then we need to fix it
  if (length(unique(myQuantiles)) == 1){
    # just keep first 2 entries - we expect to see at least 2 entries later on
    if (length(myQuantiles)>1){
      myQuantiles <- myQuantiles[c(1,2)]
    } else {
      myQuantiles <- c(myQuantiles,myQuantiles)
    }
    myQuantiles[1] <- myQuantiles[1] -1
  }

  # create custom labels without scientific notation (#20)
  myLabels <- paste0(
    formatNumber(myQuantiles[-length(myQuantiles)]),
    " - ",
    formatNumber(myQuantiles[-1])
  )

  # create new variable with quantiles - landings
  dataToProcess <- dataToProcess %>%
    dplyr::mutate(mean_quantiles = cut(
      var1,
      breaks = myQuantiles,
      labels = myLabels,
      include.lowest = TRUE
    ))

  dataToProcess
}


#' Internal function to create spatial plot using stat rectangles and
#' (optionallly) circles to represent the sampling coverage
#'
#' @param dataToPlot Data to plot by stat rectangle
#' @param pointsDataToPlot Optional secondary data to plot as circles
#' @param tooltipText Tooltip text
#' @param pointsTooltipText Tooltip text for secondary data
#' @param legendText Legend text
#' @param pointsLegendText Legend text for secondary data
#' @param title Title
#' @param subtitle Subtitle
#'
#' @return A ggplot graph
#'
spatialPlotCreateGraph <- function(dataToPlot,
                                   pointsDataToPlot = NA,
                                   tooltipText = "",
                                   pointsTooltipText = NA,
                                   legendText = "",
                                   pointsLegendText = NA,
                                   title = "",
                                   subtitle = "") {
  if (length(pointsDataToPlot) == 1 && is.na(pointsDataToPlot)) {
    plotPointsData <- FALSE
  } else {
    plotPointsData <- TRUE
  }

  # get extent of plot and only draw the rectangles and shoreline around it -
  # drawing all of them made the interactive plot very slow (#5)
  plotExtent <- sf::st_bbox(dataToPlot[!is.na(dataToPlot$var1), ])
  dataToPlot <- cropToExtent(dataToPlot, plotExtent)
  shorelineToPlot <- cropToExtent(RDBESvisualise::shoreline, plotExtent)

  # plot univariate map with points

  gg <- ggplot2::ggplot(data = dataToPlot) +
    ggplot2::geom_sf(
      data = shorelineToPlot,
      color = "white",
      fill = "gray",
      show.legend = FALSE
    ) +
    ggiraph::geom_sf_interactive(
      ggplot2::aes(
        fill = mean_quantiles,
        tooltip = paste(tooltipText, formatNumber(var1))
      ),
      color = "white",
      size = 0.1
    ) +
    ggiraph::scale_fill_brewer_interactive(
      type = "seq",
      palette = "RdYlBu",
      name = legendText,
      direction = -1,
      guide = ggplot2::guide_legend(
        keyheight = ggplot2::unit(5, units = "mm"),
        title.position = "top",
        reverse = TRUE
      )
    ) +
    ggplot2::coord_sf(
      xlim = plotExtent[c("xmin", "xmax")],
      ylim = plotExtent[c("ymin", "ymax")]
    )


  # See if we need to add the secondary data to the plot
  if (plotPointsData) {

    # Get rid of any NAs first - otherwise we get warnings
    pointsDataToPlot_noNa <- pointsDataToPlot[!is.na(pointsDataToPlot$var2),]

    gg <- gg +
      ggiraph::geom_point_interactive(
        data = pointsDataToPlot_noNa,
        ggplot2::aes(
          x = X,
          y = Y,
          size = var2,
          tooltip = paste(pointsTooltipText, formatNumber(var2))
        ),
        shape = 1,
        color = "black",
        alpha = 0.5
      ) +
      ggplot2::scale_size(labels = formatNumber)
  }


  gg <- gg +
    # add titles
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle,
      size = pointsLegendText
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
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 10)
    )

  x <- ggiraph::girafe(ggobj = gg, width_svg = 6, height_svg = 6)
  x <- ggiraph::girafe_options(
    x,
    ggiraph::opts_zoom(min = .4, max = 2)
  )

  x
}

#' Internal function to crop spatial data to the extent of a plot plus a
#' margin, so that only the visible part is drawn (#5)
#'
#' @param spatialData An sf object or an object that can be converted to one
#' with [sf::st_as_sf()], e.g. [shoreline]
#' @param plotExtent The extent of the plot, from [sf::st_bbox()]
#' @param margin Margin to add on each side, in degrees
#'
#' @return An sf object
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' - Notes/scope: Reduced the time to create one interactive map with the H1
#'   test data from about 20 s to about 1 s.
#' @keywords internal
cropToExtent <- function(spatialData, plotExtent, margin = 1) {
  cropExtent <- plotExtent + c(-margin, -margin, margin, margin)
  # st_crop warns that attributes are assumed constant over the geometries
  suppressWarnings(sf::st_crop(sf::st_as_sf(spatialData), cropExtent))
}
