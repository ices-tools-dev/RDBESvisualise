#' Provides graphical outputs which compare the statistical
#' rectangle where landings occured to the statistical rectangles where
#' sampling occured. The relative amounts of landings/samples are
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
#' myH1RawObject <- RDBEScore::createRDBESDataObject(
#'   rdbesExtractPath = "../tests/testthat/h1_v_1_19_13"
#' )
#'
#' myYear <- 1965
#' myvesselFlag <- "ZW"
#'
#' myPlots <- coverageLandingsSpatial(
#'   dataToPlot = myH1RawObject,
#'   year = myYear,
#'   vesselFlag = myvesselFlag,
#'   catchCat = "Lan",
#'   commercialVariable = "CLoffWeight",
#'   samplingVariable = "SAsampWtLive"
#' )
#'
#' myPlots[1]
#' }
coverageSpatial <- function(dataToPlot,
                            year = NA,
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

  if (includeEffort && length(effortVariable) > 1) {
    stop("You must provide effortVariable if you want to include effort data")
  }

  if (includeSamples && length(samplingVariable) > 1) {
    stop("You must provide samplingVariable if you want to include sample data")
  }

  if (includeLandings &&
    length(landingsVariable) == 1 &&
    !landingsVariable %in% RDBESvisualise::allowedLandingsVariable) {
    stop(paste0("Invalid landingsVariable value:", landingsVariable))
  }

  if (includeEffort &&
    length(effortVariable) == 1 &&
    !effortVariable %in% RDBESvisualise::allowedEffortVariable) {
    stop(paste0("Invalid effortVariable value:", effortVariable))
  }

  if (includeSamples &&
    length(samplingVariable) == 1 &&
    !samplingVariable %in% RDBESvisualise::allowedSamplingVariable) {
    stop(paste0("Invalid samplingVariable value:", samplingVariable))
  }

  if (length(catchCat) == 1 && !catchCat %in% RDBESvisualise::allowedCatchCat) {
    stop(paste0("Invalid catchCat value:", catchCat))
  }

  if (is.na(year) == TRUE) {
    stop("You must provide  the year")
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

  # Effort
  if (includeEffort) {
    ef <- preprocessEffortDataForCoverage(dataToPlot, verbose = verbose)
    ef1 <- filterEffortDataForCoverage(ef,
      year = year,
      quarter = NA,
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
      quarter = NA,
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

  plotsToPrint <- pointsPlot(
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
    d1 <- na.omit(landingsData %>%
      dplyr::group_by(CLyear, CLstatRect) %>%
      dplyr::summarize(cl = sum(!!rlang::sym(
        landingsVariable
      ))))
  }

  if (samples) {
    d2 <- na.omit(sampleData %>%
      dplyr::group_by(SAyear, SAstatRect) %>%
      dplyr::summarize(sa = sum(!!rlang::sym(
        samplingVariable
      ))))
  }

  if (effort) {
    d3 <- na.omit(effortData %>%
      dplyr::group_by(CEyear, CEstatRect) %>%
      dplyr::summarize(ce = sum(!!rlang::sym(
        effortVariable
      ))))
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
  # extract quantiles
  myQuantiles <- dataToProcess %>%
    dplyr::pull(var1) %>%
    quantile(
      probs = seq(0, 1, length.out = numberOfClasses + 1),
      na.rm = TRUE
    ) %>%
    as.vector() # to remove names of quantiles, so idx below is numeric
  myQuantiles <- round(myQuantiles, 0)

  # create custom labels
  myLabels <- purrr::imap_chr(myQuantiles, function(., idx) {
    return(paste0(
      round(myQuantiles[idx], 10),
      " - ",
      round(myQuantiles[idx + 1], 10)
    ))
  })

  # remove last label that includes NA
  myLabels <- head(myLabels, -1)

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

  # get extent of plot
  # No_NA_l <- ices_rects_l[ices_rects_l$cl != "NA", ]
  # No_NA <- dataToPlot[dataToPlot$var1 != "NA", ]
  No_NA <- dataToPlot[!is.na(dataToPlot$var1), ]
  xlim1 <- sf::st_bbox(No_NA)[1]
  ylim2 <- sf::st_bbox(No_NA)[2]
  xlim3 <- sf::st_bbox(No_NA)[3]
  ylim4 <- sf::st_bbox(No_NA)[4]


  # plot univariate map with points

  gg <- ggplot2::ggplot(data = dataToPlot) +
    ggplot2::geom_polygon(
      data = RDBESvisualise::shoreline,
      ggplot2::aes(x = long, y = lat, group = group),
      color = "white",
      fill = "gray",
      show.legend = FALSE
    ) +
    ggiraph::geom_sf_interactive(
      ggplot2::aes(
        fill = mean_quantiles,
        tooltip = paste(tooltipText, var1)
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
      xlim = c(xlim1, xlim3),
      ylim = c(ylim2, ylim4)
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
          tooltip = paste(pointsTooltipText, var2)
        ),
        shape = 1,
        color = "black",
        alpha = 0.5
      )
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
