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
coverageSpatialBivariate <- function(dataToPlot,
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

  plotsToPrint <- bivariatePlot(
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
#' sampling occured.  The relative amounts of landings/samples are
#' shown by the use of different colours for the rectangle.
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
bivariatePlot <- function(landingsData = NA,
                          effortData = NA,
                          sampleData,
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

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  # Sample data - always needed
  d2 <- na.omit(sampleData %>%
    dplyr::group_by(SAyear, SAstatRect) %>%
    dplyr::summarize(sa = sum(!!rlang::sym(
      samplingVariable
    ))))

  # Check we still have some sample data left to plot after removing NAs
  # - if we don't have then we can't proceeds
  if (nrow(d2) == 0){
    stop(paste0("No non-NA sample data to plot - check the values ",
    "of SAyear, SAstatRect and ", samplingVariable, " in your input data"))
  }


  if (landings) {
    d1 <- na.omit(landingsData %>%
      dplyr::group_by(CLyear, CLstatRect) %>%
      dplyr::summarize(cl = sum(!!rlang::sym(
        landingsVariable
      ))))

    df_l <-
      dplyr::full_join(d1,
        d2,
        by = c("CLyear" = "SAyear", "CLstatRect" = "SAstatRect")
      )

    # Check we still have some landings data left to plot after removing NAs
    # - if we don't have then we can't plot landings data
    if (nrow(d1) == 0){
      warning(paste0("No non-NA landings data to plot - check the values ",
                  "of CLyear, CLstatRect and ", landingsVariable,
                  " in your input data"))
      landings <- FALSE
    }
  }

  if (effort) {
    d3 <- na.omit(effortData %>%
      dplyr::group_by(CEyear, CEstatRect) %>%
      dplyr::summarize(ce = sum(!!rlang::sym(
        effortVariable
      ))))

    df_e <-
      dplyr::full_join(d3,
        d2,
        by = c("CEyear" = "SAyear", "CEstatRect" = "SAstatRect")
      )

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
  y <- c(y, unique(d2$SAyear))
  if (effort) {
    y <- c(y, unique(d3$CEyear))
  }
  y <- sort(unique(y))


  ices_rect <- RDBESvisualise::icesRectSpatialPolygon
  ices_rect_df <- ices_rect@data

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {
    if (landings) {
      dd_l <- df_l %>% dplyr::filter(CLyear == y[i])

      # rename sa, cl, clstatrect variables before we call plot function
      dd_l$var1 <- dd_l$sa
      dd_l$var2 <- dd_l$cl
      dd_l$statRect <- dd_l$CLstatRect
      myTitle <- paste0("Vessel Flag: ", flagLabel)
      mySubtitle <- paste0(
        "Sampling - ",
        catchCat, " (",
        samplingVariable,
        ")  vs Landings (",
        landingsVariable,
        ") in ",
        y[i]
      )

      # make the plot
      x_l <- bivariatePlotCreateGraph(
        dataToPlot = dd_l,
        ices_rect = ices_rect,
        subtitle = mySubtitle,
        title = myTitle,
        xlab = "Higher Sampling",
        ylab = "Higher Landings"
      )

      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_l
    }


    if (effort) {
      dd_e <- df_e %>% dplyr::filter(CEyear == y[i])

      # rename sa, ce, statrect variables before we call plot function
      dd_e$var1 <- dd_e$sa
      dd_e$var2 <- dd_e$ce
      dd_e$statRect <- dd_e$CEstatRect
      myTitle <- paste0("Vessel Flag: ", flagLabel)
      mySubtitle <- paste0(
        "Sampling - ",
        catchCat, " (",
        samplingVariable,
        ")  vs Effort (",
        effortVariable,
        ") in ",
        y[i]
      )

      # make the plot
      x_e <- bivariatePlotCreateGraph(
        dataToPlot = dd_e,
        ices_rect = ices_rect,
        subtitle = mySubtitle,
        title = myTitle,
        xlab = "Higher Sampling",
        ylab = "Higher Effort"
      )


      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_e
    }
  }

  all_plot
}


#' Internal function to create bivariate plot
#'
#' @param dataToPlot The data to plot
#' @param ices_rect ICES rectangles spatial information
#' @param title Title of the graph to create
#' @param subtitle Subtitle of the graph to create
#' @param xlab x-axis label for the graph to create
#' @param ylab y-axis label of the graph to create
#'
#' @return ggplot graph
#'
bivariatePlotCreateGraph <- function(dataToPlot,
                                     ices_rect,
                                     title,
                                     subtitle,
                                     xlab,
                                     ylab) {

  # remove any NAs first
  dataToPlot_NoNA <- dataToPlot
  dataToPlot_NoNA[is.na(dataToPlot_NoNA$var1),"var1"] <- 0
  dataToPlot_NoNA[is.na(dataToPlot_NoNA$var2),"var2"] <- 0

  # create classes
  biToPlot <-
    biscale::bi_class(
      dataToPlot_NoNA,
      x = var1,
      y = var2,
      style = "fisher",
      dim = 3
    )

  ices_rect_df <- ices_rect@data

  # join to our data
  bi_ices <-
    dplyr::left_join(ices_rect_df,
      biToPlot,
      by = c("ICESNAME" = "statRect")
    )

  # assign back to ices rectangles
  ices_rect_l <- ices_rect
  ices_rect_l@data <- bi_ices

  # get ICES rectangles feature collection
  bi_fc <- sf::st_as_sf(ices_rect_l)

  # ICES rectangles feature collection without NAs for bounding box
  bi_fc_No_NA <- na.omit(bi_fc)

  # create map
  map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = RDBESvisualise::shoreline,
      ggplot2::aes(x = long, y = lat, group = group),
      color = "azure2",
      fill = "azure4",
      show.legend = FALSE
    ) +
    ggplot2::geom_sf(
      data = bi_fc,
      mapping = ggplot2::aes(fill = bi_class),
      color = "transparent",
      size = 0.1,
      show.legend = FALSE
    ) +
    biscale::bi_scale_fill(
      pal = "GrPink",
      dim = 3,
      na.value = "transparent"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(bi_fc_No_NA)[1], sf::st_bbox(bi_fc_No_NA)[3]),
      ylim = c(sf::st_bbox(bi_fc_No_NA)[2], sf::st_bbox(bi_fc_No_NA)[4]),
      expand = FALSE
    ) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  # legend
  legend <- biscale::bi_legend(
    pal = "GrPink",
    dim = 3,
    xlab = xlab,
    ylab = ylab,
    size = 9
  )

  # combine map with legend
  finalPlot <- cowplot::plot_grid(map,
    legend,
    labels = NULL,
    rel_widths = c(2.5, 1),
    rel_heights = c(2.5, 1)
  )
  x <- ggiraph::girafe(ggobj = finalPlot, width_svg = 6, height_svg = 6)
  x <- ggiraph::girafe_options(
    x,
    ggiraph::opts_zoom(min = .4, max = 2)
  )

  x
}
