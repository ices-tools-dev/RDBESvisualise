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


#' Internal function to return a list of plots which compare the species in
#' landings to those in the sample data.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param (Optional) Number of species to plot.  Default is 10.
#'
#' @return A tagList of plotly plots
#'
speciesPlot <- function(landingsData,
                        sampleData,
                        vesselFlag,
                        catchCat,
                        topN = 10) {

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  # Get the species names
  full_name <- RDBESvisualise::speciesNamesAndCodes
  full_name <- full_name[-which(duplicated(full_name$AphiaID)), ]

  # Landings data
  # Group by year, and year and quarter, and count the number of species
  df1 <- na.omit(
    landingsData %>%
      dplyr::group_by(CLyear) %>%
      dplyr::add_count(CLspecCode, name = "CLSpeCount") %>%
      dplyr::summarise(LandingCountYear = sum(CLSpeCount))
  ) %>%
    dplyr::mutate(totalSpeCountAll = sum(LandingCountYear))
  d1 <- na.omit(
    landingsData %>%
      dplyr::group_by(CLyear, CLquar, CLspecCode) %>%
      dplyr::add_count(CLspecCode, name = "CLSpeCount") %>%
      dplyr::summarise(LandingCount = sum(CLSpeCount))
  )
  d1Species <-
    dplyr::left_join(d1, full_name, by = c("CLspecCode" = "AphiaID"))

  d1Species <- dplyr::left_join(d1Species, df1, by = "CLyear") %>%
    dplyr::mutate(relativeValuesYear = LandingCount / LandingCountYear) %>%
    dplyr::mutate(relativeValuesAll = LandingCount / totalSpeCountAll) %>%
    dplyr::top_n(topN)

  # Sample data
  # Group by year, and year and quarter, and count the number of species

  # add df to calculate total species for year
  df2 <- na.omit(
    sampleData %>%
      dplyr::group_by(SAyear) %>%
      dplyr::add_count(SAspeCode, name = "SASpeCount") %>%
      dplyr::summarise(SamplingCountYear = sum(SASpeCount))
  ) %>%
    dplyr::mutate(totalSpeCountAll = sum(SamplingCountYear))

  d2 <- na.omit(
    sampleData %>%
      dplyr::group_by(SAyear, SAquar, SAspeCode) %>%
      dplyr::add_count(SAspeCode, name = "SASpeCount") %>%
      dplyr::summarise(SamplingCount = sum(SASpeCount))
  )
  d2Species <-
    dplyr::left_join(d2, full_name, by = c("SAspeCode" = "AphiaID"))

  d2Species <- dplyr::left_join(d2Species, df2, by = "SAyear") %>%
    dplyr::mutate(relSamplingYear = SamplingCount / SamplingCountYear) %>%
    dplyr::mutate(relSamplingAll = SamplingCount / totalSpeCountAll) %>%
    dplyr::top_n(topN)

  y <- unique(d1Species$CLyear)

  # Generate plots for the data

  all_plot <- htmltools::tagList()
  for (i in  seq_along(length(y))) {
    t1 <- d1Species %>% dplyr::filter(CLyear == y[i])
    t2 <- d2Species %>% dplyr::filter(SAyear == y[i])
    # Landings plot
    p1 <- plotly::plot_ly(
      t1,
      x = ~ as.character(FAODescription),
      y = ~relativeValuesYear,
      color = ~ as.character(CLquar),
      type = "bar",
      showlegend = FALSE
    ) %>%
      plotly::layout(
        title = paste0(
          "Vessel Flag ",
          flagLabel,
          ": Top Landings Species in",
          y[i]
        ),
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
      showlegend = TRUE
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
}


#' Internal function to return a list of plots which compare the relative
#' amount of the values of commercialVariable and samplingVariable by quarter
#' landings to those in the sample data.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param commercialVariable Variable from CL to plot
#' @param samplingVariable variable from SA to plot
#'
#' @return A tagList of plotly plots
#'
temporalPlot <- function(landingsData,
                         sampleData,
                         vesselFlag,
                         catchCat,
                         commercialVariable,
                         samplingVariable) {

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  d1 <- na.omit(landingsData %>%
                  dplyr::group_by(CLyear, CLquar) %>%
                  dplyr::summarize(CL = sum(!!rlang::sym(
                    commercialVariable
                  )))) %>%
    dplyr::mutate(relCL = CL / sum(CL))
  d2 <- na.omit(sampleData %>%
                  dplyr::group_by(SAyear, SAquar) %>%
                  dplyr::summarize(sa = sum(!!rlang::sym(
                    samplingVariable
                  )))) %>%
    dplyr::mutate(relSA = sa / sum(sa))

  df <-
    dplyr::left_join(d1, d2, by = c("CLyear" = "SAyear", "CLquar" = "SAquar"))

  y <- unique(df$CLyear)


  all_plot <- htmltools::tagList()
  for (i in seq_along(length(y))) {
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
}

#' Internal function to return a list of plots which compare the fishing gears
#' in landings to those in the sample data.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param quarter Quarter of year
#'
#' @return A tagList of plotly plots
#'
gearPlot <- function(landingsData,
                     sampleData,
                     vesselFlag,
                     catchCat,
                     quarter) {

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  if (is.na(quarter) == FALSE) {
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
      dplyr::mutate(relativeValuesL = LandingsGearCount / LandingsGearCountQuar)

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
      dplyr::mutate(relativeValuesS = SamplingGearCount / SamplingGearCountQuar)
  } else {
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
      dplyr::mutate(relativeValuesS = SamplingGearCount / SamplingGearCountYear)
  }


  df <-
    dplyr::left_join(d1, d2, by = c("CLyear" = "SAyear", "CLGear" = "SAgear"))

  y <- unique(df$CLyear)

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {
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
      showlegend = FALSE
    ) %>%
      plotly::layout(
        title = paste0(
          "Vessel Flag ",
          flagLabel,
          " : Top Landings Gear - Relative Values per Plot \n in",
          y[i]
        ),
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
      showlegend = FALSE
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


#' Internal function to return a list of plots which compare the statistical
#' rectangle where landings occured to the statistical rectangles where
#' sampling occured. The relative amounts of landings/samples are
#' shown by i) coloured rectangles for landings, and ii) circles for
#' sampling.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param commercialVariable The variable from the landings data to plot
#' @param samplingVariable The variable from the sample data to plot
#'
#' @return A tagList of ggplot2 plots
#'
pointsPlot <- function(landingsData,
                       sampleData,
                       vesselFlag,
                       catchCat,
                       commercialVariable,
                       samplingVariable) {

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  d1 <- na.omit(landingsData %>%
                  dplyr::group_by(CLyear, CLstatRect) %>%
                  dplyr::summarize(cl = sum(!!rlang::sym(
                    commercialVariable
                  ))))
  d2 <- na.omit(sampleData %>%
                  dplyr::group_by(SAyear, SAstatRect) %>%
                  dplyr::summarize(sa = sum(!!rlang::sym(
                    samplingVariable
                  ))))

  df <-
    dplyr::left_join(d1,
                     d2,
                     by = c("CLyear" = "SAyear", "CLstatRect" = "SAstatRect")
    )

  y <- unique(df$CLyear)

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {

    dd <- df %>% dplyr::filter(CLyear == y[i])

    # get ices shp
    ices_rects <- RDBESvisualise::icesRectSF

    # Note: I received an error saying "all columns in a tibble must
    # be vectors".  This seemed to be a problem due to an old version of sf.
    # Looked at https://github.com/r-spatial/sf/issues/1381 , then added
    # sf to the "Depends" list for the package so that library(sf) gets
    # called when the package is loaded and that fixed the problem.
    ices_rects <- ices_rects %>%
      dplyr::left_join(dd, by = c("ICESNAME" = "CLstatRect"))

    # get extent of plot
    No_NA <- ices_rects[ices_rects$cl != "NA", ]
    xlim1 <- sf::st_bbox(No_NA)[1]
    ylim2 <- sf::st_bbox(No_NA)[2]
    xlim3 <- sf::st_bbox(No_NA)[3]
    ylim4 <- sf::st_bbox(No_NA)[4]

    # define number of classes
    no_classes <- 6

    # extract quantiles
    quantiles <- ices_rects %>%
      dplyr::pull(cl) %>%
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
    labels <- head(labels, -1)

    # create new variable with quantiles - landings
    ices_rects <- ices_rects %>%
      dplyr::mutate(mean_quantiles_land = cut(
        #CL,
        cl,
        breaks = quantiles,
        labels = labels,
        include.lowest = TRUE
      ))


    # create point on surface
    points <- sf::st_coordinates(sf::st_point_on_surface(ices_rects))
    points <- as.data.frame(points)
    points$sa <- ices_rects$sa

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
          tooltip = paste("Landings:", cl)
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
          reverse = TRUE
        )
      ) +
      ggiraph::geom_point_interactive(
        data = points,
        ggplot2::aes(
          x = X,
          y = Y,
          size = sa,
          tooltip = paste("Sampling: ", sa)
        ),
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

#' Internal function to return a list of plots which compare the statistical
#' rectangle where landings occured to the statistical rectangles where
#' sampling occured.  The relative amounts of landings/samples are
#' shown by the use of different colours for the rectangle.
#'
#' @param landingsData Landings data
#' @param sampleData Sample data
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR".
#' @param catchCat Catch category
#' @param commercialVariable The variable from the landings data to plot
#' @param samplingVariable The variable from the sample data to plot
#'
#' @return A tagList of ggplot2 plots
#'
bivariatePlot <- function(landingsData,
                          sampleData,
                          vesselFlag,
                          catchCat,
                          commercialVariable,
                          samplingVariable) {

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  d1 <- na.omit(landingsData %>%
                  dplyr::group_by(CLyear, CLstatRect) %>%
                  dplyr::summarize(CL = sum(!!rlang::sym(
                    commercialVariable
                  ))))
  d2 <- na.omit(sampleData %>%
                  dplyr::group_by(SAyear, SAstatRect) %>%
                  dplyr::summarize(sa = sum(!!rlang::sym(
                    samplingVariable
                  ))))

  df <-
    dplyr::left_join(d1,
                     d2,
                     by = c("CLyear" = "SAyear", "CLstatRect" = "SAstatRect")
    )

  y <- unique(df$CLyear)

  ices_rect <- RDBESvisualise::icesRectSpatialPolygon
  ices_rect_df <- ices_rect@data

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {
    dd <- df %>% dplyr::filter(CLyear == y[i])

    # create classes
    biToPlot <-
      biscale::bi_class(
        dd,
        x = sa,
        y = CL,
        style = "fisher",
        dim = 3
      )
    # join to our data
    bi_ices <-
      dplyr::left_join(ices_rect_df,
                       biToPlot,
                       by = c("ICESNAME" = "CLstatRect"))

    # assign back to ices rectangles
    ices_rect@data <- bi_ices

    # get ICES rectangles feature collection
    bi_fc <- sf::st_as_sf(ices_rect)

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
        title = paste0("Vessel Flag: ", flagLabel),
        subtitle = paste0(
          "Sampling - ",
          catchCat, " (",
          samplingVariable,
          ")  vs Landings (",
          commercialVariable,
          ") in ",
          y[i]
        )
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
      xlab = "Higher Sampling",
      ylab = "Higher Landings",
      size = 9
    )

    # combine map with legend
    finalPlot <- cowplot::plot_grid(map,
                                    legend,
                                    labels = NULL,
                                    rel_widths = c(2.5, 1),
                                    rel_heights = c(2.5, 1))
    x <- ggiraph::girafe(ggobj = finalPlot, width_svg = 6, height_svg = 6)
    x <- ggiraph::girafe_options(
      x,
      ggiraph::opts_zoom(min = .4, max = 2)
    )

    all_plot[[i]] <- x
  }

  all_plot
}
#' Internal function to filter landings data for coverageLandingsXXX functions
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
#' Internal function to filter sample data for coverageLandingsXXX functions
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
#' Internal function to prepare sample data for coverageLandingsXXX functions
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
#' Internal function to prepare landings data for coverageLandingsXXX functions
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






