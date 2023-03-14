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
#' amount of the values of landingVariable, effortVariable and samplingVariable
#' by quarter.
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
temporalPlot <- function(landingsData = NA,
                         effortData = NA,
                         sampleData = NA,
                         vesselFlag,
                         catchCat,
                         landingsVariable,
                         effortVariable,
                         samplingVariable) {

  # see what data we've been given
  if (length(landingsData) == 1 && is.na(landingsData)){
    landings <- FALSE
  } else {
    landings <- TRUE
  }
  if (length(effortData) == 1 && is.na(effortData)){
    effort <- FALSE
  } else {
    effort <- TRUE
  }
  if (length(sampleData) == 1 && is.na(sampleData)){
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
  if (samples){
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
  if (landings){
    y <- c(y,unique(d1$CLyear))
  }
  if (samples){
    y <- c(y,unique(d2$SAyear))
  }
  if (effort){
    y <- c(y,unique(d3$CEyear))
  }
  y <- sort(unique(y))


  all_plot <- htmltools::tagList()



  for (i in seq_along(length(y))) {

    show_legend <- if (i == 1) {
      TRUE
    } else {
      FALSE
    }

    plotTitle = paste0("Vessel Flag ", flagLabel)

    # Create an empty data set to use as a base for our plots
     emptyData <- data.frame(quarters = c(1,2,3,4),values =c(NA,NA,NA,NA))
     p0 <- plotly::plot_ly(
       emptyData,
       x = ~quarters,
       y = ~values,
       type = "bar",
       alpha = 0.7
     )
    #p0 <- plotly::plotly_empty()

    # Landings
    if (landings){
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

      plotTitle = paste0(plotTitle, " | Landings: ", landingsVariable)
    }

    # Effort
    if (effort){
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

      plotTitle = paste0(plotTitle, " | Effort: ", effortVariable)
    }

    # Samples
    if (samples){
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

      plotTitle = paste0(plotTitle,
                         " | Sampling: ",
                         samplingVariable,
                         " (",
                         catchCat,
                         ") "
                         )
    }

    plotTitle = paste0(plotTitle, " in ",y[i])

    # Add a title
    p0 <- p0 %>%
      plotly::layout(
        title = list(text = plotTitle,font = list(size = 12)),
        xaxis = list(title = "Quarter"),
        yaxis = list(title = "Relative Values")
      )
    all_plot[[i]] <- p0

  }
  all_plot
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
  if (length(landingsData) == 1 && is.na(landingsData)){
    landings <- FALSE
  } else {
    landings <- TRUE
  }
  if (length(effortData) == 1 && is.na(effortData)){
    effort <- FALSE
  } else {
    effort <- TRUE
  }
  if (length(sampleData) == 1 && is.na(sampleData)){
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
        dplyr::mutate(relativeValuesL =
                        LandingsGearCount / LandingsGearCountQuar)
    }

    # Samples
    if (samples){

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
        dplyr::mutate(relativeValuesS =
                        SamplingGearCount / SamplingGearCountQuar)
    }


    # Effort
    if (effort){
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
        dplyr::mutate(relativeValuesE =
                        EffortGearCount / EffortsGearCountQuar)
    }


  } else {

    # Landings
    if (landings){
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
    if (samples){
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
        dplyr::mutate(relativeValuesS =
                        SamplingGearCount / SamplingGearCountYear)
    }

    # Effort
    if (effort){
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
  if (landings){
    y <- c(y,unique(d1$CLyear))
  }
  if (samples){
    y <- c(y,unique(d2$SAyear))
  }
  if (effort){
    y <- c(y,unique(d3$CEyear))
  }
  y <- sort(unique(y))


  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {
    # Landings
    if (landings){
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
      p1 <- plotly::plotly_empty()
    }

    # Samples
    if (samples){
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
      p2 <- plotly::plotly_empty()
    }

    # Effort
    if (effort){
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
            " : Top Gear - Relative Values per Plot \n in",
            y[i]
          ),
          yaxis = list(title = "Effort"),
          xaxis = list(categoryorder = "total descending"),
          barmode = "stack"
        )
    } else {
      p3 <- plotly::plotly_empty()
    }


    #all_plot[[i]] <- plotly::subplot(p1, p2, titleY = TRUE, nrows = 2)
    all_plot[[i]] <- plotly::subplot(p1, p3, p2, titleY = TRUE, nrows = 3)
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
  if (length(landingsData) == 1 && is.na(landingsData)){
    landings <- FALSE
  } else {
    landings <- TRUE
  }
  if (length(effortData) == 1 && is.na(effortData)){
    effort <- FALSE
  } else {
    effort <- TRUE
  }
  if (length(sampleData) == 1 && is.na(sampleData)){
    samples <- FALSE
  } else {
    samples <- TRUE
  }

  if (is.na(vesselFlag)) {
    flagLabel <- "All"
  } else {
    flagLabel <- vesselFlag
  }

  if (landings){
    d1 <- na.omit(landingsData %>%
                    dplyr::group_by(CLyear, CLstatRect) %>%
                    dplyr::summarize(cl = sum(!!rlang::sym(
                      landingsVariable
                    ))))
  }

  if (samples){
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
  if (landings){
    y <- c(y,unique(d1$CLyear))
  }
  if (samples){
    y <- c(y,unique(d2$SAyear))
  }
  if (effort){
    y <- c(y,unique(d3$CEyear))
  }
  y <- sort(unique(y))


  # get ices shp
  ices_rects <- RDBESvisualise::icesRectSF
  # define number of classes
  no_classes <- 6

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {


    if (landings){
      dd <- d1 %>% dplyr::filter(CLyear == y[i])

      # Note: I received an error saying "all columns in a tibble must
      # be vectors".  This seemed to be a problem due to an old version of sf.
      # Looked at https://github.com/r-spatial/sf/issues/1381 , then added
      # sf to the "Depends" list for the package so that library(sf) gets
      # called when the package is loaded and that fixed the problem.
      ices_rects_l <- ices_rects %>%
        dplyr::left_join(dd, by = c("ICESNAME" = "CLstatRect"))

      # get extent of plot
      No_NA_l <- ices_rects_l[ices_rects_l$cl != "NA", ]
      xlim1_l <- sf::st_bbox(No_NA_l)[1]
      ylim2_l <- sf::st_bbox(No_NA_l)[2]
      xlim3_l <- sf::st_bbox(No_NA_l)[3]
      ylim4_l <- sf::st_bbox(No_NA_l)[4]

      # extract quantiles
      quantiles_l <- ices_rects_l %>%
        dplyr::pull(cl) %>%
        quantile(
          probs = seq(0, 1, length.out = no_classes + 1),
          na.rm = TRUE
        ) %>%
        as.vector() # to remove names of quantiles, so idx below is numeric
      quantiles_l <- round(quantiles_l, 0)

      # create custom labels
      labels_l <- purrr::imap_chr(quantiles_l, function(., idx) {
        return(paste0(
          round(quantiles_l[idx], 10),
          " - ",
          round(quantiles_l[idx + 1], 10)
        ))
      })

      # remove last label that includes NA
      labels_l <- head(labels_l, -1)

      # create new variable with quantiles - landings
      ices_rects_l <- ices_rects_l %>%
        dplyr::mutate(mean_quantiles_land = cut(
          #CL,
          cl,
          breaks = quantiles_l,
          labels = labels_l,
          include.lowest = TRUE
        ))

    }


    if (effort){
      de <- d3 %>% dplyr::filter(CEyear == y[i])

      ices_rects_e <- ices_rects %>%
        dplyr::left_join(de, by = c("ICESNAME" = "CEstatRect"))

      # get extent of plot
      No_NA_e <- ices_rects_e[ices_rects_e$ce != "NA", ]
      xlim1_e <- sf::st_bbox(No_NA_e)[1]
      ylim2_e <- sf::st_bbox(No_NA_e)[2]
      xlim3_e <- sf::st_bbox(No_NA_e)[3]
      ylim4_e <- sf::st_bbox(No_NA_e)[4]

      # extract quantiles
      quantiles_e <- ices_rects_e %>%
        dplyr::pull(ce) %>%
        quantile(
          probs = seq(0, 1, length.out = no_classes + 1),
          na.rm = TRUE
        ) %>%
        as.vector() # to remove names of quantiles, so idx below is numeric
      quantiles_e <- round(quantiles_e, 0)

      # create custom labels
      labels_e <- purrr::imap_chr(quantiles_e, function(., idx) {
        return(paste0(
          round(quantiles_e[idx], 10),
          " - ",
          round(quantiles_e[idx + 1], 10)
        ))
      })

      # remove last label that includes NA
      labels_e <- head(labels_e, -1)

      # create new variable with quantiles - landings
      ices_rects_e <- ices_rects_e %>%
        dplyr::mutate(mean_quantiles_effort = cut(
          #CL,
          ce,
          breaks = quantiles_e,
          labels = labels_e,
          include.lowest = TRUE
        ))

    }


    if (samples){
      ds <- d2 %>% dplyr::filter(SAyear == y[i])

      ices_rects_s <- ices_rects %>%
        dplyr::left_join(ds, by = c("ICESNAME" = "SAstatRect"))

      # get extent of plot
      No_NA_s <- ices_rects_s[ices_rects_s$sa != "NA", ]
      xlim1_s <- sf::st_bbox(No_NA_s)[1]
      ylim2_s <- sf::st_bbox(No_NA_s)[2]
      xlim3_s <- sf::st_bbox(No_NA_s)[3]
      ylim4_s <- sf::st_bbox(No_NA_s)[4]

      # create point on surface
      points <- sf::st_coordinates(sf::st_point_on_surface(ices_rects_s))
      points <- as.data.frame(points)
      points$sa <- ices_rects_s$sa

      # extract quantiles
      quantiles_s <- ices_rects_s %>%
        dplyr::pull(sa) %>%
        quantile(
          probs = seq(0, 1, length.out = no_classes + 1),
          na.rm = TRUE
        ) %>%
        as.vector() # to remove names of quantiles, so idx below is numeric
      quantiles_s <- round(quantiles_s, 0)

      # create custom labels
      labels_s <- purrr::imap_chr(quantiles_s, function(., idx) {
        return(paste0(
          round(quantiles_s[idx], 10),
          " - ",
          round(quantiles_s[idx + 1], 10)
        ))
      })

      # remove last label that includes NA
      labels_s <- head(labels_s, -1)

      # create new variable with quantiles - landings
      ices_rects_s <- ices_rects_s %>%
        dplyr::mutate(mean_quantiles_samples = cut(
          #CL,
          sa,
          breaks = quantiles_s,
          labels = labels_s,
          include.lowest = TRUE
        ))

    }


    myPlots <- htmltools::tagList()

    if (landings){

      # plot univariate map with points

      gg <- ggplot2::ggplot(data = ices_rects_l) +
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
           ggplot2::coord_sf(
             xlim = c(xlim1_l, xlim3_l),
             ylim = c(ylim2_l, ylim4_l)
        )


      # See if we need to add the samples data to the plot
      subtitle = ""
      if (samples){
        subtitle = paste0(
                "Sampling - ",
                catchCat, " (",
                samplingVariable,
                ")  vs Landings (",
                landingsVariable,
                ") in ",
                y[i]
              )
        gg <- gg +
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
          )
      } else {
        subtitle = paste0(
          "Landings (",
          landingsVariable,
          ") in ",
          y[i]
        )
      }


      gg <- gg +
        # add titles
        ggplot2::labs(
          x = NULL,
          y = NULL,
          title = paste0("Vessel Flag:  ", flagLabel),
          subtitle = subtitle,
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

      x_l <- ggiraph::girafe(ggobj = gg, width_svg = 6, height_svg = 6)
      x_l <- ggiraph::girafe_options(
        x_l,
        ggiraph::opts_zoom(min = .4, max = 2)
      )

      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_l

    }


    if (effort){

      # plot univariate map with points

      gg <- ggplot2::ggplot(data = ices_rects_e) +
        ggplot2::geom_polygon(
          data = RDBESvisualise::shoreline,
          ggplot2::aes(x = long, y = lat, group = group),
          color = "white",
          fill = "gray",
          show.legend = FALSE
        ) +
        ggiraph::geom_sf_interactive(
          ggplot2::aes(
            fill = mean_quantiles_effort,
            tooltip = paste("Effort:", ce)
          ),
          color = "white",
          size = 0.1
        ) +
        ggiraph::scale_fill_brewer_interactive(
          type = "seq",
          palette = "RdYlBu",
          name = "Effort Variable",
          direction = -1,
          guide = ggplot2::guide_legend(
            keyheight = ggplot2::unit(5, units = "mm"),
            title.position = "top",
            reverse = TRUE
          )
        ) +
        ggplot2::coord_sf(
          xlim = c(xlim1_e, xlim3_e),
          ylim = c(ylim2_e, ylim4_e)
        )


      # See if we need to add the samples data to the plot
      subtitle = ""
      if (samples){
        subtitle = paste0(
          "Sampling - ",
          catchCat, " (",
          samplingVariable,
          ")  vs Effort (",
          effortVariable,
          ") in ",
          y[i]
        )
        gg <- gg +
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
          )
      } else {
        subtitle = paste0(
          "Effort (",
          effortVariable,
          ") in ",
          y[i]
        )
      }


      gg <- gg +
        # add titles
        ggplot2::labs(
          x = NULL,
          y = NULL,
          title = paste0("Vessel Flag:  ", flagLabel),
          subtitle = subtitle,
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

      x_e <- ggiraph::girafe(ggobj = gg, width_svg = 6, height_svg = 6)
      x_e <- ggiraph::girafe_options(
        x_e,
        ggiraph::opts_zoom(min = .4, max = 2)
      )

      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_e
    }


      if (samples){

        # plot univariate map with points

        gg <- ggplot2::ggplot(data = ices_rects_s) +
          ggplot2::geom_polygon(
            data = RDBESvisualise::shoreline,
            ggplot2::aes(x = long, y = lat, group = group),
            color = "white",
            fill = "gray",
            show.legend = FALSE
          ) +
          ggiraph::geom_sf_interactive(
            ggplot2::aes(
              fill = mean_quantiles_samples,
              tooltip = paste("Samples:", sa)
            ),
            color = "white",
            size = 0.1
          ) +
          ggiraph::scale_fill_brewer_interactive(
            type = "seq",
            palette = "RdYlBu",
            name = "Sampling Variable",
            direction = -1,
            guide = ggplot2::guide_legend(
              keyheight = ggplot2::unit(5, units = "mm"),
              title.position = "top",
              reverse = TRUE
            )
          ) +
          ggplot2::coord_sf(
            xlim = c(xlim1_s, xlim3_s),
            ylim = c(ylim2_s, ylim4_s)
          )


        # See if we need to add the samples data to the plot - we do - this
        # is just sample data
        subtitle = paste0(
          "Sampling - ",
          catchCat, " (",
          samplingVariable,
          ") in ",
          y[i]
        )

        gg <- gg +
          # add titles
          ggplot2::labs(
            x = NULL,
            y = NULL,
            title = paste0("Vessel Flag:  ", flagLabel),
            subtitle = subtitle,
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

        x_s <- ggiraph::girafe(ggobj = gg, width_svg = 6, height_svg = 6)
        x_s <- ggiraph::girafe_options(
          x_s,
          ggiraph::opts_zoom(min = .4, max = 2)
        )

        # Add our plot to the output list
        currentPlotCount <- length(all_plot)
        all_plot[[currentPlotCount + i]] <- x_s

    }

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
  if (length(landingsData) == 1 && is.na(landingsData)){
    landings <- FALSE
  } else {
    landings <- TRUE
  }
  if (length(effortData) == 1 && is.na(effortData)){
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


  if (landings){
    d1 <- na.omit(landingsData %>%
                    dplyr::group_by(CLyear, CLstatRect) %>%
                    dplyr::summarize(cl = sum(!!rlang::sym(
                      landingsVariable
                    ))))

    df_l <-
      dplyr::left_join(d1,
                       d2,
                       by = c("CLyear" = "SAyear", "CLstatRect" = "SAstatRect")
      )
  }

  if (effort){
    d3 <- na.omit(effortData %>%
                    dplyr::group_by(CEyear, CEstatRect) %>%
                    dplyr::summarize(ce = sum(!!rlang::sym(
                      effortVariable
                    ))))

    df_e <-
      dplyr::left_join(d3,
                       d2,
                       by = c("CEyear" = "SAyear", "CEstatRect" = "SAstatRect")
      )
  }



  # Get the years we want plot
  y <- c()
  if (landings){
    y <- c(y,unique(d1$CLyear))
  }
  y <- c(y,unique(d2$SAyear))
  if (effort){
    y <- c(y,unique(d3$CEyear))
  }
  y <- sort(unique(y))


  ices_rect <- RDBESvisualise::icesRectSpatialPolygon
  ices_rect_df <- ices_rect@data

  all_plot <- htmltools::tagList()

  for (i in seq_along(length(y))) {

    if (landings){
      dd_l <- df_l %>% dplyr::filter(CLyear == y[i])

      # create classes
      biToPlot_l <-
        biscale::bi_class(
          dd_l,
          x = sa,
          y = cl,
          style = "fisher",
          dim = 3
        )
      # join to our data
      bi_ices_l <-
        dplyr::left_join(ices_rect_df,
                         biToPlot_l,
                         by = c("ICESNAME" = "CLstatRect"))

      # assign back to ices rectangles
      ices_rect_l <- ices_rect
      ices_rect_l@data <- bi_ices_l

      # get ICES rectangles feature collection
      bi_fc_l <- sf::st_as_sf(ices_rect_l)

      # ICES rectangles feature collection without NAs for bounding box
      bi_fc_No_NA_l <- na.omit(bi_fc_l)

      # create map
      map_l <- ggplot2::ggplot() +
        ggplot2::geom_polygon(
          data = RDBESvisualise::shoreline,
          ggplot2::aes(x = long, y = lat, group = group),
          color = "azure2",
          fill = "azure4",
          show.legend = FALSE
        ) +
        ggplot2::geom_sf(
          data = bi_fc_l,
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
            landingsVariable,
            ") in ",
            y[i]
          )
        ) +
        ggplot2::coord_sf(
          xlim = c(sf::st_bbox(bi_fc_No_NA_l)[1], sf::st_bbox(bi_fc_No_NA_l)[3]),
          ylim = c(sf::st_bbox(bi_fc_No_NA_l)[2], sf::st_bbox(bi_fc_No_NA_l)[4]),
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
      legend_l <- biscale::bi_legend(
        pal = "GrPink",
        dim = 3,
        xlab = "Higher Sampling",
        ylab = "Higher Landings",
        size = 9
      )

      # combine map with legend
      finalPlot <- cowplot::plot_grid(map_l,
                                      legend_l,
                                      labels = NULL,
                                      rel_widths = c(2.5, 1),
                                      rel_heights = c(2.5, 1))
      x_l <- ggiraph::girafe(ggobj = finalPlot, width_svg = 6, height_svg = 6)
      x_l <- ggiraph::girafe_options(
        x_l,
        ggiraph::opts_zoom(min = .4, max = 2)
      )

      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_l
    }


    if (effort){
      dd_e <- df_e %>% dplyr::filter(CEyear == y[i])

      # create classes
      biToPlot_e <-
        biscale::bi_class(
          dd_e,
          x = sa,
          y = ce,
          style = "fisher",
          dim = 3
        )
      # join to our data
      bi_ices_e <-
        dplyr::left_join(ices_rect_df,
                         biToPlot_e,
                         by = c("ICESNAME" = "CEstatRect"))

      # assign back to ices rectangles
      ices_rect_e <- ices_rect
      ices_rect_e@data <- bi_ices_e

      # get ICES rectangles feature collection
      bi_fc_e <- sf::st_as_sf(ices_rect_e)

      # ICES rectangles feature collection without NAs for bounding box
      bi_fc_No_NA_e <- na.omit(bi_fc_e)

      # create map
      map_e <- ggplot2::ggplot() +
        ggplot2::geom_polygon(
          data = RDBESvisualise::shoreline,
          ggplot2::aes(x = long, y = lat, group = group),
          color = "azure2",
          fill = "azure4",
          show.legend = FALSE
        ) +
        ggplot2::geom_sf(
          data = bi_fc_e,
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
            ")  vs Effort (",
            effortVariable,
            ") in ",
            y[i]
          )
        ) +
        ggplot2::coord_sf(
          xlim = c(sf::st_bbox(bi_fc_No_NA_e)[1], sf::st_bbox(bi_fc_No_NA_e)[3]),
          ylim = c(sf::st_bbox(bi_fc_No_NA_e)[2], sf::st_bbox(bi_fc_No_NA_e)[4]),
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
      legend_e <- biscale::bi_legend(
        pal = "GrPink",
        dim = 3,
        xlab = "Higher Sampling",
        ylab = "Higher Effort",
        size = 9
      )

      # combine map with legend
      finalPlot <- cowplot::plot_grid(map_e,
                                      legend_e,
                                      labels = NULL,
                                      rel_widths = c(2.5, 1),
                                      rel_heights = c(2.5, 1))
      x_e <- ggiraph::girafe(ggobj = finalPlot, width_svg = 6, height_svg = 6)
      x_e <- ggiraph::girafe_options(
        x_e,
        ggiraph::opts_zoom(min = .4, max = 2)
      )

      # Add our plot to the output list
      currentPlotCount <- length(all_plot)
      all_plot[[currentPlotCount + i]] <- x_e
    }

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

#' Internal function to filter landings data for coverageLandingsXXX functions
#'
#' @param landingsData A data table of landings data ("CL")
#' @param year Year to be assessed e.g 2021
#' @param quarter Quarter to be assessed - possible choices 1,2,3 or 4
#' @param vesselFlag Registered Country of Vessel - e.g "IE", "ES" or "FR"
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return A data table of effort data ("CE")
#'
filterEffortDataForCoverage <- function(effortData,
                                          year,
                                          quarter,
                                          vesselFlag,
                                          verbose){

  if (verbose) {
    print("Filtering effort data")
  }

  ef <- effortData

  if (is.na(year) == TRUE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef
    } else {
      ef1 <- ef %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == TRUE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
    } else {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == TRUE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(CEquar %in% quarter)
    } else {
      ef1 <- ef %>% dplyr::filter(CEquar %in% quarter)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  } else if (is.na(year) == FALSE && is.na(quarter) == FALSE) {
    if (is.na(vesselFlag) == TRUE) {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
      ef1 <- ef1 %>% dplyr::filter(CEquar %in% quarter)
    } else {
      ef1 <- ef %>% dplyr::filter(CEyear %in% year)
      ef1 <- ef1 %>% dplyr::filter(CEquar %in% quarter)
      ef1 <- ef1 %>% dplyr::filter(CEvesFlagCou %in% vesselFlag)
    }
  }

  # Return filtered data
  ef1

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

preprocessEffortDataForCoverage <- function(dataToPlot, verbose){

  if (verbose) {
    print("Preparing effort data")
  }

  # get effort
  ef <- dataToPlot[["CE"]] %>%
    dplyr::select(
      CEvesFlagCou:CEMonth,
      CEstatRect,
      CEmetier6,
      CEvesLenCat,
      CEnumFracTrips:CEnumUniqVes
    )
  ef$CEGear <- substr(ef$CEmetier6, 0, 3)

  # return our effort data
  ef

}






