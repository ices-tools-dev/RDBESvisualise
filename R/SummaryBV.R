#'Function which shows tabel and/or plot with summary of Biological Variables
#'for one hierarchy
#'
#' @param data RDBES Data Obejct.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param var (Optional) One biological variable ex. Length, WeightLive,
#' WeightMeasured, Age, Sex, Maturity.
#' @param by One or more factors to split the data, ex. "SDctry" or
#' c("SDctry", "DEyear").
#' @param year (Optional) Year or years to filter the data. If you don't add
#' information, the default will by all years occures in data.
#' @param outputType Data should be present as table and/or plot. The default
#' is both: c("plot", "table").
#' @param statistics (Optional) Set to TRUE if you want additional summary
#' statistics (mean, sd, median, IQR… ). Default is FALSE.
#' @param bins (Optional) Number of bins in histograms. Default is 20.
#'
#' @return Function shows results depending on outputType defined in parameters
#' and returns them invisibly as a list: countTable, statsTable, plots,
#' tableFigures, statsFigures.
#'
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: Kasia-MIR
#' - Notes/scope: Tested with hierarchy H1 using 2021-2025 RDBES data (downloaded 2026-10-13).
#'
#' @examples
#' \dontrun{
#' data <- H8ExampleEE1 # dataset from RDBEScore
#'
#' #to create plots and/or tables use
#' summaryBiologicalVar(data,verbose=TRUE,var="Age",by=c("DEhierarchy","SDctry"),year=2022,outputType="table",statistics=TRUE)
#'}
#'
#'@export
#'
summaryBiologicalVar <- function(data,
                                 verbose = FALSE,
                                 var=NULL,
                                 by,
                                 year = NULL,
                                 outputType = c('plot','table'),
                                 statistics = FALSE,
                                 bins = 20) {

  #validate RDBES Data Object
  RDBEScore::validateRDBESDataObject(data, verbose = verbose, strict = TRUE)

  #BVid exists?
  if (is.null(data$BV) || nrow(data$BV) == 0) {
    stop('The data does not contain any BV (biological variable) records.')
  }

  #hierarchy
  hierarchy <- data$DE$DEhierarchy

  #check arguments of function
  if (missing(by) || !is.character(by)) {
    stop("'by' must be one or more column names, ex. 'SDctry'.")
  }

  by <- unique(by)

  for (b in by) {
    if (!b %in% names(data[[substr(b, 1, 2)]])) {
      stop(paste('The given column --->', b, '<--- is not present in the data.'))
    }
  }

  #check var argument in the function
  if (is.null(var)) {
    var <- unique(data$BV$BVtypeMeas)
  } else{
    if (!all(var %in% data$BV$BVtypeMeas)) {
      stop(
        paste(
          'The given measure type --->',
          var,
          '<--- is not present in the data. In your data is:',
          paste(unique(data$BV$BVtypeMeas), collapse = ', ')
        )
      )
    }
  }


  if (is.null(year)) {
    #all years, if year is not given
    year <- unique(data$DE$DEyear)
    message(paste(
      "The given year(s) --->",
      paste(sort(year), collapse = ", "),
      "<--- will be present in the data."
    ))
  } else {
    missing_years <- year[!year %in% data$DE$DEyear]

    if (length(missing_years) > 0) {
      warning(paste(
        "The given year(s) --->",
        paste(sort(missing_years), collapse = ", "),
        "<--- are not present in the data."
      ))
    }
  }


  #filter RDBESDataObejct
  data <- RDBEScore::filterRDBESDataObject(
    data,
    fieldsToFilter = "DEyear",
    valuesToFilter = year,
    killOrphans = TRUE,
    verbose = verbose
  )

  #get biological data linked with each "by" column
  plotData <- NULL

  #loop to get biological data if is more than one
  for (b in by){
  byValues <- unique(data[[substr(b, 1, 2)]][[b]])
  partData <- RDBEScore::getLinkedDataFromLevel(b, byValues, data, "BV", verbose = verbose)
  partData <- data.table::as.data.table(partData)

  if (is.null(plotData)) {
    plotData <- partData
  } else if(!b %in% names(plotData)) {
    plotData <- merge(plotData, partData[, c('BVid', b), with = FALSE], by = 'BVid')
  }
  }

  plotData <- plotData[BVtypeMeas %in% var, ]

  if (nrow(plotData) == 0) {
    stop('There are no records of the given measure type(s) for the given year(s).')
  }

  #numeric variables

  numericVar <- var[var %in% c(
    "WeightLive",
    "WeightMeasured",
    'ForkLength',
    'Age',
    'LengthCarapace',
    'LengthLowerJawFork',
    'LengthMantle',
    'LengthMaximumShell',
    'LengthPinchedTail',
    'LengthPreAnal',
    'LengthPreCaudal',
    'LengthStandard',
    'LengthTail',
    'LengthTotal',
    'LengthWingSpan',
    'WidthCarapace',
    'WidthMaximumShell'
  )]

  plotData[, valueNum := NA_real_]
  plotData[BVtypeMeas %in% numericVar, valueNum := suppressWarnings(as.numeric(BVvalueMeas))]

  #no numeric variables
  notNumber <- plotData$BVtypeMeas %in% numericVar & is.na(plotData$valueNum)

  #group used in the plots of statistics
  plotData[, group := do.call(paste, c(.SD, sep = '/')), .SDcols = by]

  #Counting
  subset <- plotData[, .(nIndividuals = .N), by = c(by, "BVtypeMeas")]

  #add info about hierarchy
  subset$hierarchy <- hierarchy

  caption = paste0("* Data from ",paste(year, collapse = ", "))

  #define list with results
  result <- list()

  if ('table' %in% outputType) {
    result$countTable <- subset
  }

  #check that statistics are available
  if (statistics & length(numericVar) == 0) {
    warning('Statistics are available only for numeric variables, statistics = FALSE is used.')
    statistics <- FALSE
  }
  #boxplot and table of statistics only for numeric variables
  if(statistics){

    statsTable <- plotData[BVtypeMeas %in% numericVar,
                           .(n = .N,
                             mean = round(mean(valueNum), 2),
                             sd = round(stats::sd(valueNum), 2),
                             median = stats::median(valueNum),
                             IQR = stats::IQR(valueNum),
                             min = min(valueNum),
                             max = max(valueNum)),
                           by = c('BVtypeMeas', by)]

    data.table::setorderv(statsTable, c('BVtypeMeas', by))

    result$statsTable <- statsTable
  }

  #Plots and statistics: one for each variable
  if ('plot' %in% outputType || statistics) {

    plots <- list()
    statsFigures <- list()
    tableFigures <- list()

    #look of the tables in the figures
    tableTheme <- gridExtra::ttheme_minimal(
      colhead = list(
        bg_params = list(
          fill = "grey85",
          col = "grey20"
        ),
        fg_params = list(
          col = "black",
          fontface = "plain"
        )
      ),
      core = list(
        bg_params = list(
          fill = "white",
          col = "gray20"
        ),
        fg_params = list(
          col = "black"
        )
      )
    )

    for (v in var[var %in% plotData$BVtypeMeas]) {

      varData <- plotData[BVtypeMeas == v, ]

      #plot with number of individuals
      if ('plot' %in% outputType) {

        if (v %in% numericVar) {

          plot <- ggplot2::ggplot(varData, ggplot2::aes(x = valueNum))

          if (v == 'Age') {

            plot <- plot + ggplot2::geom_histogram(binwidth = 1, center = 0,
                                                   fill = colourPalette[2],
                                                   color = "black")
            yLabel <- 'Number of individuals'

          }else{
            plot <- plot + ggplot2::geom_histogram(bins = bins,
                                                   fill = colourPalette[2],
                                                   color = "black")
            yLabel <- 'Number of individuals'
          }
        } else {

          #Counting

          plot <- ggplot2::ggplot(varData, ggplot2::aes(x = BVvalueMeas)) +
            ggplot2::geom_bar(fill = colourPalette[2], color = "black")
          yLabel <- 'Number of individuals'
        }

        plot <- plot +
          ggplot2::labs(title = paste0("Number of individuals by ", v, " and ", paste(by, collapse = " / ")),
                        x = v,
                        y = yLabel,
                        caption = caption) +
          ggplot2::facet_wrap(by) +
          ggplot2::theme_light()

        plots[[v]] <- plot
      }

      #boxplot and table of statistics (only for numeric variables)
      if (statistics & v %in% numericVar) {

        #boxplot of the values, cross is the mean
        p <- ggplot2::ggplot(varData, ggplot2::aes(x = group, y = valueNum)) +
          ggplot2::geom_boxplot(fill = colourPalette[2], color = "black") +
          ggplot2::stat_summary(
            fun = mean,
            geom = "point",
            shape = 4,
            size = 3,
            color = "black"
          ) +
          ggplot2::labs(y = v) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.background = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(colour = "black", size = 11),
            axis.text.y = ggplot2::element_text(colour = "black", size = 11),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_text(size = 13, vjust = 0.3),
            strip.text = ggplot2::element_text(
              face = "plain",
              size = 9,
              colour = "black"
            ),
            panel.border = ggplot2::element_rect(
              colour = "black",
              fill = NA,
              linewidth = 0.5
            )
          )

        #table of statistics for this variable
        varStats <- statsTable[BVtypeMeas == v, ]
        varStats[, BVtypeMeas := NULL]

        statsTableGrob <- gridExtra::tableGrob(varStats, rows = NULL, theme = tableTheme)

        if ('plot' %in% outputType) {

          #plot on the left, boxplot on the right, table of statistics below
          statsFigures[[v]] <- gridExtra::grid.arrange(plot + ggplot2::labs(caption = NULL),
                                                       p, statsTableGrob,
                                                       layout_matrix = rbind(c(1, 2),
                                                                             c(3, 3)),
                                                       heights = c(2, 1),
                                                       bottom = grid::textGrob(
                                                         caption,
                                                         x = grid::unit(1, "npc") - grid::unit(3, "mm"),
                                                         y = grid::unit(2, "mm"),
                                                         just = c("right", "bottom"),
                                                         gp = grid::gpar(fontsize = 10)
                                                       )
                                                       )
        } else {

          #boxplot with the table below
          statsFigures[[v]] <- gridExtra::grid.arrange(p, statsTableGrob,
                                                       nrow = 2,
                                                       heights = c(2, 1),
                                                       bottom = grid::textGrob(
                                                         caption,
                                                         x = grid::unit(1, "npc") - grid::unit(3, "mm"),
                                                         y = grid::unit(2, "mm"),
                                                         just = c("right", "bottom"),
                                                         gp = grid::gpar(fontsize = 10)
                                                       ))
        }

      } else if ('plot' %in% outputType & 'table' %in% outputType & !(v %in% numericVar)) {

        #plot on the top, table with number of individuals below

        countTableGrob <- gridExtra::tableGrob(subset[BVtypeMeas == v,
                                                      c(by, 'BVtypeMeas','nIndividuals'),
                                                      with = FALSE],
                                               rows = NULL, theme = tableTheme)

        tableFigures[[v]] <- gridExtra::grid.arrange(plot + ggplot2::labs(caption = NULL),
                                                     countTableGrob,
                                                     nrow = 2,
                                                     heights = c(2, 1),
                                                     bottom = grid::textGrob(
                                                       caption,
                                                       x = grid::unit(1, "npc") - grid::unit(3, "mm"),
                                                       y = grid::unit(2, "mm"),
                                                       just = c("right", "bottom"),
                                                       gp = grid::gpar(fontsize = 10)
                                                     ))

      }
    }

    if ('plot' %in% outputType) {
      result$plots <- plots
    }

    if (statistics) {
      result$statsFigures <- statsFigures
    }

    if (length(tableFigures) > 0) {
      result$tableFigures <- tableFigures
    }
  }
  return(result)
  }
