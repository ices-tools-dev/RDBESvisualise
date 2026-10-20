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
#' and returns them invisibly as a list: table, plots, statsTable, statsFigures,
#' tableFigures.
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
#' summaryBiologicalVar(data,verbose=TRUE,var="Age",by="DEhierarchy",year=2022,outputType="table",statistics=TRUE)
#'}
#'
#'@export
#'
summaryBiologicalVar <- function(data,
                                 verbose = FALSE,
                                 var,
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
    if (!var %in% data$BV$BVtypeMeas) {
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


  missing_years <- year[!year %in% data$DE$DEyear]

  if (length(missing_years) > 0) {
    warning(paste(
      "The given year(s) --->",
      paste(missing_years, collapse = ", "),
      "<--- are not present in the data."
    ))
  }

  #filter RDBESDataObejct
  data <- RDBEScore::filterRDBESDataObject(
    data,
    fieldsToFilter = "DEyear",
    valuesToFilter = year,
    killOrphans = TRUE,
    verbose = verbose
  )

  byValues <- unique(data[[substr(by, 1, 2)]][[by]])

  plotData <- RDBEScore::getLinkedDataFromLevel(by, byValues, data, "BV", verbose = verbose)

  plotData <- plotData[BVtypeMeas == var, ]

  #range for weights
  if(var %in% c("WeightLive","WeightMeasured")){

    plotData[, BVvalueMeas := cut(as.numeric(BVvalueMeas), breaks = 15) ]
  }

  plotData <- plotData[, BVvalueMeas := factor(BVvalueMeas, levels = sort(unique(as.numeric(as.character(BVvalueMeas)))))]

  #Counting
  subset <- plotData[, .(nIndividuals = .N), by = c(by, "BVvalueMeas")]

  #add info about hierarchy
  subset$hierarchy <- hierarchy


  if (outputType == 'table') {
    print(subset)
  }

  if(outputType == 'plot'){
    plot <- ggplot2::ggplot(subset, ggplot2::aes(x = BVvalueMeas, y = nIndividuals, fill = BVvalueMeas)) +
      ggplot2::geom_bar(stat = "identity",fill = colourPalette[1], color = "black") +
      ggplot2::labs(title = paste0("Number of individuals by ", var," and ", by),
                    x = var,
                    y = 'Number of individuals',
                    caption = paste0("* Data from ",paste(year, collapse = ", "))) +
      ggplot2::facet_wrap(~get(by))+
      ggplot2::theme_light()
    print(plot)
  }

  if(statistics){
    max_oy <- max(subset$nIndividuals)
    min_oy <- min(subset$nIndividuals)

    p<- ggplot2::ggplot(subset, ggplot2::aes(BVvalueMeas, nIndividuals)) +
      ggplot2::geom_boxplot(fill = colourPalette[1],color="black")+
      ggplot2::facet_wrap(~get(by))+
      stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black")+
      theme_bw()+
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(colour="black",size = 11),
            axis.text.y = element_text(colour="black",size = 11),
            axis.title.x = element_blank(),
            axis.title.y=element_text(size=13, vjust=0.3),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            strip.background = element_rect(colour = "black", fill="grey", size=0.5),
            strip.text.x=element_text(size=11, vjust=0.3)
      )

    statsTable <- tableGrob(plotData)

    a<-grid.arrange(p, statsTable, nrow = 2, heights = c(2, 1))

  }
  }
