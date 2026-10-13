#'Function which shows tabel and/or plot with summary of Biological Variables
#'
#' @param data RDBES Data Obejct A masterTable with SA,DE,SD,FM,BV data.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param var One biological variable ex. Length, WeightLive, Age, Sex, Maturity.
#' @param by One factor to split the data.
#' @param year Year or years to filter the data. If you don't add information,
#' the default will by all years occures in data.
#' @param outputType Data should be present as table and/or plot.
#' @param statistics (Optional) Set to TRUE if you want additional summary
#' statistics (mean, sd, median, IQR… ). Default is FALSE
#'
#' @return Function returns results depending of output_type defined in parameters.
#'
#' @section Development review:
#' - AI-assisted: No
#' - Review: Kasia-MIR
#' - Notes/scope: Tested with hierarchy H1 using 2025 RDBES data (downloaded 2026-10-13).
#'
#' @example
#' \dontrun{
#' #perpare RDBES Data Object for all hierarchies:
#' data <- createRDBESDataObject(input = "data-raw/exampleData/H1_2026_10_08.zip")
#' #RDBESDataObjectH2 <- createRDBESDataObject(input = "data-raw/exampleData/H2_2025_10_13_160257.zip")
#'
#'
#'
#' #to create plots and/or tables use
#' summaryBiologicalVar(data,verbose,var="Age",by="DEhierarchy",year=2024,output_type,statistics)
#'}
var<-'Age'
by <-"SDctry"
year<-2025
verbose <-TRUE
summaryBiologicalVar <- function(data,
                                 verbose = TRUE,
                                 var,
                                 by,
                                 year,
                                 outputType,
                                 statistics) {

  #required packages to run function
  require(RDBEScore)
  require(data.table)
  require(ggplot2)
  require(gridExtra)

  #validate RDBES Data Object
  validateRDBESDataObject(data, verbose = verbose, strict = TRUE)

  #check arguments of function
  if (!var %in% data$BV$BVtypeMeas) {
    stop(
      paste(
        'The given measure type --->',
        var,
        '<--- is not present in the data. In your data is:',
        paste(unique(data$BVtypeMeas), collapse = ', ')
      )
    )
  }

  if (!year %in% data$DE$DEyear) {
    stop(paste(
      'The given measure type --->',
      year,
      '<--- is not present in the data.'
    ))
  }

  #palette
  ###tu trzeba zmienic by czytało wprost z paczki
  color <- readLines("../RDBESvisualise/data/colorCountryColScale.txt")

  #filter RDBESDataObejct
  data <- RDBEScore::filterRDBESDataObject(
    data,
    fieldsToFilter = "DEyear",
    valuesToFilter = year,
    killOrphans = TRUE,
    verbose = verbose
  )

  byValues <- unique(data[[substr(by, 1, 2)]][[by]])

  plotData <- getLinkedDataFromLevel(by, byValues, data, "BV", verbose = verbose)

  plotData <- plotData[BVtypeMeas == var, ]

  #range for weights
  if(var == "WeightLive"){

    plotData[, BVvalueMeas := cut(as.numeric(BVvalueMeas), breaks = 15) ]
  }

  plotData <- plotData[, BVvalueMeas := factor(BVvalueMeas, levels = sort(unique(as.numeric(as.character(BVvalueMeas)))))]

  #Counting
  subset <- plotData[, .(nIndividuals = .N), by = c(by, "BVvalueMeas")]



  if (outputType == 'table') {
    print(subset)
  }

  if(outputType == 'plot'){
    plot <- ggplot(subset, aes(x = BVvalueMeas, y = nIndividuals)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = paste0("Number of individuals by ", var," and ", by), x = var, y = 'n individuals') +
      facet_wrap(~get(by))+
      scale_fill_manual(values = color)
    print(plot)
  }

  if(statistics){
    max_oy <- max(subset$nIndividuals)
    min_oy <- min(subset$nIndividuals)

    p<- ggplot(subset, aes(BVvalueMeas, nIndividuals)) +
      geom_boxplot(color="black")+
      facet_wrap(~get(by))+
      stat_summary(fun.y=mean, geom="point", shape=4, size=3, color="black", fill="black")+
      scale_y_continuous(limits = c(min_oy,max_oy))+theme_bw()+
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
