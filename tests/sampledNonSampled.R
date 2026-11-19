#' Provides graphical outputs to determine a percentage of sampled data
#' @param RDBESDataObject RDBESDataObject.
#' @param groupBy  vector of grouping variables
#' @param verbose (Optional) Set to TRUE if you want informative text on
#' @example
#' RDBESDataObject <- createRDBESDataObject(input = "data-raw/exampleData/CS_H1_myData.zip")
#' res <- sampledOrNotsampled(RDBESDataObject)
#' res
#' res1 <- sampledOrNotsampled(RDBESDataObject, "FOarea")
#' res1



SampledNonSampled <- function(RDBESDataObject,
                            groupBy = NA,
                             verbose = FALSE) {
  
  require(ggplot2)
  require(tibble)
  require(data.table)
  require(dplyr)
  require(purrr)

  
  # delete orphans
  
  RDBESDataObject <- RDBEScore::findAndKillOrphans(RDBESDataObject)
  
 # all columns ending with "id" and containing only "NA" will be deleted
  
  RDBESDataObject <-  lapply(RDBESDataObject, function(x) 
   {
    if (!is.null(x)) { 
     x%>%dplyr::select(-(ends_with("id") & where(~ all(is.na(na_if(as.character(.x), ""))))))
    } 
    })


  # check hierarchy
  
  H <- unique(RDBESDataObject[["DE"]]$DEhierarchy)
  
  if (verbose) {
    print(paste0("Upper hierarchy: ", H))
  }
  
  
  # other hierarchies - in progress
  if (H==1)  required.tables.list <- c("DE", "SD", "VS", "VD", "FT", "FO", "SS", "SA") else 
    if (H==2) required.tables.list <- c("DE", "SD", "VD", "FT", "FO", "SS", "SA")   else stop("Only upper hierarchies 1 and 2 are included.")
  

  groupByStandard <- c("DEyear", "DEhierarchy") ### takes "DEhierarchy" and "DEyear" always in groupBy
  if (all(is.na(groupBy))) groupBy <- groupByStandard else groupBy <- union(groupByStandard, groupBy)

  #' 
  #'  Internal function prop() produces a table showing the percentage of sampled units in the table RDBESDataObject[[y]]:
  #' @param y  RDBESDataObject table name, e.g. "FO", "SA" etc.
  #' @param tables.ordered ordered vector of names of all tables related to upper hierarchy. The last table is "SA". E.g. for upper hierarchy 1, c("DE", "SD", "VS", "VD", "FT", "FO", "SS", "SA")
  #'
    prop <- function(y, tables.ordered) { 
      index <- which(tables.ordered == y)
      sublist <- lapply(tables.ordered[1:index], function(u) RDBESDataObject[[u]])  ### list of tables required
      reversed_sublist <- rev(sublist)
      MasterTable <- purrr::reduce(reversed_sublist, ~ suppressMessages(left_join(.x, .y)))
      
      if (nrow(RDBESDataObject[[y]]%>%select((ends_with("samp") & !contains(c("num", "time")))))>0)
      {
    
      if (y!="DE")
      {
      groupBytable <- intersect(groupBy, names(MasterTable))
      table <- MasterTable %>%
        dplyr::select(all_of(groupBytable), (ends_with("samp") & !contains(c("num", "time"))) | 
                        ends_with("noSampReason") | ends_with("noSampReasonFM") | ends_with("noSampReasonBV"))%>%
        dplyr::mutate(across(contains("noSampReason"), ~replace(., is.na(.) | .=="", "Unknown")))%>%
        dplyr::select(all_of(groupBytable), starts_with(y))%>%
        dplyr::group_by_all()%>%
        count()%>%
        as_tibble()%>%
        dplyr::mutate(across(contains("noSampReason"), ~ if_else(if_any(contains("samp"), ~.x == "Y"), NA, .x)))%>%
        as.data.table()%>%
        dplyr::group_by(across(all_of(groupBytable)))%>%
        mutate(percentage = round(n/sum(n), 2)*100)

      
      
      } else
      {
        table <- MasterTable %>%
          dplyr::select(DEyear, DEhierarchy, (ends_with("samp") & !contains(c("num", "time"))) | ends_with("noSampReason"))%>%
          dplyr::mutate(across(contains("noSampReason"), ~replace(., is.na(.) | .=="", "NotIndicated")))%>%
          dplyr::select("DEyear", "DEhierarchy", starts_with(y))%>%
          dplyr::group_by_all()%>%
          dplyr::count()%>%
          as_tibble()%>%
          dplyr::mutate(across(contains("noSampReason"), ~ if_else(if_any(contains("samp"), ~.x == "Y"), NA, .x)))%>%
          as.data.table()%>%
          dplyr::group_by(DEyear, DEhierarchy)%>%
          dplyr::mutate(percentage = round(n/sum(n), 2)*100)
      }
        
        
      }
        
      
    }

    
      
 # Here a list of tables showing the percentage of sampled units, is generated

  list.prop <-   compact(lapply(required.tables.list, prop, tables.ordered = required.tables.list)) #### list of tables
    
  list.prop.names <- unlist(lapply(list.prop, function(x) { paste0(substr(names(x)[ncol(x)-2], 1, 2), "_units_sampled") } ))  ### names of tables in the list

  names(list.prop) <- list.prop.names
  


return(list.prop)

}





