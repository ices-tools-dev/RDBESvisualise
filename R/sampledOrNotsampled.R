#' Provides graphical outputs to determine a percentage of sampled data
#' @param RDBESDataObject RDBESDataObject.
#' @param groupBy  vector of grouping variables
#' @param verbose (Optional) Set to TRUE if you want informative text on
#' @example
#' RDBESDataObject <- createRDBESDataObject(input = "data-raw/exampleData/CS_H1_DE_23-24_corr.zip")
#' res <- sampledOrNotsampled(RDBESDataObject)
#' print(res)

library(ggplot2)
library(tibble)
library(data.table)
library(dplyr)
library(purrr)
library(RDBESvisualise)
library(RDBEScore)

#RDBESDataObject <- RDBEScore::createRDBESDataObject(input  = "C:/Users/wischnewski/WGRDBES-EST/data/CS_H1_DE_23-24_corr.zip")



SampledNonSampled <- function(RDBESDataObject,
                            groupBy = NA,
                             verbose = FALSE) {
  
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
        dplyr::select(all_of(groupBytable), (ends_with("samp") & !contains(c("num", "time"))) | ends_with("noSampReason"))%>%
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
  
  
  
  # Interbal function for plotting: percentage of sampled/nonsampled + percentage of reason for not sampling
  
  plotting_percentange <- function(table)
  {
  groupBytable <- intersect(groupBy, names(table))
  groupping <- "group_label"
  
  plot_no_sampled_reasons <- table %>%
    filter(across(ends_with("samp")) == "N") %>%
    group_by(across(all_of(groupBytable)), across(ends_with("samp")), across(ends_with("noSampReason"))) %>%
    summarise(n = sum(n), .groups = 'drop')%>%
    group_by(across(all_of(groupBytable)))%>%
    mutate(p = round(n/sum(n), 2))%>%
    as_tibble() %>%
    mutate("{groupping}" := across(ends_with("noSampReason"))) %>%
    as.data.table()%>%
    arrange(across(all_of(groupBytable)),across(ends_with("samp")), across(ends_with("noSampReason")))
  
  plot_sampled_yes_no <- table %>%
    group_by(across(all_of(groupBytable)), across(ends_with("samp"))) %>%
    summarise(n = sum(n), .groups = 'drop')%>%
    group_by(across(all_of(groupBytable)))%>%
    mutate(p = round(n/sum(n), 2))%>%
    as_tibble() %>%
    mutate("{groupping}" := across(ends_with("samp"))) %>%
    as.data.table()%>%
    arrange(across(all_of(groupBytable)),across(ends_with("samp")))
  
  
#  p1 <- ggplot(..., aes(x = group_label, y = p, fill = group_label)) +
 #   geom_col(show.legend = FALSE) +
  #  geom_text(
   #   aes(label = scales::percent(p, accuracy = 1)),
  #    vjust = -0.5, size = 4) + 
#    scale_y_continuous(labels = scales::percent) +
 #   labs(
#      title = "Percentage of Y/N sampled",
 #     x = "Y/N",
#      y = "Percentage"
 #   ) +
#    theme_minimal() +
#    theme(legend.position = "none")
  
#  p2 <- ggplot(... , aes(x = group_label, y = p, fill = group_label)) +
  #  geom_col(show.legend = FALSE) +
  #  geom_text(
  #    aes(label = scales::percent(p, accuracy = 1)),
  #    vjust = -0.5, size = 4) +
  #  scale_y_continuous(labels = scales::percent) +
  #  labs(
  #    title = "Percentage of Reasons for not sampling",
  #    x = "Reason for not sampling",
  #    y = "Percentage"
  #  ) +
  #  theme_minimal() +
  #  theme(legend.position = "none")
  
  #plot_grid(p1, p2)
  }


#lapply(list.prop, plotting_percentange)

return(list.prop)

}

res <- SampledNonSampled(RDBESDataObject)
print(res)

res1 <- SampledNonSampled(RDBESDataObject, groupBy=c("FOarea"))
print(res1)

#res2 <- SampledNonSampled(RDBESDataObject, groupBy=c("FOarea", "VDlenCat"))
#print(res2)



