#' Provides graphical outputs to determine a percentage of sampled data.
#' @param RDBESDataObject An RDBESDataObject.
#' @param year Selected Year/Years. The default is NA, i.e. all Years presented in RDBESDataObject.
#' @param vesselFlag Selected Country. The default is NA, i.e. all Countries presented in RDBESDataObject.
#' @param verbose (Optional) Set to TRUE if you want informative text on
#' validation and function steps printed out, or FALSE if you don't.
#' The default is FALSE.
#' @example
#' RDBESDataObject <- createRDBESDataObject(input = "data-raw/exampleData/H1_2024_10_14.zip")
#' res <- sampledOrNotsampled(RDBESDataObject)
#' print(res)

#RDBESDataObject <- RDBEScore::createRDBESDataObject(input  = "C:/Users/wischnewski/WGRDBES-EST/data/ZW_data")
#RDBESDataObject <- RDBEScore::createRDBESDataObject(input  = "C:/Users/wischnewski/WGRDBES-EST/data/German_NS_H1_data")

sampledOrNotsampled <- function(RDBESDataObject,
                                year = NA, 
                                vesselFlag = NA,
                             verbose = FALSE) {
  
## Filter the input data. At present, only year and country are considered for filtering
if (any(!is.na(c(year, vesselFlag))))
{
  filterDataFields <- c("DEyear","SDctry")[which(!is.na(c(year, vesselFlag)))]
  filterDataValues <- c(na.omit(c(year, vesselFlag)))
    
  RDBESDataObject <- RDBEScore::filterRDBESDataObject(RDBESDataObject,
  fieldsToFilter = filterDataFields,
  valuesToFilter = filterDataValues)
}
  
### all columns ending with "id" and containing only "NA" will be deleted
  
  RDBESDataObject <-  lapply(RDBESDataObject, function(x) 
   {
    if (!is.null(x)) { 
     x%>%dplyr::select(-(ends_with("id") & where(~ all(is.na(na_if(as.character(.x), ""))))))
    } 
    } )


  
  #check hierarchy
  H <- unique(RDBESDataObject[["DE"]]$DEhierarchy)
  
  if (verbose) {
    print(paste0("Upper hierarchy: ", H))
  }
  
  if (H %in% c(1)) {
  
  #### now for H1 only!
  
names.tables.H1 <- as.list(c("DE", "VS", "FT", "FO","SA"))
  
RDBESobject1 <- merge(RDBESDataObject[["SA"]], RDBESDataObject[["SS"]], by=c("SSid"),all.x=TRUE)
RDBESobject2 <- merge(RDBESobject1, RDBESDataObject[["FO"]], by=c("FOid"),all.x=TRUE)
RDBESobject3 <- merge(RDBESobject2, RDBESDataObject[["FT"]], by=c("FTid"),all.x=TRUE)
RDBESobject4 <- merge(RDBESobject3, RDBESDataObject[["VD"]], by=c("VDid"),all.x=TRUE)
RDBESobject5 <- merge(RDBESobject4, RDBESDataObject[["VS"]], by=c("VDid","VSid"),all.x=TRUE)
RDBESobject6 <- merge(RDBESobject5, RDBESDataObject[["SD"]], by=c("SDid"),all.x=TRUE)
masterTable <- merge(RDBESobject6, RDBESDataObject[["DE"]], by=c("DEid"),all.x=TRUE)

}

prop <- function(y) {masterTable %>%
    dplyr::select(DEyear, DEhierarchy, (ends_with("samp") & !contains(c("num", "time"))) | ends_with("noSampReason"))%>%
    dplyr::mutate(across(contains("noSampReason"), ~replace(., is.na(.) | .=="", "NotIndicated")))%>%
    dplyr::group_by(pick("DEyear","DEhierarchy", starts_with(y)))%>%
    dplyr::summarise(Number_of_sampled_units = n(), .groups = "keep")%>%
    dplyr::group_by(DEyear,DEhierarchy) %>%
    dplyr::mutate(Percentage = Number_of_sampled_units / sum(Number_of_sampled_units)*100)%>%
    dplyr::arrange(DEyear)}

### don't know how to change "NotIndicated" to NA if in the Yes/No column we have "Y"

list.prop <- lapply(names.tables.H1, prop)

## list of tables, which shows a percentage of sampled/unsampled units: in DE, VS, FT, FO, SA

list.prop <- setNames(list.prop, paste0(names.tables.H1,"_percentage"))

# doesn't work yet
#plot_prop <- function(g) {xx <- (g%>%ungroup()%>%select(ends_with("samp")));
#yy <- colnames(g%>%ungroup()%>%select(ends_with("tage")));
#ggplot(g, aes(x=xx, y = yy, 
 #             fill=xx)) +
  #  geom_col(position = "dodge") + 
  #facet_wrap(~DEyear) + 
   # labs(y = "% Sampled/Not Sampled", x = "Yes/No")
#}

return(list.prop)

}


res <- sampledOrNotsampled(RDBESDataObject)
print(res)

