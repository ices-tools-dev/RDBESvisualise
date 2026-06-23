#' Provides barplots ranging 0 – 100 % showing the percentage of the sampling method used.
#'
#' @param RDBESobj an RDBESDataObject)
#' @param displayPercentages logical, if TRUE the percentages are displayed on the bars
#' @param table one table from the RDBESDataObject)

pattern <- "selectionMethod"
exclusion_pattern <- "Cluster"

selectionMethod <- function(RDBESobj = myH1RawObject,
                           displayPercentages = FALSE){

# if(!table %in% names(RDBESobj)){
#   stop("The table provided is not in the RDBES object")
# }

  lapply(myH1RawObject, function(df) {
    # Find columns that match the pattern but do not contain the exclusion pattern
    cols <- grep(pattern, names(df), value = TRUE)
    cols <- cols[!grepl(exclusion_pattern, cols)]  # Exclude columns that match the exclusion pattern

    # Check if there are any matching columns after exclusion
    if (length(cols) > 0) {
      # Convert each column to character to avoid data type mismatches
      df %>%
        select(all_of(cols)) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = everything(), names_to = "column", values_to = "unique_value") %>%
        count(column, unique_value) %>% # TODO  Check - Ok matches the number of rows
        mutate(hierarchy = list_index)  # Add list index as a separate column
    } else {
      NULL
    }
  })


colName = paste(table, 'selectMeth', sep = '')

if(!colName %in% names(RDBESobj[[table]])){
  stop("The selection method column is not in the table provided")
}

RDBESobj[[table]] %>%
  count(!!sym(colName)) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = !!sym(colName))) +
  geom_bar(stat="identity")+
  ylab("%") +
  xlab(table) +
  ggtitle("Percentage of the sampling method used")+
  scale_fill_manual(
    values = RDBESvisualise:::paletteForPlotting
  ) +
  theme_bw() -> myPlot

if(displayPercentages){
  myPlot + geom_text(aes(label = paste0(round(pct, 1), "%")),
                    position = position_stack(vjust = 0.5)) -> myPlot
}

myPlot

}


testData <- RDBEScore::createRDBESDataObject(
  input = "data-raw/exampleData/wszystko/2025_10_14_125156.zip",Hierarchy =1, strict = FALSE)

# selectionMethod(RDBESobj = testData,
#                 displayPercentages = FALSE,
#                 table = 'BV')
