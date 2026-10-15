#' Summarise the selection methods
#'
#' Shows, for each table, the percentage of units selected with each selection
#' method (`<table>selectMeth`).
#'
#' @param RDBESDataObject An RDBESDataObject
#' @param tables (Optional) The tables to summarise, e.g. `c("VS", "FT")`.
#' Default is all tables with data that have a `selectMeth` column.
#' @param output_type (Optional) "plot" (default) or "table"
#'
#' @return For `output_type = "table"` a data frame with the columns `table`,
#' `value` (the selection method), `n` and `percentage`. Missing and empty
#' values are shown as "(missing)". For `output_type = "plot"` a ggplot2 plot
#' with stacked bars of the percentages.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' - Notes/scope: Counts checked against `table()` of the RDBEScore H1 and H8
#'   example data (#12).
#' @export
#'
#' @examples
#' summariseSelectionMethod(RDBEScore::H1Example, output_type = "table")
#' summariseSelectionMethod(RDBEScore::H8ExampleEE1)
summariseSelectionMethod <- function(RDBESDataObject,
                                     tables = NULL,
                                     output_type = c("plot", "table")) {
  output_type <- match.arg(output_type)

  counts <- countTableValues(RDBESDataObject, tables, "selectMeth")

  if (output_type == "table") {
    return(counts)
  }
  plotTablePercentages(counts, "Selection method") +
    ggplot2::labs(title = "Selection methods used")
}
