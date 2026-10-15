#' Summarise the sampled and not sampled units
#'
#' Shows, for each table, the percentage of units that were sampled or not
#' (`<table>samp`) and, for the units that were not sampled, the percentage of
#' each reason for not sampling (`<table>noSampReason`).
#'
#' @param RDBESDataObject An RDBESDataObject
#' @param tables (Optional) The tables to summarise, e.g. `c("VS", "FT")`.
#' Default is all tables with data that have a `samp` column.
#' @param output_type (Optional) "plot" (default) or "table"
#'
#' @return For `output_type = "table"` a data frame with the columns `table`,
#' `value`, `n`, `percentage` and `variable` ("Sampled" or "Reason for not
#' sampling"). Missing and empty values are shown as "(missing)". For
#' `output_type = "plot"` a ggplot2 plot with stacked bars of the percentages.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' - Notes/scope: Replaces the draft `SampledNonSampled()` function, which did
#'   not load (#30, #13). Counts checked against `table()` of the RDBEScore
#'   H1 example data.
#' @export
#'
#' @examples
#' summariseSampledNonSampled(RDBEScore::H1Example, output_type = "table")
#' summariseSampledNonSampled(RDBEScore::H1Example, tables = c("VS", "SA"))
summariseSampledNonSampled <- function(RDBESDataObject,
                                       tables = NULL,
                                       output_type = c("plot", "table")) {
  output_type <- match.arg(output_type)

  sampled <- countTableValues(RDBESDataObject, tables, "samp")
  sampled$variable <- rep("Sampled", nrow(sampled))
  # Only the units that were not sampled have a reason for not sampling
  reasons <- countTableValues(RDBESDataObject, unique(sampled$table),
                              "noSampReason", notSampledOnly = TRUE)
  if (!is.null(reasons)) {
    reasons$variable <- "Reason for not sampling"
  }
  counts <- rbind(sampled, reasons)

  if (output_type == "table") {
    return(counts)
  }
  counts$variable <- factor(counts$variable,
                            levels = c("Sampled", "Reason for not sampling"))
  plotTablePercentages(counts, "") +
    ggplot2::facet_wrap(ggplot2::vars(.data$variable)) +
    ggplot2::labs(title = "Sampled and not sampled units")
}
