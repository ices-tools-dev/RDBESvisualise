#' Summarise the biological variables
#'
#' Counts the individuals with each type of biological measurement
#' (`BVtypeMeas`, e.g. measured, weighed or aged individuals) and summarises
#' the measured values.
#'
#' @param RDBESDataObject An RDBESDataObject with a single hierarchy
#' @param var (Optional) The biological variables to summarise. Each is matched
#' to the start of `BVtypeMeas`, e.g. "Length" matches "LengthTotal". Default
#' is all of "Length", "Weight", "Age", "Sex" and "Maturity".
#' @param by (Optional) Columns to split the data by, e.g. "SAspeCode" or
#' "DEyear". Any column of the tables above BV in the hierarchy can be used.
#' Default is NULL.
#' @param year (Optional) Years (`DEyear`) to include. Default is all years.
#' @param output_type (Optional) "table" (default) or "plot"
#'
#' @return For `output_type = "table"` a data frame with one row per
#' `BVtypeMeas` (and `by`), the number of individuals (`n`) and, for numeric
#' variables, the `mean`, `sd`, `median` and `IQR` of `BVvalueMeas`. For
#' `output_type = "plot"` a list of ggplot2 plots named by `BVtypeMeas`: a
#' histogram with the summary statistics for numeric variables and a bar plot
#' for categorical variables.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' - Notes/scope: Tested with the RDBEScore 0.4.0 example data for
#'   hierarchies 1, 7 and 8 (#14). Statistics checked against direct
#'   calculations on the BV table.
#' @export
#'
#' @examples
#' summariseBiologicalVariables(RDBEScore::H8ExampleEE1)
#' plots <- summariseBiologicalVariables(RDBEScore::H8ExampleEE1,
#'   var = c("Length", "Sex"), output_type = "plot"
#' )
#' plots$LengthTotal
summariseBiologicalVariables <- function(RDBESDataObject,
                                         var = c("Length", "Weight", "Age",
                                                 "Sex", "Maturity"),
                                         by = NULL,
                                         year = NULL,
                                         output_type = c("table", "plot")) {
  var <- match.arg(var, several.ok = TRUE)
  output_type <- match.arg(output_type)

  hierarchy <- unique(RDBESDataObject[["DE"]]$DEhierarchy)
  if (length(hierarchy) != 1) {
    stop("RDBESDataObject must contain a single hierarchy")
  }
  if (is.null(RDBESDataObject[["BV"]]) || nrow(RDBESDataObject[["BV"]]) == 0) {
    stop("No BV data in RDBESDataObject")
  }

  # One row per BV record, with the columns of the tables above it
  bv <- as.data.frame(RDBEScore::createRDBESEstObject(RDBESDataObject,
                                                      hierarchy))
  bv <- bv[!is.na(bv$BVid) & !duplicated(bv$BVid), ]
  if (!all(by %in% names(bv))) {
    stop("Column(s) not found: ", paste(setdiff(by, names(bv)), collapse = ", "))
  }
  if (!is.null(year)) {
    bv <- bv[bv$DEyear %in% year, ]
  }
  bv$BVtypeMeas <- as.character(bv$BVtypeMeas)
  bv <- bv[Reduce(`|`, lapply(var, function(v) startsWith(bv$BVtypeMeas, v))), ]
  if (nrow(bv) == 0) {
    stop("No BV data for var = ", paste(var, collapse = ", "))
  }

  # Length, weight and age are summarised as numbers. Other variables (e.g.
  # maturity stages coded as numbers) and variables with values that are not
  # numbers are summarised as categories.
  bv$BVvalueMeas <- as.character(bv$BVvalueMeas)
  bv$value <- suppressWarnings(as.numeric(bv$BVvalueMeas))
  hasValue <- !is.na(bv$BVvalueMeas) & bv$BVvalueMeas != ""
  isMeasurement <- Reduce(`|`, lapply(c("Length", "Weight", "Age"),
                                      function(v) startsWith(bv$BVtypeMeas, v)))
  notNumeric <- unique(bv$BVtypeMeas[!isMeasurement | (hasValue & is.na(bv$value))])
  bv$value[bv$BVtypeMeas %in% notNumeric] <- NA

  groups <- c("BVtypeMeas", by)
  groupData <- split(bv, do.call(paste, bv[groups]), drop = TRUE)
  summaryStats <- do.call(rbind, lapply(groupData, function(d) {
    numeric <- !d$BVtypeMeas[1] %in% notNumeric
    data.frame(d[1, groups, drop = FALSE],
               n = nrow(d),
               mean = if (numeric) mean(d$value, na.rm = TRUE) else NA,
               sd = if (numeric) stats::sd(d$value, na.rm = TRUE) else NA,
               median = if (numeric) stats::median(d$value, na.rm = TRUE) else NA,
               IQR = if (numeric) stats::IQR(d$value, na.rm = TRUE) else NA)
  }))
  rownames(summaryStats) <- NULL

  if (output_type == "table") {
    return(summaryStats)
  }

  plots <- lapply(split(bv, bv$BVtypeMeas), function(d) {
    type <- d$BVtypeMeas[1]
    p <- ggplot2::ggplot(d) +
      ggplot2::labs(title = type, y = "Number of individuals",
                    x = paste(unique(d$BVvalUnitScale), collapse = ", ")) +
      ggplot2::theme_bw()
    if (type %in% notNumeric) {
      p <- p + ggplot2::geom_bar(ggplot2::aes(x = .data$BVvalueMeas))
    } else {
      typeStats <- summaryStats[summaryStats$BVtypeMeas == type, ]
      typeStats$label <- paste0("n = ", typeStats$n,
                                "\nmean = ", signif(typeStats$mean, 3),
                                "\nsd = ", signif(typeStats$sd, 3),
                                "\nmedian = ", signif(typeStats$median, 3),
                                "\nIQR = ", signif(typeStats$IQR, 3))
      p <- p +
        ggplot2::geom_histogram(ggplot2::aes(x = .data$value), bins = 30) +
        ggplot2::geom_label(data = typeStats,
                            ggplot2::aes(label = .data$label),
                            x = Inf, y = Inf, hjust = 1.05, vjust = 1.05,
                            size = 3)
    }
    if (!is.null(by)) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(by)),
                                   scales = "free_y")
    }
    p
  })
  plots
}
