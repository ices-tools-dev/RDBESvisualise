#' Provides barplots of the inputted table column, summed by up til 4 factors,
#' inserted as the column names from the same table.
#'
#' @param RDBESobj an RDBESDataObject)
#' @param var one column name from the RDBESDataObject)
#' @param valBy one or more column name from the same table as the 'var' variable)
#' The order of the column names put in the 'valBy' determines how the plot is arranged
#' columns with many factorlevels can be put in the end to get a new plot pr level.
#' @param output_type Should the output be a bar plot or a table, written to the environment)

amountByGroup <- function(
    data,
    var,
    valBy = NULL,
    filters = NULL,
    output_type = c("plot", "table"),
    title = NULL,
    xlab = NULL,
    ylab = NULL,
    asPct = FALSE,        # if TRUE convert the summarized value to percent of total
    verbose = TRUE
) {

  # Dependencies
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Please install data.table")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2")
  library(data.table)
  library(ggplot2)

  output_type <- match.arg(output_type)

  # Basic checks
  if (missing(data)) stop("Please provide 'data' (one RDBES table, e.g. RDBESobj$CE).")
  if (missing(var) || !is.character(var) || length(var) != 1) stop("'var' must be a single column name (string).")
  if (is.null(valBy) || !is.character(valBy)) stop("'valBy' must be a character vector of grouping column names (at least one).")
  if (!all(valBy %in% names(data))) stop(paste("Not all 'valBy' columns are present in data. Missing:",
                                               paste(setdiff(valBy, names(data)), collapse = ", ")))

  dt <- as.data.table(data)

  # Apply filters (filters is a named list: list(ColName = value_or_vector, ...)
  if (!is.null(filters)) {
    if (!is.list(filters)) stop("'filters' must be a named list, e.g. list(CEYear = 2022, CLspecCode = 'COD')")
    for (col in names(filters)) {
      if (col %in% names(dt)) {
        vals <- filters[[col]]
        # allow NA in filters[[col]]? If NA, do nothing. Otherwise subset
        if (!(length(vals) == 1 && is.na(vals))) {
          dt <- dt[get(col) %in% vals]
          if (verbose) message(sprintf("Filter applied: %s in %s", paste(vals, collapse = ","), col))
        }
      } else {
        warning(sprintf("Filter column '%s' not found in data — ignored.", col))
      }
    }
  }

  if (nrow(dt) == 0) {
    warning("No rows left after applying filters. Returning empty table.")
    if (output_type == "table") return(dt[, ..c(valBy, var)])
    if (output_type == "plot") return(invisible(NULL))
  }

  # Decide summary mode:
  first2 <- substr(var, 1, 2)
  # CASE A: categorical occurrence counts for e.g. SA/BV
  if (first2 %in% c("SA", "BV")) {
    # make sure grouping includes var (count combos)
    groupCols <- unique(c(var, valBy))
    summary_dt <- dt[, .N, by = groupCols]
    setnames(summary_dt, "N", "Freq")
    summary_var <- "Freq"
  } else {
    # CASE B: numeric sum
    if (!is.numeric(dt[[var]])) {
      stop(sprintf("Variable '%s' is not numeric. For non-SA/BV columns var must be numeric to sum.", var))
    }
    groupCols <- valBy
    summary_dt <- dt[, .(value = sum(get(var), na.rm = TRUE)), by = groupCols]
    setnames(summary_dt, "value", var)
    summary_var <- var
  }

  # Optionally convert to percentages (percentage of the whole summary table total)
  if (asPct) {
    total_sum <- sum(summary_dt[[summary_var]], na.rm = TRUE)
    if (total_sum == 0) {
      warning("Total sum is 0 — percentages will be NA or 0.")
      summary_dt[[summary_var]] <- NA_real_
    } else {
      summary_dt[[summary_var]] <- 100 * summary_dt[[summary_var]] / total_sum
    }
    # adjust y label default if not provided
    if (is.null(ylab)) ylab <- "Percent (%)"
  }

  # If the user asked for the table, return it
  if (output_type == "table") {
    # Return a data.table (invisible) but also print a message if verbose
    if (verbose) message("Returning summary table (data.table).")
    return(summary_dt[])}
  else{
    print("Output Type Not Available")
    }


  # ---------- Plotting ----------
  # Prepare labels
  if (is.null(title)) title <- paste0(var, " by ", paste(valBy, collapse = ", "))
  if (is.null(xlab)) xlab <- valBy[1]
  if (is.null(ylab)) ylab <- summary_var

  # Convert grouping columns to factors for consistent plotting (preserve order)
  for (g in valBy) {
    if (!is.factor(summary_dt[[g]])) summary_dt[[g]] <- as.factor(summary_dt[[g]])
  }

  # Basic plot for first level grouping
  p_base <- ggplot(summary_dt, aes_string(x = valBy[1], y = summary_var)) +
    geom_bar(stat = "identity") +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Add faceting logic:
  if (length(valBy) == 1) {
    p <- p_base
    print(p)

  } else if (length(valBy) == 2) {
    # second var used as facet
    facet_formula <- as.formula(paste("~", valBy[2]))
    p <- p_base + facet_wrap(facet_formula, scales = "free_y")
    print(p)

  } else if (length(valBy) == 3 &&  #only use facet grid if factor level is fewer then 5, else loop over
             length(unique(summary_dt[[valBy[2]]])) < 5 &&
             length(unique(summary_dt[[valBy[3]]])) < 5) {
    # small factor levels -> facet_grid rows ~ cols
    facetf <- as.formula(paste(valBy[3], "~", valBy[2]))
    p <- p_base + facet_grid(facetf, scales = "free_y")
    print(p)

  } else if (length(valBy) > 2) {
    # fallback: loop over the last valBy
    lastVar <- valBy[length(valBy)]
    otherVars <- valBy[-length(valBy)]
    lv_levels <- sort(unique(summary_dt[[lastVar]]))

    for (lv in lv_levels) {
      sub_dt <- summary_dt[summary_dt[[lastVar]] == lv, ]
      if (nrow(sub_dt) == 0) next
      p_tmp <- ggplot(sub_dt, aes_string(x = otherVars[1], y = summary_var)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(title, " — ", lastVar, ": ", lv),
             x = xlab,
             y = ylab) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (length(otherVars) == 2) {
        p_tmp <- p_tmp + facet_wrap(as.formula(paste("~", otherVars[2])), scales = "free_y")
      } else if (length(otherVars) == 3 &&
                 length(unique(sub_dt[[otherVars[2]]])) < 5 &&
                 length(unique(sub_dt[[otherVars[3]]])) < 5) {
        p_tmp <- p_tmp + facet_grid(as.formula(paste(otherVars[3], "~", otherVars[2])), scales = "free_y")
      }

      print(p_tmp)
    }
    message("Multiple plots (one per level of the last grouping variable) were generated.")
  }

  invisible(summary_dt[])
}
