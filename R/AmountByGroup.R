#' Summarise one or more columns by grouping variables (numeric + categorical)
#'
#' `amountByGroup()` provides a convenient way to summarise data tables
#' that may contain both numeric and categorical variables. The function
#' supports flexible filtering, percentage conversion, grouped summaries,
#' and optional plotting. Designed for RDBES datasets or similar.
#'
#' @details
#' **Main features:**
#' * Works with both numeric and categorical variables.
#' * Numeric variables are summarised by `sum()`, with optional
#'   percentage conversion (`asPct = TRUE`).
#' * Categorical variables are summarised as frequency counts.
#' * Multiple variables can be summarised at once:
#'   - Numeric variables are aggregated together in *wide format*.
#'   - Categorical variables produce frequency tables by group.
#' * Filters can be applied prior to summarisation using a named list.
#' * Results can be optionally assigned to the global environment
#'   using `assign_to_global = TRUE`.
#' * Output options:
#'   - `"table"`: returns a data.table (or list of tables if mixed types).
#'   - `"plot"`: generates one or more ggplot2 visualisations.
#'
#' **Examples of usage:**
#' ```
#' # Summarise one numeric variable by year and flag
#' tbl1 <- amountByGroup(
#'   data = landings,
#'   var = "LiveWeight",
#'   valBy = c("Year", "FlagCountry"),
#'   filters = list(SpeciesGroup = "COD"),
#'   asPct = FALSE,
#'   output_type = "table"
#' )
#'
#' # Summarise a categorical variable (species name)
#' tbl2 <- amountByGroup(
#'   data = landings,
#'   var = "SpeciesName",
#'   valBy = "Harbour",
#'   output_type = "plot"
#' )
#'
#' # Summarise multiple numeric + categorical variables together
#' tbl3 <- amountByGroup(
#'   data = landings,
#'   var = c("Weight", "NumSamp", "SpeciesName"),
#'   valBy = c("Year", "FlagCountry"),
#'   output_type = "table"
#' )
#' ```
#'
#' **Note:** When both numeric and categorical variables are passed,
#' the output is a list with two components: `$numeric` and `$categorical`.
#'
#' @param data data.frame or data.table — the table to summarise.
#' @param var character vector — one or more column names to summarise.
#'   Can include both numeric and categorical variables.
#' @param valBy character vector — one or more grouping column names.
#' @param filters named list (optional) — subsets before summarising,
#'   e.g. `list(Year = 2022, Flag = c("XX","YY"))`.
#' @param output_type character, one of `"table"` or `"plot"` (default: `"table"`).
#' @param asPct logical — if `TRUE`, convert numeric summaries to percentages.
#' @param assign_to_global logical — if `TRUE`, assign output table to `.GlobalEnv`.
#' @param verbose logical — print progress messages (default `TRUE`).
#'
#' @return Invisibly returns either:
#'   - a single `data.table` (if only numeric or only categorical vars),
#'   - or a list with `$numeric` and `$categorical` components (if mixed).
#'
#' @export
amountByGroup <- function(
    data,
    var,
    valBy,
    filters = NULL,
    output_type = c("table", "plot"),
    asPct = FALSE,
    assign_to_global = FALSE,
    verbose = TRUE
) {
  # ---- dependencies ----
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Please install the 'data.table' package.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install the 'ggplot2' package.")
  library(data.table)
  library(ggplot2)

  # ---- argument checks ----
  output_type <- match.arg(output_type)
  if (missing(data)) stop("'data' must be provided.")
  if (missing(var) || !is.character(var)) stop("'var' must be a character vector.")
  if (missing(valBy) || !is.character(valBy) || length(valBy) < 1)
    stop("'valBy' must be a character vector of one or more grouping columns.")

  # helper infix for NULL filters
  `%||%` <- function(x, y) if (!is.null(x)) x else y
  dt <- as.data.table(data)

  # check all columns exist
  missing_cols <- setdiff(c(var, valBy, names(filters %||% list())), names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("The following columns are missing from 'data': %s",
                 paste(missing_cols, collapse = ", ")))
  }

  # ---- optional filters ----
  if (!is.null(filters)) {
    if (!is.list(filters)) stop("'filters' must be a named list, e.g. list(Year = 2022).")
    for (col in names(filters)) {
      if (col %in% names(dt)) {
        vals <- filters[[col]]
        dt <- dt[get(col) %in% vals]
        if (verbose) message(sprintf("Filter applied: %s in %s",
                                     paste(vals, collapse = ","), col))
      } else {
        warning(sprintf("Filter column '%s' not found in data — ignored.", col))
      }
    }
  }

  # empty after filters
  if (nrow(dt) == 0) {
    warning("No rows left after filtering.")
    return(invisible(data.table()))
  }
  # function for assigning to global environment (name uses exact var/valBy strings)
  assign_summary_global <- function(tbl, var_vec, valBy_vec) {
    obj_name <- paste0("tbl_", paste(var_vec, collapse = "_"), "_by_",
                       paste(valBy_vec, collapse = "_"))
    assign(obj_name, tbl, envir = .GlobalEnv)
    if (verbose) message("Assigned table to global environment as: ", obj_name)
    invisible(obj_name)
  }

  # ---- SINGLE-VARIABLE CASE ----------------------------------------------------------------
  if (length(var) == 1) {
    i <- var[1]

    if (is.numeric(dt[[i]])) {
      summary_dt <- dt[, .(value = sum(get(i), na.rm = TRUE)), by = valBy]
      setnames(summary_dt, "value", i)

      if (asPct) {
        tot <- sum(summary_dt[[i]], na.rm = TRUE)
        summary_dt[[i]] <- if (tot == 0) NA_real_ else 100 * summary_dt[[i]] / tot
      }

      if (output_type == "table") {
        if (assign_to_global) assign_summary_global(summary_dt, var, valBy)
        if (verbose) message("Returning numeric summary table.")
        return(invisible(summary_dt[]))
      }

      if (output_type == "plot") {
        xvar <- valBy[1]
        yvar <- i
        p <- ggplot(summary_dt, aes_string(x = xvar, y = yvar)) +
          geom_bar(stat = "identity") +
          labs(title = paste0(i, " by ", paste(valBy, collapse = ", ")),
               x = xvar, y = ifelse(asPct, paste0(i, " (%)"), i)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # ADD FACETING
        if (length(valBy) >= 2) {
          p <- p + facet_wrap(
            as.formula(paste("~", valBy[2])),
            scales = "free_y"
          )
        }


        print(p)
        if (verbose) message("Plot generated for numerical variable.")
        return(invisible(summary_dt[]))
      }

      stop("Unsupported output_type for numeric variable.")
    } else {
      groupCols <- unique(c(i, valBy))
      cnt_dt <- dt[, .N, by = groupCols]
      setnames(cnt_dt, "N", "Freq")

      if (output_type == "table") {
        if (assign_to_global) assign_summary_global(cnt_dt, var, valBy)
        if (verbose) message("Returning frequency table for categorical variable.")
        return(invisible(cnt_dt[]))
      }

      if (output_type == "plot") {
        xvar <- if (!(i %in% valBy)) valBy[1] else i
        p <- ggplot(cnt_dt, aes_string(x = xvar, y = "Freq")) +
          geom_bar(stat = "identity") +
          labs(title = paste0("Count of ", i, " by ", paste(setdiff(valBy, i), collapse = ", ")),
               x = xvar, y = "Freq") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        if (length(valBy) == 2 && !(i %in% valBy)) {
          p <- p + facet_wrap(as.formula(paste("~", valBy[2])), scales = "free_y")
        }
        print(p)
        if (verbose) message("Plot generated for categorical variable.")
        return(invisible(cnt_dt[]))
      }

      stop("Unsupported output_type for categorical variable.")
    }
  }

  # ---- MULTI-VARIABLE CASE ------------------------------------------------------------------------
  if (length(var) > 1) {
    num_vars <- names(Filter(is.numeric, dt[, ..var]))
    cat_vars <- setdiff(var, num_vars)
    summary_list <- list()

    # numeric vars → sum
    if (length(num_vars) > 0) {
      summary_list[["numeric"]] <- dt[, lapply(.SD, sum, na.rm = TRUE),
                                      by = valBy, .SDcols = num_vars]
      if (asPct) {
        for (i in num_vars) {
          tot <- sum(summary_list[["numeric"]][[i]], na.rm = TRUE)
          summary_list[["numeric"]][[i]] <- if (tot == 0) NA_real_ else
            100 * summary_list[["numeric"]][[i]] / tot
        }
      }
    }
    # categorical vars → count combinations
    if (length(cat_vars) > 0) {
      summary_list[["categorical"]] <- dt[, .N, by = c(valBy, cat_vars)]
      setnames(summary_list[["categorical"]], "N", "Freq")
    }

    summary_dt <- if (length(summary_list) == 1) summary_list[[1]] else summary_list

    if (output_type == "table") {
      if (assign_to_global) assign_summary_global(summary_dt, var, valBy)
      return(invisible(summary_dt))
    }

    if (output_type == "plot") {
      if ("numeric" %in% names(summary_list)) {
        for (i in num_vars) {
          plt_dt <- summary_list[["numeric"]][, c(valBy, i), with = FALSE]
          xvar <- valBy[1]
          yvar <- i
          p <- ggplot(plt_dt, aes_string(x = xvar, y = yvar)) +
            geom_bar(stat = "identity") +
            labs(
              title = paste0(i, " by ", paste(valBy, collapse = ", ")),
              x = xvar,
              y = ifelse(asPct, paste0(i, " (%)"), i)
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))

         ## Faceting (new)

      if (length(valBy) == 2) {
          p <- p + facet_wrap(as.formula(paste("~", valBy[2])), scales = "free_y")
      } else if (length(valBy) >= 3) {
        if (length(unique(plt_dt[[valBy[2]]])) < 8 &&
            length(unique(plt_dt[[valBy[3]]])) < 8) {
          p <- p + facet_grid(
            as.formula(paste(valBy[3], "~", valBy[2])),
            scales = "free_y"
          )
        }
        }

       print(p)
        }
      }

      if ("categorical" %in% names(summary_list)) {
        for (catv in cat_vars) {
          cnt_dt <- summary_list[["categorical"]]
          xvar <- if (!(catv %in% valBy)) valBy[1] else catv
          p <- ggplot(cnt_dt, aes_string(x = xvar, y = "Freq")) +
            geom_bar(stat = "identity") +
            labs(title = paste0("Count of ", catv, " by ", paste(setdiff(valBy, catv), collapse = ", ")),
                 x = xvar, y = "Freq") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          if (length(valBy) == 2 && !(catv %in% valBy)) {
            p <- p + facet_wrap(as.formula(paste("~", valBy[2])), scales = "free_y")
          }
          print(p)
        }
      }

      if (verbose) message("Plots generated for numeric and/or categorical variables.")
      return(invisible(summary_dt))
    }
    stop("Unsupported output_type for multi-variable case.")
  } # closes multi-variable case
}   # closes main function


