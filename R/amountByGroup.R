#' Summarise numeric and categorical variables by grouping variables
#'
#' `amountByGroup()` summarises one or more variables (numeric and/or
#' categorical) by one or more grouping variables. Numeric variables are
#' aggregated using sums, while categorical variables are summarised as
#' frequency counts. The function supports flexible filtering, percentage
#' conversion, grouped summaries, and optional plotting. Designed for RDBES
#' datasets or similar.
#'
#' @details
#' Main features:
#' \itemize{
#'   \item Numeric variables are summed (optionally converted to percentages).
#'   \item Categorical variables are counted as frequencies.
#'   \item Multiple variables can be summarised simultaneously.
#'   \item Optional filtering prior to aggregation.
#'   \item Optional plotting using ggplot2.
#' }
#'
#' When both numeric and categorical variables are supplied, the result
#' is returned as a list with components \code{$numeric} and
#' \code{$categorical}.
#'
#' @param data A \code{data.frame} or \code{data.table}.
#' @param var Character vector of column names to summarise.
#' @param valBy Character vector of grouping variables.
#' @param filters Optional named list of filters applied before aggregation.
#' @param output_type One of \code{"table"} or \code{"plot"}.
#' @param asPct Logical; if \code{TRUE}, numeric summaries are converted to percentages.
#' @param assign_to_global Logical; if \code{TRUE}, assigns result to \code{.GlobalEnv}.
#' @param verbose Logical; print progress messages.
#'
#' @return Invisibly returns a \code{data.table} or a list of tables.
#'
#' @importFrom data.table as.data.table setnames
#' @importFrom ggplot2 ggplot geom_bar labs theme_minimal theme facet_wrap facet_grid element_text
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Summarise one numeric variable by year and flag
#' tbl1 <- amountByGroup(
#'   data = landings,
#'   var = "LiveWeight",
#'   valBy = c("Year", "FlagCountry"),
#'   filters = list(SpeciesGroup = "COD"),
#'   asPct = FALSE,
#'   output_type = "table"
#' )
#'
#' Summarise a categorical variable (species name)
#' tbl2 <- amountByGroup(
#'   data = landings,
#'   var = "SpeciesName",
#'   valBy = "Harbour",
#'   output_type = "plot"
#' )
#'
#' Summarise multiple numeric + categorical variables together
#' tbl3 <- amountByGroup(
#'   data = landings,
#'   var = c("Weight", "NumSamp", "SpeciesName"),
#'   valBy = c("Year", "FlagCountry"),
#'   output_type = "table"
#' )
#' }
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

  # ---- argument checks ----

  output_type <- match.arg(output_type)

  if (missing(data)) stop("'data' must be provided.")
  if (!is.character(var)) stop("'var' must be a character vector.")
  if (!is.character(valBy) || length(valBy) < 1)
    stop("'valBy' must be a character vector with at least one element.")

  `%||%` <- function(x, y) if (!is.null(x)) x else y
  dt <- data.table::as.data.table(data)

  # check all columns exist
  missing_cols <- setdiff(c(var, valBy, names(filters %||% list())), names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("The following columns are missing from 'data': %s",
                 paste(missing_cols, collapse = ", ")))
  }


  # ---- optional filters ----
  if (!is.null(filters)) {
    if (!is.list(filters))
      stop("'filters' must be a named list, e.g. list(Year = 2022).")

    for (col in names(filters)) {
      if (col %in% names(dt)) {
        vals <- filters[[col]]
        dt <- dt[get(col) %in% vals]
        if (verbose) {
          message(sprintf(
            "Filter applied: %s in %s",
            paste(vals, collapse = ","),
            col
          ))
        }
      } else {
        warning(sprintf(
          "Filter column '%s' not found in data — ignored.",
          col
        ))
      }
    }
  }

  # empty after filters
  if (nrow(dt) == 0) {
    warning("No rows left after filtering.")
    return(invisible(data.table::as.data.table(NULL)))
  }
  # function for assigning to global environment (name uses exact var/valBy strings)
  assign_summary_global <- function(tbl, var_vec, valBy_vec) {
    obj_name <- paste0(
      "tbl_", paste(var_vec, collapse = "_"),
      "_by_", paste(valBy_vec, collapse = "_")
    )
    assign(obj_name, tbl, envir = .GlobalEnv)
    if (verbose)
      message("Assigned table to global environment as: ", obj_name)
    invisible(obj_name)
  }

  # ---- SINGLE-VARIABLE CASE -------------------------------------------------
  if (length(var) == 1) {
    i <- var[1]

    if (is.numeric(dt[[i]])) {

      summary_dt <- dt[, list(value = sum(get(i), na.rm = TRUE)), by = valBy]
      data.table::setnames(summary_dt, "value", i)

      if (asPct) {
        tot <- sum(summary_dt[[i]], na.rm = TRUE)
        summary_dt[[i]] <- if (tot == 0) NA_real_ else
          100 * summary_dt[[i]] / tot
      }

      if (output_type == "table") {
        if (assign_to_global)
          assign_summary_global(summary_dt, var, valBy)
        if (verbose) message("Returning numeric summary table.")
        return(invisible(summary_dt[]))
      }

      if (output_type == "plot") {
        p <- ggplot2::ggplot(
          summary_dt,
          ggplot2::aes_string(x = valBy[1], y = i)
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::labs(
            title = paste0(i, " by ", paste(valBy, collapse = ", ")),
            x = valBy[1],
            y = ifelse(asPct, paste0(i, " (%)"), i)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )

        # ADD FACETING
        if (length(valBy) >= 2) {
          p <- p + ggplot2::facet_wrap(
            stats::as.formula(paste("~", valBy[2])),
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
      data.table::setnames(cnt_dt, "N", "Freq")

      if (output_type == "table") {
        if (assign_to_global)
          assign_summary_global(cnt_dt, var, valBy)
        if (verbose)
          message("Returning frequency table for categorical variable.")
        return(invisible(cnt_dt[]))
      }

      if (output_type == "plot") {
        xvar <- if (!(i %in% valBy)) valBy[1] else i
        p <- ggplot2::ggplot(
          cnt_dt,
          ggplot2::aes_string(x = xvar, y = "Freq")
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::labs(
            title = paste0(
              "Count of ", i, " by ",
              paste(setdiff(valBy, i), collapse = ", ")
            ),
            x = xvar,
            y = "Freq"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )

        if (length(valBy) == 2 && !(i %in% valBy)) {
          p <- p + ggplot2::facet_wrap(
            stats::as.formula(paste("~", valBy[2])),
            scales = "free_y"
          )
        }

        print(p)
        if (verbose) message("Plot generated for categorical variable.")
        return(invisible(cnt_dt[]))
      }
      stop("Unsupported output_type for categorical variable.")
    }
  }

  # ---- MULTI-VARIABLE CASE --------------------------------------------------
  if (length(var) > 1) {

    num_vars <- names(Filter(is.numeric, dt[, ..var]))
    cat_vars <- setdiff(var, num_vars)
    summary_list <- list()

    # numeric vars → sum
    if (length(num_vars) > 0) {
      summary_list[["numeric"]] <-
        dt[, lapply(.SD, sum, na.rm = TRUE),
           by = valBy, .SDcols = num_vars]

      if (asPct) {
        for (i in num_vars) {
          tot <- sum(summary_list[["numeric"]][[i]], na.rm = TRUE)
          summary_list[["numeric"]][[i]] <-
            if (tot == 0) NA_real_ else
              100 * summary_list[["numeric"]][[i]] / tot
        }
      }
    }
    # categorical vars → count combinations
    if (length(cat_vars) > 0) {
      summary_list[["categorical"]] <-
        dt[, .N, by = c(valBy, cat_vars)]
      data.table::setnames(
        summary_list[["categorical"]],
        "N", "Freq"
      )
    }

    summary_dt <-
      if (length(summary_list) == 1)
        summary_list[[1]] else summary_list

    if (output_type == "table") {
      if (assign_to_global)
        assign_summary_global(summary_dt, var, valBy)
      return(invisible(summary_dt))
    }

    if (output_type == "plot") {

      if ("numeric" %in% names(summary_list)) {
        for (i in num_vars) {

          plt_dt <- summary_list[["numeric"]][, c(valBy, i), with = FALSE]

          p <- ggplot2::ggplot(
            plt_dt,
            ggplot2::aes_string(x = valBy[1], y = i)
          ) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::labs(
              title = paste0(i, " by ", paste(valBy, collapse = ", ")),
              x = valBy[1],
              y = ifelse(asPct, paste0(i, " (%)"), i)
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )
          ## Faceting
          if (length(valBy) == 2) {
            p <- p + ggplot2::facet_wrap(
              stats::as.formula(paste("~", valBy[2])),
              scales = "free_y"
            )
          } else if (length(valBy) >= 3) {
            if (length(unique(plt_dt[[valBy[2]]])) < 8 &&
                length(unique(plt_dt[[valBy[3]]])) < 8) {
              p <- p + ggplot2::facet_grid(
                stats::as.formula(
                  paste(valBy[3], "~", valBy[2])
                ),
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

          p <- ggplot2::ggplot(
            cnt_dt,
            ggplot2::aes_string(x = xvar, y = "Freq")
          ) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::labs(
              title = paste0(
                "Count of ", catv, " by ",
                paste(setdiff(valBy, catv), collapse = ", ")
              ),
              x = xvar,
              y = "Freq"
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )

          if (length(valBy) == 2 && !(catv %in% valBy)) {
            p <- p + ggplot2::facet_wrap(
              stats::as.formula(paste("~", valBy[2])),
              scales = "free_y"
            )
          }

          print(p)
        }
      }

      if (verbose)
        message("Plots generated for numeric and/or categorical variables.")
      return(invisible(summary_dt))
    }
    stop("Unsupported output_type for multi-variable case.")
  } # closes multi-variable case
}   # closes main function
