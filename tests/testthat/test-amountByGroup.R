# =====================================================================
#  Unit tests for amountByGroup()
#  -------------------------------------------------------------------
#  These tests validate all major behaviors of the amountByGroup() function:
#   • Numeric summarisation (sum by group)
#   • Categorical summarisation (frequency counts)
#   • Mixed variable handling (numeric + categorical)
#   • Filter application
#   • Global assignment behavior
#   • NA handling
#   • Input validation and error handling
# =====================================================================

test_that("amountByGroup correctly summarises numeric variables", {
  # ------------------------------------------------------------------
  # PURPOSE:
  #   Verify that numeric variables are summed correctly by the grouping columns.
  # ------------------------------------------------------------------
  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023, 2023),
    Flag = c("PT", "SE", "PT", "SE"),
    Catch = c(10, 20, 30, 40)
  )

  res <- amountByGroup(
    data = dt,
    var = "Catch",
    valBy = "Year",
    output_type = "table",
    verbose = FALSE
  )

  # CHECKS
  expect_s3_class(res, "data.table")
  expect_true(all(c("Year", "Catch") %in% names(res)))
  expect_equal(sum(res$Catch), sum(dt$Catch))
})


test_that("amountByGroup correctly counts categorical variables", {
  # ------------------------------------------------------------------
  # PURPOSE:
  #   Verify that non-numeric variables produce frequency counts per group.
  # ------------------------------------------------------------------
  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023, 2023, 2023),
    Species = c("COD", "HAD", "COD", "HAD", "COD")
  )

  res <- amountByGroup(
    data = dt,
    var = "Species",
    valBy = "Year",
    output_type = "table",
    verbose = FALSE
  )

  # CHECKS
  expect_s3_class(res, "data.table")
  expect_true(all(c("Year", "Species", "Freq") %in% names(res)))
  expect_equal(sum(res$Freq), nrow(dt)) # total count should match input rows
})


test_that("amountByGroup applies filters correctly", {
  # ------------------------------------------------------------------
  # PURPOSE:
  #   Confirm that the 'filters' argument correctly subsets the input table.
  # ------------------------------------------------------------------
  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023),
    Flag = c("PT", "SE", "PT"),
    Catch = c(10, 20, 30)
  )

  res <- amountByGroup(
    data = dt,
    var = "Catch",
    valBy = "Year",
    filters = list(Flag = "PT"),
    output_type = "table",
    verbose = FALSE
  )

  # CHECKS
  expect_equal(unique(res$Year), c(2022, 2023))
  expect_equal(sum(res$Catch), 40) # only PT rows (10 + 30)
})


test_that("amountByGroup handles mixed numeric and categorical variables", {
  # ------------------------------------------------------------------
  # PURPOSE:
  #   Ensure function can summarise both numeric (sum) and categorical (counts)
  #   in the same call, returning a list with separate data.tables.
  # ------------------------------------------------------------------
  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023, 2023),
    Flag = c("PT", "SE", "PT", "SE"),
    Catch = c(10, 20, 30, 40),
    Species = c("COD", "HAD", "COD", "HAD")
  )

  res <- amountByGroup(
    data = dt,
    var = c("Catch", "Species"),
    valBy = "Flag",
    output_type = "table",
    verbose = FALSE
  )

  # CHECKS
  expect_type(res, "list")
  expect_true(all(c("numeric", "categorical") %in% names(res)))
  expect_s3_class(res$numeric, "data.table")
  expect_s3_class(res$categorical, "data.table")
})


test_that("amountByGroup assigns output table to global environment when requested", {
  # ------------------------------------------------------------------
  # PURPOSE:
  #   Verify that 'assign_to_global = TRUE' creates an object in .GlobalEnv
  #   with the correct auto-generated name pattern.
  # ------------------------------------------------------------------
  library(data.table)
  dt <- data.table(Year = c(2022, 2023), Catch = c(10, 30))

  invisible(
    amountByGroup(
      data = dt,
      var = "Catch",
      valBy = "Year",
      assign_to_global = TRUE,
      output_type = "table",
      verbose = FALSE
    )
  )

  expect_true(any(grepl("^tbl_Catch_by_Year$", ls(.GlobalEnv))))
  rm(list = grep("^tbl_Catch_by_Year$", ls(.GlobalEnv), value = TRUE), envir = .GlobalEnv)
})


test_that("amountByGroup gracefully handles missing or NA data", {
  # ------------------------------------------------------------------
  # PURPOSE:
  #   Verify the function does not crash when the dataset contains NA values.
  # ------------------------------------------------------------------
  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, NA),
    Catch = c(10, NA, 30)
  )

  res <- amountByGroup(
    data = dt,
    var = "Catch",
    valBy = "Year",
    output_type = "table",
    verbose = FALSE
  )

  expect_s3_class(res, "data.table")
  expect_true(all(is.finite(res$Catch) | is.na(res$Catch)))
})


test_that("amountByGroup throws informative errors for invalid inputs", {
  # ------------------------------------------------------------------
  # PURPOSE:
  #   Confirm proper error messages for missing or invalid parameters.
  # ------------------------------------------------------------------
  library(data.table)
  dt <- data.table(Year = 2022, Catch = 10)

  expect_error(amountByGroup(valBy = "Year"), "must be provided")
  expect_error(amountByGroup(data = dt, var = 123, valBy = "Year"), "must be a character vector")
  expect_error(amountByGroup(data = dt, var = "Catch", valBy = "Missing"), "missing from 'data'")
})
