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
#   • Regression test: multi-variable numeric plots → faceting
# =====================================================================

# First basic check: the function should exist in package namespace
test_that("amountByGroup exists", {
  expect_true(exists("amountByGroup", mode = "function"))
})

# ------------------------------------------------------------------
# PURPOSE:
#   Verify that numeric variables are summed correctly by the grouping columns.
# ------------------------------------------------------------------
test_that("amountByGroup correctly summarises numeric variables", {

  ## Mock data
  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023, 2023),
    Flag = c("XT", "SY", "XT", "SY"),
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
  expect_s3_class(res, "data.table") # checks that the object returned by your function (res) is of S3 class "data.table"
  expect_true(all(c("Year", "Catch") %in% names(res)))
  expect_equal(sum(res$Catch), sum(dt$Catch))
})

# ------------------------------------------------------------------
# PURPOSE:
#   Verify that non-numeric variables produce frequency counts per group.
# ------------------------------------------------------------------
test_that("amountByGroup correctly counts categorical variables", {

  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023, 2023, 2023),
    Species = c("HKE", "HOM", "HKE", "HOM", "HKE")
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

# ------------------------------------------------------------------
# PURPOSE:
#   Confirm that the 'filters' argument correctly subsets the input table.
# ------------------------------------------------------------------
test_that("amountByGroup applies filters correctly", {

  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023),
    Flag = c("XT", "SY", "XT"),
    Catch = c(10, 20, 30)
  )

  res <- amountByGroup(
    data = dt,
    var = "Catch",
    valBy = "Year",
    filters = list(Flag = "XT"),
    output_type = "table",
    verbose = FALSE
  )

  # CHECKS
  expect_equal(unique(res$Year), c(2022, 2023))
  expect_equal(sum(res$Catch), 40) # only XT rows (10 + 30)
})

# ------------------------------------------------------------------
# PURPOSE:
#   Ensure function can summarise both numeric (sum) and categorical (counts)
#   in the same call, returning a list with separate data.tables.
# ------------------------------------------------------------------
test_that("amountByGroup handles mixed numeric and categorical variables", {

  library(data.table)
  dt <- data.table(
    Year = c(2022, 2022, 2023, 2023),
    Flag = c("XT", "SY", "XT", "SY"),
    Catch = c(10, 20, 30, 40),
    Species = c("HKE", "HOM", "HKE", "HOM")
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

# ------------------------------------------------------------------
# PURPOSE:
#   Ensure function is working and consistent for numeric plot output.
# ------------------------------------------------------------------
test_that("amountByGroup produces a ggplot for numeric variable", {

  data <- data.table(
    Year = rep(2020:2021, each = 3),
    Catch = c(100, 200, 150, 50, 75, 125)
  )

  # Should not error
  expect_no_error({
    res <- amountByGroup(
      data = data,
      var = "Catch",
      valBy = "Year",
      output_type = "plot",
      verbose = FALSE
    )
  })

  # Check that res is still returned invisibly as a data.table
  expect_s3_class(res, "data.table")
})

# ------------------------------------------------------------------
# PURPOSE:
#   Ensure function is working and consistent for categorical plot output.
# ------------------------------------------------------------------
test_that("amountByGroup produces ggplot for categorical variable", {

  data <- data.table(
    CatchCat = c('Lan','Lan','Dis','Lan'),
    Species = c("Hake", "HorseMackerel", "Hake", "Megrim")
  )

  expect_no_error({
    res <- amountByGroup(
      data = data,
      var = "Species",
      valBy = "CatchCat",
      output_type = "plot",
      verbose = FALSE
    )
  })

  # check result table
  expect_s3_class(res, "data.table")
  expect_true("Freq" %in% names(res))
})


# ------------------------------------------------------------------
# PURPOSE:
#   Verify that 'assign_to_global = TRUE' creates an object in .GlobalEnv
#   with the correct auto-generated name pattern.
# ------------------------------------------------------------------
test_that("amountByGroup assigns output table to global environment when requested", {

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

# ------------------------------------------------------------------
# PURPOSE:
#   Verify the function does not crash when the dataset contains NA values.
# ------------------------------------------------------------------
test_that("amountByGroup gracefully handles missing or NA data", {

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

# ------------------------------------------------------------------
# PURPOSE:
#   Confirm proper error messages for missing or invalid parameters.
# ------------------------------------------------------------------
test_that("amountByGroup throws informative errors for invalid inputs", {

  library(data.table)
  dt <- data.table(Year = 2022, Catch = 10)

  expect_error(amountByGroup(valBy = "Year"), "must be provided")
  expect_error(amountByGroup(data = dt, var = 123, valBy = "Year"), "must be a character vector")
  expect_error(amountByGroup(data = dt, var = "Catch", valBy = "Missing"), "missing from 'data'")
})

# ------------------------------------------------------------------
# PURPOSE:
#   Confirm proper faceting for multi-variable numeric plots.
# ------------------------------------------------------------------
test_that("multi-variable numeric plotting applies faceting when valBy has two variables", {

  skip_if_not_installed("ggplot2")
  skip_if_not_installed("data.table")

  library(data.table)
  library(ggplot2)

  # ---- minimal reproducible data ----
  dt <- data.table(
    Year = c(2021, 2021, 2022, 2022),
    Quarter = c("Q1", "Q2", "Q1", "Q2"),
    WeightKg = c(100, 200, 150, 250),
    ValueEUR = c(1000, 2000, 1500, 2500)
  )

  # Run the function (side-effect: creates plots)
  expect_silent(
    amountByGroup(
      data = dt,
      var = c("WeightKg", "ValueEUR"),
      valBy = c("Year", "Quarter"),
      output_type = "plot",
      verbose = FALSE
    )
  )

  # Retrieve the last plot produced
  p <- last_plot()

  # Basic sanity
  expect_s3_class(p, "ggplot")

  # REGRESSION CHECK: must be faceted
  expect_false(
    inherits(p$facet, "FacetNull"),
    info = "Expected faceting when valBy has two variables, but no facet was found."
  )
})

# ------------------------------------------------------------------
# PURPOSE:
#   Confirm proper faceting for mixed-variable plots.
# ------------------------------------------------------------------

test_that("single numeric variable with two valBy facets correctly", {

  library(data.table)
  library(ggplot2)

  dt <- data.table(
    Year = c(2021, 2021, 2022, 2022),
    Quarter = c("Q1", "Q2", "Q1", "Q2"),
    WeightKg = c(100, 200, 150, 250)
  )

  expect_silent(
    amountByGroup(
      data = dt,
      var = "WeightKg",
      valBy = c("Year", "Quarter"),
      output_type = "plot",
      verbose = FALSE
    )
  )

  p <- last_plot()

  expect_s3_class(p, "ggplot")
  expect_false(
    inherits(p$facet, "FacetNull"),
    info = "Expected faceting for single numeric variable with multiple valBy"
  )
})





