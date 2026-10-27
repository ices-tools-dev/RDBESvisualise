# tests/testthat/test-summaryBV.R
# AI-assisted: Yes
# Review: Kasia-MIR
# Notes/scope: Tested with hierarchy H1 using
# 2025 RDBES data (downloaded 2026-10-27).

# plots are drawn by the function - send them to a null device
grDevices::pdf(NULL)

capture.output({  ## suppresses printing of console output when running test()

  # test data (dataset from RDBEScore)
  h1 <- RDBEScore::H1Example

  bvTypes <- unique(h1$BV$BVtypeMeas)

  # numeric types handled by summaryBiologicalVar
  numericCandidates <- c("WeightLive", "WeightMeasured", "ForkLength", "Age",
                         "LengthCarapace", "LengthLowerJawFork", "LengthMantle",
                         "LengthMaximumShell", "LengthPinchedTail",
                         "LengthPreAnal", "LengthPreCaudal", "LengthStandard",
                         "LengthTail", "LengthTotal", "LengthWingSpan",
                         "WidthCarapace", "WidthMaximumShell")
  numTypes <- intersect(bvTypes, numericCandidates)
  catTypes <- setdiff(bvTypes, numericCandidates)

  # the function sends a message when 'year' is not given
  quiet <- function(expr) suppressMessages(expr)

  # Test arguments ----------------------------------------------------------

  test_that("summaryBiologicalVar errors if 'by' is missing or not character", {

    expect_error(quiet(summaryBiologicalVar(h1)),
                 "'by' must be one or more column names")
    expect_error(quiet(summaryBiologicalVar(h1, by = 1)),
                 "'by' must be one or more column names")
  })

  test_that("summaryBiologicalVar errors if 'by' column is not in the data", {

    expect_error(quiet(summaryBiologicalVar(h1, by = "SDnotAColumn")),
                 "is not present in the data")
  })

  test_that("summaryBiologicalVar errors for invalid 'statistics'", {

    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry", statistics = "yes")),
                 "'statistics' must be TRUE or FALSE")
    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry", statistics = NA)),
                 "'statistics' must be TRUE or FALSE")
    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry",
                                            statistics = c(TRUE, FALSE))),
                 "'statistics' must be TRUE or FALSE")
  })

  test_that("summaryBiologicalVar errors for invalid 'bins'", {

    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry", bins = 0)),
                 "'bins' must be a single positive number")
    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry", bins = "a")),
                 "'bins' must be a single positive number")
    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry", bins = c(10, 20))),
                 "'bins' must be a single positive number")
  })

  test_that("summaryBiologicalVar errors if 'var' is not in the data", {

    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry",
                                            var = "NotABiologicalVariable")),
                 "is not present in the data")
  })

  test_that("summaryBiologicalVar errors if there are no BV records", {

    noBV <- h1
    noBV$BV <- noBV$BV[0, ]

    expect_error(quiet(summaryBiologicalVar(noBV, by = "SDctry")),
                 "does not contain any BV")
  })

  test_that("summaryBiologicalVar warns if some of the years are not in the data", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    validYear <- unique(h1$DE$DEyear)[1]

    expect_warning(quiet(summaryBiologicalVar(h1, by = "SDctry",
                                              year = c(validYear, 1900))),
                   "are not present in the data")
  })

  # Test general behaviour ----------------------------------------------------

  test_that("summaryBiologicalVar runs without errors or warnings for H1", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    expect_error(quiet(summaryBiologicalVar(h1, by = "SDctry")), NA)
    expect_warning(quiet(summaryBiologicalVar(h1, by = "SDctry")), NA)
  })

  test_that("summaryBiologicalVar returns the result invisibly", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    expect_invisible(quiet(summaryBiologicalVar(h1, by = "SDctry")))
  })

  test_that("summaryBiologicalVar sends a message if 'year' is not given", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    expect_message(summaryBiologicalVar(h1, by = "SDctry"),
                   "will be present in the data")
  })

  test_that("summaryBiologicalVar does not modify the input object", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    bvBefore <- data.table::copy(h1$BV)
    namesBefore <- names(h1$BV)

    quiet(summaryBiologicalVar(h1, by = "SDctry", statistics = TRUE))

    expect_equal(h1$BV, bvBefore)
    expect_equal(names(h1$BV), namesBefore)
  })

  # Test outputType -----------------------------------------------------------

  test_that("summaryBiologicalVar returns countTable and plots by default", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry"))

    expect_true(all(c("countTable", "plots") %in% names(res)))
    expect_null(res$statsTable)
    expect_null(res$statsFigures)
    expect_true(all(vapply(res$plots, inherits, logical(1), what = "ggplot")))
    expect_setequal(names(res$plots), bvTypes)
  })

  test_that("summaryBiologicalVar returns only the table for outputType = 'table'", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", outputType = "table"))

    expect_equal(names(res), "countTable")
  })

  test_that("summaryBiologicalVar returns no table for outputType = 'plot'", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", outputType = "plot"))

    expect_null(res$countTable)
    expect_null(res$tableFigures)
    expect_true("plots" %in% names(res))
  })

  test_that("summaryBiologicalVar creates tableFigures for non-numeric variables", {

    skip_if(length(catTypes) == 0, "no non-numeric variables in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", var = catTypes,
                                      outputType = c("plot", "table")))

    expect_setequal(names(res$tableFigures), catTypes)
    expect_true(all(vapply(res$tableFigures, inherits, logical(1),
                           what = "gtable")))
  })

  # Test count table -----------------------------------------------------------

  test_that("countTable has the expected columns", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry"))

    expect_true(all(c("SDctry", "BVtypeMeas", "nIndividuals", "hierarchy") %in%
                      names(res$countTable)))
    expect_equal(unique(res$countTable$hierarchy),
                 unique(h1$DE$DEhierarchy))
  })

  test_that("countTable counts are consistent with the BV table", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry"))

    expect_gt(sum(res$countTable$nIndividuals), 0)
    expect_lte(sum(res$countTable$nIndividuals), nrow(h1$BV))
  })

  test_that("summaryBiologicalVar uses only the variable given in 'var'", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    oneType <- bvTypes[1]
    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", var = oneType))

    expect_equal(unique(res$countTable$BVtypeMeas), oneType)
    expect_equal(names(res$plots), oneType)
  })

  test_that("summaryBiologicalVar works with more than one 'by' column", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = c("SDctry", "DEyear")))

    expect_true(all(c("SDctry", "DEyear") %in% names(res$countTable)))
  })

  test_that("summaryBiologicalVar filters the data by year", {

    skip_if(nrow(h1$BV) == 0, "no BV records in the test data")

    validYear <- unique(h1$DE$DEyear)[1]
    res <- quiet(summaryBiologicalVar(h1, by = "DEyear", year = validYear))

    expect_equal(unique(res$countTable$DEyear), validYear)
  })

  # Test statistics ---------------------------------------------------------------

  test_that("statsTable has the expected columns and values", {

    skip_if(length(numTypes) == 0, "no numeric variables in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", var = numTypes,
                                      statistics = TRUE))

    expect_equal(names(res$statsTable),
                 c("BVtypeMeas", "SDctry", "n", "mean", "sd", "median",
                   "min", "max"))
    expect_true(all(res$statsTable$min <= res$statsTable$median))
    expect_true(all(res$statsTable$median <= res$statsTable$max))
    expect_true(all(res$statsTable$min <= res$statsTable$mean))
    expect_true(all(res$statsTable$mean <= res$statsTable$max))
  })

  test_that("statsTable counts agree with countTable", {

    skip_if(length(numTypes) == 0, "no numeric variables in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", var = numTypes,
                                      statistics = TRUE))

    expect_equal(sum(res$statsTable$n), sum(res$countTable$nIndividuals))
  })

  test_that("statsFigures are created for numeric variables", {

    skip_if(length(numTypes) == 0, "no numeric variables in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", var = numTypes,
                                      statistics = TRUE))

    expect_setequal(names(res$statsFigures), numTypes)
    expect_true(all(vapply(res$statsFigures, inherits, logical(1),
                           what = "gtable")))
  })

  test_that("statsFigures are created also for outputType = 'table'", {

    skip_if(length(numTypes) == 0, "no numeric variables in the test data")

    res <- quiet(summaryBiologicalVar(h1, by = "SDctry", var = numTypes,
                                      outputType = "table", statistics = TRUE))

    expect_true(all(c("countTable", "statsTable", "statsFigures") %in%
                      names(res)))
    expect_null(res$plots)
  })

  test_that("summaryBiologicalVar warns if statistics are not available", {

    skip_if(length(catTypes) == 0, "no non-numeric variables in the test data")

    expect_warning(
      res <- quiet(summaryBiologicalVar(h1, by = "SDctry", var = catTypes,
                                        statistics = TRUE)),
      "Statistics are available only for numeric variables"
    )
    expect_null(res$statsTable)
    expect_null(res$statsFigures)
  })

}) ## end capture.output

grDevices::dev.off()
