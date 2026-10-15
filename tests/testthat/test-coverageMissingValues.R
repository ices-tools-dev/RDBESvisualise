# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the H1 test data in h1_v_20250211 (#37). The expected
# numbers of missing values are counted directly from the SA and CL tables.
capture.output({  ## suppresses printing of console output when running test()

  myH1RawObject <- RDBEScore::createRDBESDataObject(input = "h1_v_20250211")

  test_that("coverage plots warn about missing sample values", {
    landedSamples <- myH1RawObject$SA[myH1RawObject$SA$SAcatchCat == "Lan", ]
    nMissing <- sum(is.na(landedSamples$SAsampWtMes))
    expect_gt(nMissing, 0)

    expect_warning(
      plots <- coverageByGear(
        dataToPlot = myH1RawObject,
        catchCat = "Lan",
        samplingVariable = "SAsampWtMes",
        includeLandings = FALSE,
        includeEffort = FALSE
      ),
      paste("SAsampWtMes is missing for", nMissing, "of",
            nrow(landedSamples), "sample records")
    )
    expect_s3_class(plots[[1]], "plotly")
  })

  test_that("coverage plots warn about missing landings values", {
    x <- myH1RawObject
    x$CL <- data.table::copy(x$CL)
    x$CL$CLoffWeight[1:2] <- NA

    expect_warning(
      coverageTemporal(
        dataToPlot = x,
        catchCat = "Lan",
        landingsVariable = "CLoffWeight",
        includeEffort = FALSE,
        includeSamples = FALSE
      ),
      paste("CLoffWeight is missing for 2 of", nrow(x$CL), "landings records")
    )
  })

  test_that("there is no warning without missing values", {
    expect_no_warning(
      coverageByGear(
        dataToPlot = myH1RawObject,
        catchCat = "Lan",
        landingsVariable = "CLoffWeight",
        includeEffort = FALSE,
        includeSamples = FALSE
      )
    )
  })

  test_that("missing values are excluded from the temporal sums", {
    x <- myH1RawObject
    x$CL <- data.table::copy(x$CL)
    x$CL$CLquar <- 1
    x$CL$CLoffWeight[1] <- NA

    plots <- suppressWarnings(coverageTemporal(
      dataToPlot = x,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      includeEffort = FALSE,
      includeSamples = FALSE
    ))

    # Before, the quarter with a missing value was dropped from the plot
    # plotly warns about the empty base trace of the plot
    plotData <- suppressWarnings(plotly::plotly_build(plots[[1]]))$x$data
    landings <- plotData[[which(sapply(plotData, function(d) {
      identical(d$name, "Landings")
    }))]]
    expect_equal(as.numeric(landings$y), 1)
  })

}) ## end capture.output
