# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the H1 test data in h1_v_20250211 with random quarters
# for CL and random rectangles for SA (#11). The expected sums are calculated
# directly from the SA, SS, FO and CL tables.
capture.output({  ## suppresses printing of console output when running test()

  prepareTestData <- function() {
    x <- RDBEScore::createRDBESDataObject(input = "h1_v_20250211")
    set.seed(1)
    x[["CL"]]$CLquar <- as.integer(runif(nrow(x[["CL"]]), min = 1, max = 4.99))
    x[["SA"]]$SAstatRect <- sample(unique(x[["CL"]]$CLstatRect),
                                   size = nrow(x[["SA"]]), replace = TRUE)
    x
  }
  myH1RawObject <- prepareTestData()

  # Landed samples of 1965 with the quarter of their FO end date
  sa <- merge(myH1RawObject$SA, myH1RawObject$SS[, c("SSid", "FOid")],
              by = "SSid")
  sa <- merge(sa, myH1RawObject$FO[, c("FOid", "FOendDate")], by = "FOid")
  sa$quarter <- lubridate::quarter(as.Date(sa$FOendDate))
  sa <- sa[format(as.Date(sa$FOendDate), "%Y") == "1965" & sa$SAcatchCat == "Lan", ]

  test_that("sampling and landings are summed by quarter", {
    coverage <- produceSamplingCoverage(myH1RawObject,
      var = "SAnumSamp",
      contrastVar = "CLoffWeight",
      resolution = "Quarter",
      year = 1965,
      catchCat = "Lan",
      output_type = "table"
    )

    expectedSampling <- tapply(sa$SAnumSamp, sa$quarter, sum)
    cl <- myH1RawObject$CL
    expectedLandings <- sapply(1:4, function(q) sum(cl$CLoffWeight[cl$CLquar == q]))
    expect_equal(coverage$Quarter, 1:4)
    expect_equal(coverage$sampling, as.vector(expectedSampling))
    expect_equal(coverage$contrast, expectedLandings)
    expect_equal(coverage$samplingShare,
                 as.vector(expectedSampling / sum(expectedSampling)))
    expect_equal(sum(coverage$contrastShare), 1)
  })

  test_that("SAamountSamp counts the samples and by splits the data", {
    coverage <- produceSamplingCoverage(myH1RawObject,
      var = "SAamountSamp",
      contrastVar = "CLoffWeight",
      resolution = "ICES Rectangle",
      by = "Quarter",
      year = 1965,
      catchCat = "Lan",
      output_type = "table"
    )

    expected <- table(sa$SAstatRect, sa$quarter)
    expect_equal(nrow(coverage), length(expected))
    expect_equal(
      coverage$sampling,
      as.vector(expected[cbind(coverage$`ICES Rectangle`, coverage$Quarter)])
    )
    # shares add up to 1 within each quarter
    expect_equal(as.vector(tapply(coverage$samplingShare, coverage$Quarter, sum)),
                 rep(1, 4))
  })

  test_that("effort can be used as the contrast variable", {
    coverage <- suppressWarnings(produceSamplingCoverage(myH1RawObject,
      var = "SAnumSamp",
      contrastVar = "CEnumFracTrips",
      resolution = "Gear",
      catchCat = "Lan",
      output_type = "table"
    ))
    expect_equal(sum(coverage$contrast),
                 sum(myH1RawObject$CE$CEnumFracTrips))
  })

  test_that("plots are ggplot2 plots", {
    for (myResolution in c("ICES Rectangle", "Quarter")) {
      p <- produceSamplingCoverage(myH1RawObject,
        var = "SAnumSamp",
        contrastVar = "CLoffWeight",
        resolution = myResolution,
        year = 1965,
        catchCat = "Lan"
      )
      expect_s3_class(p, "ggplot")
      expect_match(p$labels$title, "Sampling catchCat=Lan", fixed = TRUE)
    }
  })

  test_that("invalid parameters give errors", {
    expect_error(
      produceSamplingCoverage(myH1RawObject, contrastVar = "CLoffWeight",
                              resolution = "Quarter"),
      "You must provide one catchCat"
    )
    expect_error(
      produceSamplingCoverage(myH1RawObject, contrastVar = "CEnumFracTrips",
                              resolution = "Species", catchCat = "Lan"),
      "Species is not available for effort data"
    )
    expect_error(
      produceSamplingCoverage(myH1RawObject, resolution = "Quarter",
                              by = "Quarter", catchCat = "Lan"),
      "by must be one or more of"
    )
    expect_error(
      produceSamplingCoverage(myH1RawObject, contrastVar = "CLnotAVariable",
                              resolution = "Quarter", catchCat = "Lan"),
      "Invalid contrastVar value"
    )
  })

}) ## end capture.output
