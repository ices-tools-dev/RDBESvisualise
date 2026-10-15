# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the H1 test data in h1_v_20250211, which has samples from
# several years. The expected years are taken directly from the CL, CE and
# FO tables. Before the fix only the first year was plotted.
capture.output({  ## suppresses printing of console output when running test()

  myH1RawObject <- RDBEScore::createRDBESDataObject(input = "h1_v_20250211")

  # Years of the landed samples, from the end date of their fishing operation
  sa <- merge(myH1RawObject$SA, myH1RawObject$SS[, c("SSid", "FOid")],
              by = "SSid")
  sa <- merge(sa, myH1RawObject$FO[, c("FOid", "FOendDate")], by = "FOid")
  sampleYears <- format(as.Date(sa$FOendDate[sa$SAcatchCat == "Lan"]), "%Y")
  expectedYears <- sort(unique(c(sampleYears,
                                 myH1RawObject$CL$CLyear,
                                 myH1RawObject$CE$CEyear)))

  titleOf <- function(plot) {
    unlist(suppressWarnings(plotly::plotly_build(plot))$x$layout$title)[1]
  }

  test_that("there is one plot per year", {
    expect_gt(length(expectedYears), 1)

    gearPlots <- coverageByGear(myH1RawObject,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAnumSamp"
    )
    temporalPlots <- coverageTemporal(myH1RawObject,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAnumSamp"
    )

    for (plots in list(gearPlots, temporalPlots)) {
      expect_length(plots, length(expectedYears))
      titles <- vapply(plots, titleOf, character(1), USE.NAMES = FALSE)
      expect_equal(substring(titles, nchar(titles) - 3), expectedYears)
    }
  })

}) ## end capture.output
