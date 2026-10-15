# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Checks the allowed variable names against the column names of
# the H1 test data in h1_v_20250211 (current RDBES format). The effort
# variable "CESciDaySea" had a typo.
capture.output({  ## suppresses printing of console output when running test()

  myH1RawObject <- RDBEScore::createRDBESDataObject(input = "h1_v_20250211")

  test_that("the allowed variables are columns of the RDBES tables", {
    expect_true(all(allowedLandingsVariable %in% names(myH1RawObject$CL)))
    expect_true(all(allowedEffortVariable %in% names(myH1RawObject$CE)))
    expect_true(all(allowedSamplingVariable %in% names(myH1RawObject$SA)))
    expect_true(all(allowedCatchCat %in% c("Lan", "Dis", "Catch")))
  })

  test_that("every allowed effort variable can be plotted", {
    for (effortVariable in allowedEffortVariable) {
      # several optional effort variables are empty in the test data
      expect_s3_class(
        suppressWarnings(coverageTemporal(myH1RawObject,
          catchCat = "Lan",
          effortVariable = effortVariable,
          includeLandings = FALSE,
          includeSamples = FALSE
        ))[[1]],
        "plotly"
      )
    }
  })

}) ## end capture.output
