# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the RDBEScore 0.4.0 H7 and H8 example data (#14). The
# expected statistics are calculated directly from the BV table.
capture.output({  ## suppresses printing of console output when running test()

  test_that("numbers of individuals and statistics are correct", {
    bv <- RDBEScore::H8ExampleEE1$BV
    summaryTable <- summariseBiologicalVariables(RDBEScore::H8ExampleEE1)

    expect_equal(summaryTable$BVtypeMeas, sort(unique(bv$BVtypeMeas)))
    expect_equal(summaryTable$n, as.vector(table(bv$BVtypeMeas)))

    lengths <- as.numeric(bv$BVvalueMeas[bv$BVtypeMeas == "LengthTotal"])
    lengthRow <- summaryTable[summaryTable$BVtypeMeas == "LengthTotal", ]
    expect_equal(lengthRow$mean, mean(lengths))
    expect_equal(lengthRow$sd, sd(lengths))
    expect_equal(lengthRow$median, median(lengths))
    expect_equal(lengthRow$IQR, IQR(lengths))
  })

  test_that("sex and maturity are summarised as categories", {
    summaryTable <- summariseBiologicalVariables(RDBEScore::H8ExampleEE1,
                                                 var = c("Sex", "Maturity"))
    expect_equal(summaryTable$BVtypeMeas, c("Maturity", "Sex"))
    expect_true(all(is.na(summaryTable$mean)))
  })

  test_that("the data can be split by a column of an upper table", {
    bv <- RDBEScore::H7Example$BV
    sa <- RDBEScore::H7Example$SA
    summaryTable <- summariseBiologicalVariables(RDBEScore::H7Example,
                                                 var = "Age", by = "SAspeCode")

    ages <- bv[bv$BVtypeMeas == "Age", ]
    ageSpecies <- sa$SAspeCode[match(ages$SAid, sa$SAid)]
    expected <- tapply(as.numeric(ages$BVvalueMeas), ageSpecies, mean)
    expect_equal(as.character(summaryTable$SAspeCode), names(expected))
    expect_equal(summaryTable$mean, as.vector(expected))
  })

  test_that("plots are returned for each type of measurement", {
    plots <- summariseBiologicalVariables(RDBEScore::H8ExampleEE1,
                                          var = c("Length", "Sex"),
                                          output_type = "plot")
    expect_equal(names(plots), c("LengthTotal", "Sex"))
    expect_s3_class(plots$LengthTotal, "ggplot")
  })

  test_that("invalid input gives errors", {
    expect_error(summariseBiologicalVariables(RDBEScore::H8ExampleEE1,
                                              by = "notAColumn"),
                 "Column\\(s\\) not found: notAColumn")
    expect_error(summariseBiologicalVariables(RDBEScore::H8ExampleEE1,
                                              year = 1900),
                 "No BV data")
  })

}) ## end capture.output
