# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the RDBEScore 0.4.0 H1 example data (#13, #30). The
# expected counts are calculated with table() on the SA table.
capture.output({  ## suppresses printing of console output when running test()

  test_that("sampled and not sampled units are counted per table", {
    x <- RDBEScore::H1Example
    counts <- summariseSampledNonSampled(x, output_type = "table")

    expect_equal(unique(counts$table), c("DE", "VS", "FT", "FO", "SS", "SA"))
    saSampled <- counts[counts$table == "SA" & counts$variable == "Sampled", ]
    expected <- table(x$SA$SAsamp)
    expect_equal(saSampled$value, names(expected))
    expect_equal(saSampled$n, as.vector(expected))
    expect_equal(saSampled$percentage,
                 as.vector(100 * expected / sum(expected)))
  })

  test_that("reasons are counted for the units that were not sampled", {
    x <- RDBEScore::H1Example
    counts <- summariseSampledNonSampled(x, tables = "SA",
                                         output_type = "table")

    reasons <- counts[counts$variable == "Reason for not sampling", ]
    # The not sampled SA units in the example data have no reason
    expect_equal(reasons$value, "(missing)")
    expect_equal(reasons$n, sum(x$SA$SAsamp == "N"))
    expect_equal(reasons$percentage, 100)
  })

  test_that("a plot is returned and unknown tables give an error", {
    expect_s3_class(summariseSampledNonSampled(RDBEScore::H1Example), "ggplot")
    expect_error(
      summariseSampledNonSampled(RDBEScore::H1Example, tables = "CL"),
      "No samp data in table\\(s\\): CL"
    )
  })

}) ## end capture.output
