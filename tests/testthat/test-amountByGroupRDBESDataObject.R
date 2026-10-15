# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the RDBEScore 0.4.0 H8 example data (#10). The expected
# sums are calculated with tapply() on the CL table.
capture.output({  ## suppresses printing of console output when running test()

  test_that("amountByGroup accepts an RDBESDataObject", {
    x <- RDBEScore::H8ExampleEE1
    result <- amountByGroup(x, var = "CLoffWeight", valBy = "CLquar",
                            verbose = FALSE)

    expected <- tapply(x$CL$CLoffWeight, x$CL$CLquar, sum)
    expect_equal(result$CLoffWeight[order(result$CLquar)],
                 as.vector(expected))
  })

  test_that("amountByGroup filters an RDBESDataObject", {
    x <- RDBEScore::H8ExampleEE1
    result <- amountByGroup(x, var = "SAsampWtLive", valBy = "SAcatchCat",
                            filters = list(SAcatchCat = "Lan"),
                            verbose = FALSE)
    expect_equal(result$SAsampWtLive,
                 sum(x$SA$SAsampWtLive[x$SA$SAcatchCat == "Lan"], na.rm = TRUE))
  })

}) ## end capture.output
