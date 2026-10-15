# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the RDBEScore 0.4.0 H1 and H8 example data (#12). The
# expected counts are calculated with table() on the VS table.
capture.output({  ## suppresses printing of console output when running test()

  test_that("selection methods are counted per table", {
    x <- RDBEScore::H1Example
    counts <- summariseSelectionMethod(x, output_type = "table")

    expect_equal(unique(counts$table),
                 c("VS", "FT", "FO", "SS", "SA", "BV"))
    vs <- counts[counts$table == "VS", ]
    expected <- table(x$VS$VSselectMeth)
    expect_equal(vs$value, names(expected))
    expect_equal(vs$n, as.vector(expected))
    expect_equal(sum(vs$percentage), 100)
  })

  test_that("cluster selection methods are not included", {
    counts <- summariseSelectionMethod(RDBEScore::H1Example,
                                       output_type = "table")
    expect_false(any(grepl("Cluster", counts$value)))
  })

  test_that("tables can be chosen and a plot is returned", {
    counts <- summariseSelectionMethod(RDBEScore::H8ExampleEE1,
                                       tables = c("VS", "LE"),
                                       output_type = "table")
    expect_equal(unique(counts$table), c("VS", "LE"))

    expect_s3_class(summariseSelectionMethod(RDBEScore::H8ExampleEE1),
                    "ggplot")
  })

}) ## end capture.output
