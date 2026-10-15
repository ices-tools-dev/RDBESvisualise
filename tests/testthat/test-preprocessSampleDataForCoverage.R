# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Uses the RDBEScore 0.4.0 example data for hierarchies 1, 5, 7
# and 8 (#25, #34, #19, #33, #18). In these data each sample has its own row
# in the table with the date, so the expected years and quarters are counted
# directly from that table.
capture.output({  ## suppresses printing of console output when running test()

  # Counts of the samples by year and quarter of the given dates
  countYearQuarter <- function(dates) {
    dates <- as.Date(as.character(dates))
    table(paste(format(dates, "%Y"), lubridate::quarter(dates)))
  }

  test_that("the sample date is taken from FO for hierarchy 1", {
    x <- RDBEScore::H1Example
    expect_equal(nrow(x$FO), nrow(x$SA))

    sa <- preprocessSampleDataForCoverage(x)

    expect_equal(nrow(sa), nrow(x$SA))
    expect_equal(table(paste(sa$SAyear, sa$SAquar)),
                 countYearQuarter(x$FO$FOendDate))
  })

  test_that("the sample date is taken from LE for hierarchies 5, 7 and 8", {
    # In H5 the samples are linked to LE, not to FT
    expect_true(all(is.na(RDBEScore::H5Example$SS$FTid)))
    for (x in list(RDBEScore::H5Example, RDBEScore::H7Example,
                   RDBEScore::H8ExampleEE1)) {
      expect_null(x$FO)
      expect_equal(nrow(x$LE), nrow(x$SA))

      sa <- preprocessSampleDataForCoverage(x)

      expect_equal(table(paste(sa$SAyear, sa$SAquar)),
                   countYearQuarter(x$LE$LEdate))
      expect_equal(table(sa$SAmonth),
                   table(lubridate::month(as.Date(x$LE$LEdate))))
    }
  })

  test_that("samples without a date get the year from DEyear with a warning", {
    x <- RDBEScore::H8ExampleEE1
    x$LE <- data.table::copy(x$LE)
    x$LE$LEdate <- NA

    expect_warning(
      sa <- preprocessSampleDataForCoverage(x),
      "No sample date found for 15 of 15 samples"
    )
    expect_true(all(sa$SAyear == x$DE$DEyear))
    expect_true(all(is.na(sa$SAquar)))
    expect_true(all(is.na(sa$SAmonth)))
  })

  test_that("missing SAgear is taken from FOgear or LEgear (#33)", {
    # H1: some SAgear values are empty
    x <- RDBEScore::H1Example
    expect_true(any(x$SA$SAgear == ""))
    sa <- preprocessSampleDataForCoverage(x)
    expect_false(any(is.na(sa$SAgear) | sa$SAgear == ""))

    # H7 and H8: all SAgear values are empty and there is no FO table
    for (x in list(RDBEScore::H7Example, RDBEScore::H8ExampleEE1)) {
      expect_true(all(x$SA$SAgear == ""))
      sa <- preprocessSampleDataForCoverage(x)
      expect_equal(table(sa$SAgear), table(x$LE$LEgear))
    }
  })

  test_that("missing SAstatRect and SAarea are taken from LE", {
    x <- RDBEScore::H8ExampleEE1
    expect_true(all(x$SA$SAstatRect == ""))
    sa <- preprocessSampleDataForCoverage(x)
    expect_equal(table(sa$SAstatRect), table(x$LE$LEstatRect))
    expect_equal(table(sa$SAarea), table(x$LE$LEarea))
  })

  test_that("vessel details are added, also when VD has no rows", {
    sa <- preprocessSampleDataForCoverage(RDBEScore::H8ExampleEE1)
    expect_equal(sort(unique(sa$VDflgCtry)),
                 sort(unique(RDBEScore::H8ExampleEE1$VD$VDflgCtry)))
    expect_true(all(sa$VDlenCat %in% RDBEScore::H8ExampleEE1$VD$VDlenCat))

    x <- RDBEScore::H7Example
    expect_equal(nrow(x$VD), 0)
    sa <- preprocessSampleDataForCoverage(x)
    expect_true(all(is.na(sa$VDflgCtry)))
  })

  test_that("more than one hierarchy gives an error", {
    x <- RDBEScore::H1Example
    x$DE <- data.table::copy(x$DE)
    x$DE$DEhierarchy[1] <- 2

    expect_error(preprocessSampleDataForCoverage(x), "single hierarchy")
  })

}) ## end capture.output
