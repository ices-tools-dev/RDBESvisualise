# AI-assisted: Yes
# Review: (pending)
# Notes/scope: Tests the colour palette (#7), number labels without scientific
# notation (#20), cropping of spatial data (#5) and the coverage plot title
# (#35). Uses the H1 test data in h1_v_20250211.
capture.output({  ## suppresses printing of console output when running test()

  test_that("the palette starts with the RCG colours and has enough colours", {
    expect_equal(getPaletteForPlotting(19), RCGPalette)
    expect_length(unique(getPaletteForPlotting(49)), 49)
    expect_length(getPaletteForPlotting(100), 100)
    expect_length(getPaletteForPlotting(0), 0)
  })

  test_that("the country colour palette is exported with one colour per country", {
    expect_length(RDBESvisualise::colourPalette, 47)
    expect_false(anyDuplicated(names(RDBESvisualise::colourPalette)) > 0)
    expect_true(all(grepl("^#[0-9A-F]{6}$", RDBESvisualise::colourPalette)))
  })

  test_that("numbers are formatted without scientific notation", {
    oldOptions <- options(scipen = 0)
    on.exit(options(oldOptions))

    expect_equal(formatNumber(c(1e5, 2.5e7)), c("100,000", "25,000,000"))
    expect_equal(getOption("scipen"), 0)

    quantileData <- getQuantiles(data.frame(var1 = c(1e5, 2e6, 3e7, NA)))
    expect_false(any(grepl("e\\+", levels(quantileData$mean_quantiles))))
    expect_equal(levels(quantileData$mean_quantiles)[1], "100,000 - 733,333")
  })

  test_that("spatial data is cropped to the plot extent plus a margin", {
    plotExtent <- sf::st_bbox(c(xmin = 20, ymin = 58, xmax = 22, ymax = 60),
                              crs = sf::st_crs(4326))

    cropped <- cropToExtent(RDBESvisualise::shoreline, plotExtent)
    croppedExtent <- sf::st_bbox(cropped)

    expect_s3_class(cropped, "sf")
    expect_gt(nrow(cropped), 0)
    expect_lt(nrow(cropped), length(RDBESvisualise::shoreline))
    # The edges of the crop are great circles, so they can bulge a little
    expect_true(croppedExtent["xmin"] >= 18.9 && croppedExtent["xmax"] <= 23.1)
    expect_true(croppedExtent["ymin"] >= 56.9 && croppedExtent["ymax"] <= 61.1)
  })

  test_that("the plot title says that catchCat is for the sample data", {
    myH1RawObject <- RDBEScore::createRDBESDataObject(input = "h1_v_20250211")
    plots <- suppressWarnings(coverageByGear(
      dataToPlot = myH1RawObject,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      samplingVariable = "SAnumSamp",
      includeEffort = FALSE
    ))

    plotLayout <- plotly::plotly_build(plots[[1]])$x$layout
    expect_match(unlist(plotLayout$title), "Sampling catchCat=Lan",
                 fixed = TRUE, all = FALSE)
  })

}) ## end capture.output
