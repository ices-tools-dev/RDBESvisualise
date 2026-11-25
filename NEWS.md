# RDBESvisualise 0.2.1

## New features

* Added functions for producing sampling coverage plots:
  `coverageByGear()`, `coverageByGroupingVariable()`, `coverageBySpecies()`,
  `coverageSpatial()`, `coverageSpatialBivariate()`, `coverageTemporal()` and
  `produceSamplingCoverage()`.
* Added `summaryBiologicalVar()` for summarising biological variables.
* Added the plotting helpers `barPlotsByGroupingVariable()`,
  `bivariatePlot()`, `bivariatePlotCreateGraph()`, `spatialPlotCreateGraph()`,
  `temporalPlot()` and `rectanglesToRegions()`.
* Added vignettes describing the sampling coverage and summary functions.

## Bug fixes

* Fixed an error in `coverageSpatial()` caused by the deprecation of
  `fortify(<SpatialPolygonsDataFrame>)` in ggplot2. The function now uses
  `geom_sf()`.
* Fixed an error in `coverageSpatialBivariate()` when the number of unique
  values of a variable was close to the number of observations.
* Fixed an error in `summaryBiologicalVar()` when the grouping variable was not
  present in the data, and when `.SDcols` referred to columns that did not exist.

## Other changes

* Added `sp` to Imports.
* Added a GitHub Actions workflow that runs `R CMD check`.

# RDBESvisualise 0.1.1 (2026-03-09)

* Added `RDBEScore` and `magrittr` to Imports.
* Removed the `hello()` placeholder function.
* Renamed the `amountByGroup` vignette to `useAmountByGroup`.

# RDBESvisualise 0.1.0

* First release.
