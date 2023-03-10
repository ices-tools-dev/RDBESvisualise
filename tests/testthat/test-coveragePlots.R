#capture.output({  ## suppresses printing of console output when running test()

prepareTestData <- function(){

  # Prepare some test data
  myH1RawObject <-
    RDBEScore::createRDBESDataObject(rdbesExtractPath = "h1_v_1_19_13")
  # Generate some quarters for CL (test data is all Q1)
  set.seed(1)
  myH1RawObject[['CL']]$CLquar <-
    as.integer(runif(nrow(myH1RawObject[['CL']]), min = 1, max= 4.99))
  # Generate some quarters for CE (test data is all Q1)
  set.seed(1)
  myH1RawObject[['CE']]$CEquar <-
    as.integer(runif(nrow(myH1RawObject[['CE']]), min = 1, max= 4.99))
  # Generate some values for SAsampWtLive (test data is all blank)
  set.seed(1)
  myH1RawObject[['SA']]$SAsampWtLive <-
    as.integer(runif(nrow(myH1RawObject[['SA']]), min = 1, max= 200))
  # Generate some stat rectagnle values (test data value is all NA)
  set.seed(1)
  myH1RawObject[['SA']]$SAstatRect <-
    sample(unique(myH1RawObject[['CL']]$CLstatRect),
           size = nrow(myH1RawObject[['SA']]),
           replace = TRUE)

  myH1RawObject

}

test_that("Species landings plot runs without errors",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Species, landings plot
  expect_error(
    coverageLandingsBySpecies(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan"
    )
    ,NA)
})



test_that("Temporal landings plot runs without errors",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Temporal, landings plot
  expect_error(
    coverageLandingsTemporal(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      commercialVariable = "CLoffWeight",
      samplingVariable = "SAsampWtLive"
    )
    ,NA)
})

test_that("Stat rectangles points landings runs without errors",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"


  # Stat rectangles, points, landings plot
  expect_error(
    coverageLandingsSpatial(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      var = "Statrec",
      catchCat = "Lan",
      commercialVariable = "CLoffWeight",
      samplingVariable = "SAsampWtLive"
    )
    ,NA)

})

test_that("Stat rectangles bivariate landings runs without errors",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Stat rectangles, bivariate, landings plot
  expect_error(
    coverageLandingsSpatialBivariate(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      commercialVariable = "CLoffWeight",
      samplingVariable = "SAsampWtLive",
    )
    ,NA)

})


#}) ## end capture.output
