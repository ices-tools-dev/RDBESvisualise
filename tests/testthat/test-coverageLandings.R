#capture.output({  ## suppresses printing of console output when running test()

test_that("coverageLandings runs without errors",  {

  myH1RawObject <-
    RDBEScore::createRDBESDataObject(rdbesExtractPath = "h1_v_1_19_13")
  # Generate some quarters for CL (test data is all Q1)
  set.seed(1)
  myH1RawObject[['CL']]$CLquar <- as.integer(runif(nrow(myH1RawObject[['CL']]), min = 1, max= 4.99))
  # Generate some values for SAsampWtLive (test data is all blank)
  set.seed(1)
  myH1RawObject[['SA']]$SAsampWtLive <- as.integer(runif(nrow(myH1RawObject[['SA']]), min = 1, max= 200))
  # Generate some stat rectagnle values (test data value is all NA)
  set.seed(1)
  myH1RawObject[['SA']]$SAstatRect <- sample(unique(myH1RawObject[['CL']]$CLstatRect), size = nrow(myH1RawObject[['SA']]), replace = TRUE)

  myYear = 1965
  myvesselFlag = "ZW"

  # p  <-  coverageLandings(
  #   dataToPlot = myH1RawObject,
  #   year = myYear,
  #   vesselFlag = myvesselFlag,
  #   var = "Statrec",
  #   catchCat = "Lan",
  #   commercialVariable = "CLoffWeight",
  #   samplingVariable = "SAsampWtLive",
  #   spatialPlot = "Points"
  # )
  # p[1]


  # Species, landings plot
  expect_error(
    coverageLandings(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      var = "species",
      catchCat = "Lan"
    )
    ,NA)


  # Gear, landings plot
  expect_error(
    coverageLandings(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      var = "gear",
      catchCat = "Lan"
    )
    ,NA)

  # Temporal, landings plot
  expect_error(
    coverageLandings(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      commercialVariable = "CLoffWeight",
      samplingVariable = "SAsampWtLive"
    )
    ,NA)

  # Stat rectangles, landings plot
  expect_error(
    coverageLandings(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      var = "Statrec",
      catchCat = "Lan",
      commercialVariable = "CLoffWeight",
      samplingVariable = "SAsampWtLive",
      spatialPlot = "Points"
    )
    ,NA)

})


#}) ## end capture.output
