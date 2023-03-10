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

test_that("Gear plot runs without errors for landings only",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear, landings plot
  expect_error(
    coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      includeLandings = TRUE,
      includeEffort = FALSE,
      includeSamples = FALSE,
    )
    ,NA)
})

test_that("Gear plot runs without errors for effort only",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear, landings plot
  expect_error(
    coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      includeLandings = FALSE,
      includeEffort = TRUE,
      includeSamples = FALSE,
    )
    ,NA)
})

test_that("Gear plot runs without errors for samples only",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear, landings plot
  expect_error(
    coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      includeLandings = FALSE,
      includeEffort = FALSE,
      includeSamples = TRUE,
    )
    ,NA)
})

test_that("Gear plot runs without errors for landing and samples",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear, landings plot
  expect_error(
    coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      includeLandings = TRUE,
      includeEffort = FALSE,
      includeSamples = TRUE,
    )
    ,NA)
})

test_that("Gear plot runs without errors for effort and samples",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear, landings plot
  expect_error(
    coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      includeLandings = FALSE,
      includeEffort = TRUE,
      includeSamples = TRUE,
    )
    ,NA)
})

test_that("Gear plot runs without errors for landinsg and effort",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear, landings plot
  expect_error(
    coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      includeLandings = TRUE,
      includeEffort = TRUE,
      includeSamples = FALSE,
    )
    ,NA)
})

test_that("Gear plot runs without errors for landings, effort, and samples",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear, landings plot
  expect_error(
    coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      includeLandings = TRUE,
      includeEffort = TRUE,
      includeSamples = TRUE,
    )
    ,NA)
})



#}) ## end capture.output
