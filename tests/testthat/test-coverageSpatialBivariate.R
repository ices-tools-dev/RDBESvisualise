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

test_that("Spatial bivariate plot runs without errors for landings only",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Spatial bivariate plot
  expect_error(
    plots <- coverageSpatialBivariate(
        dataToPlot = myH1RawObject,
        year = myYear,
        vesselFlag = myvesselFlag,
        catchCat = "Lan",
        landingsVariable = "CLoffWeight",
        effortVariable = "CEnumFracTrips",
        samplingVariable = "SAsampWtLive",
        includeLandings = TRUE,
        includeEffort = FALSE
      )
    ,NA)

  # expect 1 girafe object
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"girafe")
})

test_that("Spatial bivariate plot runs without errors for effort only",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Spatial bivariate plot
  expect_error(
    plots <- coverageSpatialBivariate(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = FALSE,
      includeEffort = TRUE
    )
    ,NA)

  # expect 1 girafe object
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"girafe")
})




test_that("Spatial bivariate plot runs without errors for landinsg and effort",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Spatial bivariate plot
  expect_error(
    plots <- coverageSpatialBivariate(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = TRUE
    )
    ,NA)

  # expect 2 girafe objects
  expect_equal(length(plots),2)
  expect_s3_class(plots[[1]],"girafe")
  expect_s3_class(plots[[2]],"girafe")
})


#}) ## end capture.output
