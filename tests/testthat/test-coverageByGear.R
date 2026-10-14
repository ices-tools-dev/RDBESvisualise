capture.output({  ## suppresses printing of console output when running test()

prepareTestData <- function(){

  # Prepare some test data
  myH1RawObject <-
    RDBEScore::createRDBESDataObject(input = "h1_v_20250211")
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

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = FALSE,
      includeSamples = FALSE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")

})

test_that("Gear plot runs without errors for effort only",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = FALSE,
      includeEffort = TRUE,
      includeSamples = FALSE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")

})

test_that("Gear plot runs without errors for samples only",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = FALSE,
      includeEffort = FALSE,
      includeSamples = TRUE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")
})

test_that("Gear plot runs without errors for landing and samples",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = FALSE,
      includeSamples = TRUE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")
})

test_that("Gear plot runs without errors for effort and samples",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = FALSE,
      includeEffort = TRUE,
      includeSamples = TRUE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")
})

test_that("Gear plot runs without errors for landinsg and effort",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = TRUE,
      includeSamples = FALSE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")
})

test_that("Gear plot runs without errors for landings, effort, and samples",  {

  myH1RawObject <- prepareTestData()
  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = TRUE,
      includeSamples = TRUE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")
})

test_that("Gear plot runs without error if you try to plot sample data with empty VD",  {

  myH1RawObject <- prepareTestData()
  # Remove VD data rows
  myH1RawObject[["VD"]] <- myH1RawObject[["VD"]][1==0,]

  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myH1RawObject,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = TRUE,
      includeSamples = TRUE,
    )
    ,NA)

  # expect a single plotly, plot
  expect_equal(length(plots),1)
  expect_s3_class(plots[[1]],"plotly")
})

test_that("Gear plot stops with error if you try to plot sample data with null VD table",  {

  myH1RawObject <- prepareTestData()
  # Remove VD data
  myNewH1 <- RDBEScore::newRDBESDataObject (DE = myH1RawObject[["DE"]],
                                 SD = myH1RawObject[["SD"]],
                                 VS = myH1RawObject[["VS"]],
                                 FT = myH1RawObject[["FT"]],
                                 FO = myH1RawObject[["FO"]],
                                 TE = myH1RawObject[["TE"]],
                                 LO = myH1RawObject[["LO"]],
                                 OS = myH1RawObject[["OS"]],
                                 LE = myH1RawObject[["LE"]],
                                 SS = myH1RawObject[["SS"]],
                                 SA = myH1RawObject[["SA"]],
                                 FM = myH1RawObject[["FM"]],
                                 BV = myH1RawObject[["BV"]],
                                 VD = NULL,
                                 SL = myH1RawObject[["SL"]],
                                 IS = myH1RawObject[["IS"]],
                                 CL = myH1RawObject[["CL"]],
                                 CE = myH1RawObject[["CE"]])


  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myNewH1,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = TRUE,
      includeSamples = TRUE,
    )
    ,"No VD data present in sample data - this is required")


})

test_that("Gear plot stops with error if you try to plot sample data with null FO table",  {

  myH1RawObject <- prepareTestData()
  # Remove FO data
  myNewH1 <- RDBEScore::newRDBESDataObject (DE = myH1RawObject[["DE"]],
                                            SD = myH1RawObject[["SD"]],
                                            VS = myH1RawObject[["VS"]],
                                            FT = myH1RawObject[["FT"]],
                                            FO = NULL,
                                            TE = myH1RawObject[["TE"]],
                                            LO = myH1RawObject[["LO"]],
                                            OS = myH1RawObject[["OS"]],
                                            LE = myH1RawObject[["LE"]],
                                            SS = myH1RawObject[["SS"]],
                                            SA = myH1RawObject[["SA"]],
                                            FM = myH1RawObject[["FM"]],
                                            BV = myH1RawObject[["BV"]],
                                            VD = myH1RawObject[["BV"]],
                                            SL = myH1RawObject[["SL"]],
                                            IS = myH1RawObject[["IS"]],
                                            CL = myH1RawObject[["CL"]],
                                            CE = myH1RawObject[["CE"]])


  myYear <- 1965
  myvesselFlag <- "ZW"

  # Gear plot
  expect_error(
    plots <- coverageByGear(
      dataToPlot = myNewH1,
      year = myYear,
      vesselFlag = myvesselFlag,
      catchCat = "Lan",
      landingsVariable = "CLoffWeight",
      effortVariable = "CEnumFracTrips",
      samplingVariable = "SAsampWtLive",
      includeLandings = TRUE,
      includeEffort = TRUE,
      includeSamples = TRUE,
    )
    ,"No FO data present in sample data - this is required")


})

}) ## end capture.output
