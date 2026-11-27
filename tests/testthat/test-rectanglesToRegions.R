test_that("rectangles convert to the expected levels", {
  rects <- c("41F6", "39G5", "28E8", "44G0", "58C5", "36E6")

  expect_equal(
    rectanglesToRegions(rects, from = "StatRec", to = "Area"),
    c("27.4.b", "27.3.d.25", "27.7.d", "27.3.a.20", "27.5.a.2", "27.7.a")
  )
  expect_equal(
    rectanglesToRegions(rects, from = "StatRec", to = "Division"),
    c("27.4.b", "27.3.d", "27.7.d", "27.3.a", "27.5.a", "27.7.a")
  )
  expect_equal(
    rectanglesToRegions(rects, from = "StatRec", to = "SubArea"),
    c("27.4", "27.3", "27.7", "27.3", "27.5", "27.7")
  )
  expect_equal(
    rectanglesToRegions(rects, from = "StatRec", to = "Region"),
    c("NSEA", "BA", "NSEA", "NSEA", "NSEA", "NAtl")
  )
  expect_equal(
    rectanglesToRegions("41F6", from = "StatRec", to = "FAOArea"),
    "27"
  )
})

test_that("output keeps the length and order of the input", {
  expect_equal(
    rectanglesToRegions(c("39G5", "41F6", "39G5", NA), from = "StatRec", to = "Region"),
    c("BA", "NSEA", "BA", NA)
  )
})

test_that("areas convert up the hierarchy", {
  expect_equal(
    rectanglesToRegions(c("27.3.d.28.1", "27.5.b.1.a", "27.4.a"),
                        from = "Area", to = "SubDivision"),
    c("27.3.d.28", "27.5.b.1", NA)
  )
  expect_equal(
    rectanglesToRegions(c("27.3.d.28", "27.3.c.22"),
                        from = "SubDivision", to = "Region"),
    c("BA", "BA")
  )
  expect_equal(
    rectanglesToRegions(c("27.7.d", "27.7.e", "27.3.a"),
                        from = "Division", to = "Region"),
    c("NSEA", "NAtl", "NSEA")
  )
})

test_that("RCG regions follow the RCG split", {
  # 27.5.a, 27.12 and 27.14 belong to NSEA, 27.5.b to NAtl
  expect_equal(
    rectanglesToRegions(c("27.5.a", "27.5.b"), from = "Division", to = "Region"),
    c("NSEA", "NAtl")
  )
  expect_equal(
    rectanglesToRegions(c("27.12", "27.14", "27.4", "27.6"), from = "SubArea", to = "Region"),
    c("NSEA", "NSEA", "NSEA", "NAtl")
  )
  # every division lies in a single region
  divReg <- unique(icesStatRecLookup[, c("Division", "Region")])
  expect_false(anyDuplicated(divReg$Division) > 0)
})

test_that("level is detected from the code", {
  expect_equal(
    detectIcesLevel(c("37G6", "27", "27.4", "27.4.a", "27.3.d.25", "27.3.d.28.1", NA)),
    c("StatRec", "FAOArea", "SubArea", "Division", "SubDivision", "Area", NA)
  )
  expect_equal(
    rectanglesToRegions(c("41F6", "27.7.e", "27.3.d"), "Region"),
    c("NSEA", "NAtl", "BA")
  )
})

test_that("codes already at the target level are returned unchanged", {
  expect_equal(rectanglesToRegions("27.4", from = "SubArea", to = "SubArea"), "27.4")
  expect_equal(rectanglesToRegions(c("41F6", "27.4.a"), "Division"),
               c("27.4.b", "27.4.a"))
  expect_warning(res <- rectanglesToRegions("27.4.zzz", "Division"), "27.4.zzz")
  expect_true(is.na(res))
})

test_that("lookup table is complete", {
  expect_false(anyNA(icesStatRecLookup$Region))
  expect_false(anyDuplicated(icesStatRecLookup$StatRec) > 0)
})

test_that("ambiguous and invalid conversions fail", {
  # 27.7 and 27.5 are split between NSEA and NAtl
  expect_error(rectanglesToRegions("27.7", from = "SubArea", to = "Region"),
               "more than one")
  expect_error(rectanglesToRegions("27.5", from = "SubArea", to = "Region"),
               "more than one")
  expect_error(rectanglesToRegions("27.3.d", from = "Division", to = "Area"),
               "more than one")
  expect_error(rectanglesToRegions("41F6", from = "Rectangle", to = "Region"))
})

test_that("unknown codes give NA with a warning", {
  expect_warning(
    res <- rectanglesToRegions(c("41F6", "99Z9"), from = "StatRec", to = "Region"),
    "99Z9"
  )
  expect_equal(res, c("NSEA", NA))
})
