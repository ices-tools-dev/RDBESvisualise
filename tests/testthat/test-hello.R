#capture.output({  ## suppresses printing of console output when running test()

test_that("hello runs without errors or warnings",  {
  myObject <- expect_warning(hello,NA)
  myObject <-expect_error(hello,NA)
})


#}) ## end capture.output
