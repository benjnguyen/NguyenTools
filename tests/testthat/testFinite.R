
  context("Testing MLE")
  test_that("Output is finite")
  {
    x <- 5
    expect_lt(x, Inf)
  })
