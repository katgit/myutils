test_that("numeric_summary function works", {

  x <- c(1, 3, 5, 7, 9)
  result <- numeric_summary(x)

  expected_result <- c(min=1.0, max=9.0,
                       mean=5.0, sd=3.16228,
                       length=5., Nmiss=0)

  expect_equal( round(result, 5), expected_result)



  x <- c(0,0,0,0,0)
  result <- numeric_summary(x)

  expected_result <- c(min=0.0, max=0.0,
                       mean=0.0, sd=0,
                       length=5., Nmiss=0)

  expect_equal( round(result, 5), expected_result)

})



test_that("numeric_summary function produces appropriate error messages", {
  x <- c(F, F, T, T, T, F)
  expect_error( numeric_summary(x) )

})
