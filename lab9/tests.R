source("functions.R")
context("Test for range value") 
test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(stat_range(x), 4)
  expect_length(stat_range(x), 1)
  expect_type(stat_range(x), 'double')
})

context("Test for range value") 
test_that("range works as expected", {
  y <- c(1, 2, 3, 4, 5)
  
  expect_length(stat_range(y), 1)
  expect_type(stat_range(y), 'NA_real_')
})

context("Test for range value") 
test_that("range works as expected", {
  z <- c(1, 2, 3, 4, 5)
  
  expect_equal(stat_range(z), 1L)
  expect_length(stat_range(z), 1)
  expect_type(stat_range(z), 'integer')
})

context("Test for range value") 
test_that("range works as expected", {
  w <- c("hey")
  
  expect_error(stat_range(w), "non-numeric argument to binary operator")
})



context("Test for stat centers") 
test_that("stat centers works as expected", {
  w <- c(1, 3, 8)
  expect_equal(stat_centers(w), c(3, 4))
  expect_length(stat_centers(w), 2)
  expect_type(stat_centers(w), 'double')
})

context("Test for stat spreads") 
test_that("stat spreads works as expected", {
  w <- c(1, 10, 10, 10)
  expect_equal(stat_spreads(w), c(9, 2.25, 4.5))
  expect_length(stat_spreads(w), 3)
  expect_type(stat_spreads(w), 'double')
})