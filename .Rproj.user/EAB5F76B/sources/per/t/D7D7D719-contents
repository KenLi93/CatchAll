library(CatchAll)
library(breakaway)
library(testthat)
context("Best models are the same")

data(apples)

test_that("Analysis of the apples dataset gives the same best model", {

  apple_best_model <- best_model(apples)
  expect_equal(apple_best_model$name,
               "Three-component Geometric mixture Model")
  expect_equal(apple_best_model$other$cutoff,
               163)

})
