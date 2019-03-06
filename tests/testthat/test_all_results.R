library(CatchAll)
library(breakaway)
library(testthat)
context("All models have the correct format")

data(apples)

test_that("Analysis of the apples dataset gives similar results", {

  apple_results_new <- all_parametric_model(apples)
  apple_results_old <- read.csv("./tests/apple_Analysis.csv")

  apple_results_new <- apple_results_new[, c("Model", "tau", "Est", "SE")]
  apple_results_old <- apple_results_old[, c("Model", "Cutoff", "Estimated.Total.Sp", "SE")]

  names(apple_results_new) <- c("Model", "tau", "New.Est", "New.SE")
  apple_results_new$Model <- as.factor(apple_results_new$Model)

  names(apple_results_old) <- c("Model", "tau", "Old.Est", "Old.SE")
  apple_results_old$Model <- as.factor(apple_results_old$Model)

  apples_merged <- merge(apple_results_new, apple_results_old)

  for (ii in 1:nrow(apples_merged)) {
    expect_lte(abs(apples_merged$New.Est[ii] - apples_merged$Old.Est[ii]),
               0.05 * apples_merged$Old.Est[ii])
    expect_lte(abs(apples_merged$New.SE[ii] - apples_merged$Old.SE[ii]),
               0.05 * apples_merged$Old.SE[ii])
  }
})
