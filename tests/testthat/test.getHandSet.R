library(testthat)
context("Test getHandset")

test_that("default getHandSet tests", {
  set_length <- 10
  base_rate <- 0.1
  
  test_set<- getHandSet(codeSet, set_length, base_rate, returnSet = TRUE)
  
  test_set_br <- baserate(test_set)
  
  expect_equal(colnames(test_set), c("firstRater", "secondRater"))
  expect_gte(test_set_br$firstBaserate, base_rate)
})

test_that("getHandSet tests without enought positives", {
  set_length <- 40
  base_rate <- 0.8
  
  expect_error(getHandSet(codeSet, set_length, base_rate, returnSet = TRUE))
})

test_that("default getHandSet return kappa", {
  set_length <- 40
  base_rate <- 0
  
  # pull the full example rhoR::codeSet, with expected K of 0.625
  hand_set_kappa <- getHandSet(codeSet, set_length, base_rate)
  
  expect_equal(hand_set_kappa, 0.625)
})
