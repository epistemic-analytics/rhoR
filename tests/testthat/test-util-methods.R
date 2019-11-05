library(testthat)
context("Testing utility methods within package")

test_that("p value", {
  dist <- seq(60, 80) / 100
  dist <- dist[dist != 0.7]

  expect_equal(getBootPvalue(dist, 0.6), 1)
  expect_equal(getBootPvalue(dist, 0.7), 0.5)
})

test_that("brpk values", {
  expect_false(checkBRPKcombo(0.2, 0.2, 0.8))
  expect_true(checkBRPKcombo(0.2, 0.8, 0.8))
})

test_that("p combos", {
  p_min <- 0.80;
  p_max <- 0.85;
  
  combo <- genPcombo(0.8, p_min, p_max, 0.5)
  expect_true(p_min < combo && combo < p_max)
})

test_that("create random sets", {
  len <- 200
  br <- 0.2
  set <- rhoR:::createRandomSet(len, br, 0.4, 0.65, 0, 1)
  
  expect_gte(sum(set[,1]), len * br)
  expect_gte(floor(kappa(set) * 100) / 100, 0.4)
  expect_lte(floor(kappa(set) * 100) / 100, 0.65)
})

test_that("rho min", {
  br <- 0.2
  rho_min <- rhoMin(br)
  expect_lte(rho_min, 30)
  
  expect_error(rhoMin(br, inc = 12.5), regexp = "Inc value must")
  expect_error(rhoMin(br, alpha = 2), regexp = "Alpha must")
  
  expect_output(rhoR:::rhoMin(br, printInc = TRUE), regexp = "10\\s0\\.")
})

test_that("rho given kappa errors", {
  expect_error(rhoK(2), regexp = "between 0 and 1")
  expect_error(rhoK(-2), regexp = "between 0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = -1), regexp = "OcSBaserate must be positive")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 1.5), regexp = "testSetLength.*?positive integer")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, OcSLength = 2.5), regexp = "OcSLength.*?positive integer")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, replicates = 1.5), regexp = "Replicates.*?positive integer")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSKappaThreshold = -1), regexp = "ScSKappaTh.*?positive")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSKappaMin = -1), regexp = "ScSKappaMin.*?positive")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSKappaMin = 0.9, ScSKappaThreshold = 0.8), regexp = "ScSKappaThresh.*?greater.*?Min")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMin = -2), regexp = "ScSPrecisionMin.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMin = 2), regexp = "ScSPrecisionMin.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMax = -2), regexp = "ScSPrecisionMax.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMax = 2), regexp = "ScSPrecisionMax.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMin = 0.6, ScSPrecisionMax = 0.5), regexp = "ScSPrecisionMax.*?greater.*?Min")
})