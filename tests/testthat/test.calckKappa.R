library("rhoR");
context("Test rho()")

test_that("kappa is correct", {
  expect_equal(floor(calcKappa(contingencyTable, isSet = F)*10000), 6250)
})