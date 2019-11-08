library(testthat)
context("Testing utility methods within package")

test_that("p value", {
  dist <- seq(60, 80) / 100
  dist <- dist[dist != 0.7]

  expect_equal(getBootPvalue(dist, 0.6), 1)
  expect_equal(getBootPvalue(dist, 0.7), 0.5)
  
  expect_equal(getBootPvalue_c(rep(0.8, 10), 0.7), 1)
})

test_that("brpk values", {
  expect_false(checkBRPKcombo(0.2, 0.2, 0.8))
  expect_true(checkBRPKcombo(0.2, 0.8, 0.8))
})

test_that("p combos", {
  p_min <- 0.80;
  p_max <- 0.85;
  
  combo <- rhoR:::genPcombo(0.8, p_min, p_max, 0.5)
  expect_true(p_min < combo && combo < p_max)
  
  expect_error(rhoR:::genPcombo(0.9, p_min, p_max, 0.5))
})

test_that("create random sets", {
  len <- 200
  br <- 0.2
  set <- rhoR:::createRandomSet(len, br, 0.4, 0.65, 0, 1)
  
  expect_gte(sum(set[,1]), len * br)
  expect_gte(round(kappa(set), 2), 0.4)
  expect_lte(round(kappa(set), 2), 0.65)
  
  ct <- rhoR:::createRandomSet(len, br, 0.4, 0.65, 0, 1, type = "ct")
  expect_gte(sum(ct[1, ]), len * br)
  expect_gte(round(kappa(ct), 2), 0.4)
  expect_lte(round(kappa(ct), 2), 0.65)
  
  ct <- rhoR:::random_contingency_table(len, br, 0.5, 0.75, 0, 1)
  expect_gte(sum(ct[1, ]), len * br)
  expect_gte(round(kappa(ct), 2), 0.5)
  expect_lte(round(kappa(ct), 2), 0.75)
})

test_that("rho min", {
  br <- 0.2
  rho_min <- rhoMin(br)
  expect_lte(rho_min, 50)
  
  expect_error(rhoMin(br, inc = 12.5), regexp = "Inc value must")
  expect_error(rhoMin(br, alpha = 2), regexp = "Alpha must")
  
  expect_output(rhoR:::rhoMin(br, printInc = TRUE), regexp = "10\\s0\\.")
})

test_that("rho given kappa errors", {
  expect_error(rhoK(2, OcSBaserate = 0.5, testSetLength = 10), regexp = "between 0 and 1")
  expect_error(rhoK(-2, OcSBaserate = 0.5, testSetLength = 10), regexp = "between 0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = -1), regexp = "OcSBaserate.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 1.5), regexp = "testSetLength.*?positive integer")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, OcSLength = 2.5), regexp = "OcSLength.*?positive integer")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, replicates = 1.5), regexp = "replicates.*?positive integer")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSKappaThreshold = -1), regexp = "ScSKappaTh.*?positive")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSKappaMin = -1), regexp = "ScSKappaMin.*?positive")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSKappaMin = 0.9, ScSKappaThreshold = 0.8), regexp = "ScSKappaThresh.*?greater.*?Min")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMin = -2), regexp = "ScSPrecisionMin.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMin =  2), regexp = "ScSPrecisionMin.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMax = -2), regexp = "ScSPrecisionMax.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMax =  2), regexp = "ScSPrecisionMax.*?0 and 1")
  expect_error(rhoK(0.9, OcSBaserate = 0.2, testSetLength = 200, ScSPrecisionMin = 0.6, ScSPrecisionMax = 0.5), regexp = "ScSPrecisionMax.*?greater.*?Min")
})

test_that("create from PR", {
  prec <- 0.8
  rec <- 0.6
  hsl <- 40
  br <- 0.2
  
  set <- rhoR:::prset(prec, rec, hsl, br)
  ct  <-   contingency_table(prec, rec, hsl, br)

  set_tp <- sum(set[, 1] == 1 & set[, 2] == 1)
  set_tn <- sum(set[, 1] == 0 & set[, 2] == 0)
  set_fn <- sum(set[, 1] == 1 & set[, 2] == 0)
  set_fp <- sum(set[, 1] == 0 & set[, 2] == 1)
  
  expect_equal(set_tp, ct[1, 1])
  expect_equal(set_tn, ct[2, 2])
  expect_equal(set_fn, ct[1, 2])
  expect_equal(set_fp, ct[2, 1])
})

test_that("rhoK on set and ct", { 
  len = 1000
  br = 0.2
  set <- rhoR:::createRandomSet(len, br, 0.9, 1, 0.8, 1)

  set_k <- kappa(set)
  ocs_br <- 0.2
  tsl <- 80

  set_k_std <- replicate(10, rhoK(set_k, OcSBaserate = ocs_br, testSetLength = tsl))
  set_k_c   <- replicate(10, rhoK(set_k, OcSBaserate = ocs_br, testSetLength = tsl, method = "c"))
  testthat::expect_equivalent(mean(set_k_std), mean(set_k_c), tolerance = 0.01)
})

test_that("Generate KPs", {
  num <- 10000
  br  <- 0.2
  kmin <- 0.60
  kmax <- 0.65
  pmin <- 0.8
  pmax <- 0.85
  
  kps_list <- generateKPs(num, br, kmin, kmax, pmin, pmax)
  
  kps_r <- matrix(unlist(kps_list), byrow = TRUE, ncol = 2, dimnames = list(NULL, names(kps_list[[1]])))
  
  kps_c <- generate_kp_list(num, br, kmin, kmax, pmin, pmax)
  
  r_means <- colMeans(kps_r)
  c_means <- colMeans(kps_c)
  testthat::expect_true(rhoR:::all_equal(r_means[1], c_means[1], tolerance = 0.001))
  testthat::expect_true(rhoR:::all_equal(r_means[2], c_means[2], tolerance = 0.001))
})

test_that("CT kappa comparisons", {
  prec <- 0.8
  rec <- 0.6
  hsl <- 40
  br <- 0.2

  ct <- contingency_table(prec, rec, hsl, br)
  k_r <- kappaCT(ct)
  k_c <- kappa_ct(ct)
  
  testthat::expect_equivalent(k_r, k_c)
})

test_that("compare calcRhos", {
  num <- 100
  br  <- 0.2
  kmin <- 0.85
  kmax <- 0.95
  pmin <- 0.8
  pmax <- 0.95
  
  kps_list <- rhoR:::generateKPs(num, br, kmin, kmax, pmin, pmax)
  kps_r <- matrix(unlist(kps_list), byrow = TRUE, ncol = 2, dimnames = list(NULL, names(kps_list[[1]])))
  kps_c <- generate_kp_list(num, br, kmin, kmax, pmin, pmax)

  observedKappa = 0.91
  fullSetBaserate = 0.2
  handSetLength = 80
  handSetBaserateInflation = 0.2
  
  calc_rs <- replicate(10, rhoR:::calcRho(  observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation, KPs = kps_list))
  calc_cs <- replicate(10, rhoR:::calcRho_c(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation, KPs = kps_r))
  expect_true(rhoR:::all_equal(mean(calc_rs), mean(calc_cs), tolerance = 0.05))
  
  calc_rs <- replicate(50, rhoR:::calcRho(  observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation, KPs = kps_list, replicates = 50))
  calc_cs <- replicate(50, rhoR:::calcRho_c(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation, KPs = kps_r, replicates = 50))
  expect_true(rhoR:::all_equal(mean(calc_rs), mean(calc_cs), tolerance = 0.05))
  
  calc_rs <- replicate(10, rhoR:::calcRho(  observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation))
  calc_cs <- replicate(10, rhoR:::calcRho_c(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation))
  expect_true(rhoR:::all_equal(mean(calc_rs), mean(calc_cs), tolerance = 0.05))
})

test_that("kps from c", {
  num <- 10000
  br  <- 0.2
  kmin <- 0
  kmax <- 1
  pmin <- 0
  pmax <- 1

  kps_c <- generate_kp_list(num, br, kmin, kmax, pmin, pmax, distributionType = 1)
  expect_equal(mean(kps_c[,2]), 0.9, tolerance = 0.03)
})

test_that("ct equals set", {
  ct_as_set <- as.code.set(contingencyTable)
  expect_equal(ct_as_set[,1], codeSet[,1])
  expect_equal(ct_as_set[,2], codeSet[,2])
})
test_that("set equals ct", {
  set_as_ct <- as.contingency.table(codeSet)
  expect_equivalent(unclass(set_as_ct), contingencyTable)
})

test_that("verify rating set printing", {
  # tmp <- tempfile()
  # on.exit(unlink(tmp), add = TRUE)
  
  ct_as_set <- as.code.set(contingencyTable)
  # sink(tmp)
  # print(ct_as_set)

  # output <- readLines(tmp)
  # unlink(tmp)
  # sink(file = NULL)
  # testthat::expect_true(grepl(x = output, pattern = "\\s+\\[,1\\]\\s\\[,2\\]")[1])
  testthat::expect_output(print(ct_as_set), regexp = "first_rater second_rater" )
})

test_that("test dollar completion", {
  set_as_ct <- as.contingency.table(codeSet)
  expect_null(rhoR:::`$.rating.set`(set_as_ct, ""))
  expect_equal(rhoR:::`$.rating.set`(set_as_ct, "kappa"), 0.625)
  expect_equal(rhoR:::`$.rating.set`(set_as_ct, "second_negative"), 35)
  expect_equal(rhoR:::`$.rating.set`(set_as_ct, "first_positive"), 4)
  
  ct_as_set <- as.code.set(contingencyTable)
  expect_null(rhoR:::`$.rating.set`(ct_as_set, ""))
  expect_equal(rhoR:::`$.rating.set`(ct_as_set, "kappa"), 0.625)
  expect_equal(sum(rhoR:::`$.rating.set`(ct_as_set, "second_rater") == 0), 35)
  expect_true(all(rhoR:::.codegen_dollar(ct_as_set) %in% rhoR:::additional_attributes))
  
  expect_true(all(rhoR:::.DollarNames.rating.set(ct_as_set) %in% c("first_rater", "second_rater", "baserate", "kappa")))
  expect_true(all(rhoR:::.DollarNames.rating.set(set_as_ct) %in% c("first_positive", "first_negative",
                                                                   "second_positive", "second_negative",
                                                                   "agreement", "baserate", "kappa")))
})

# test_that("package loading", {
#   testthat::expect_error(rhoR:::.onLoad("", "rhoR"))
# })
