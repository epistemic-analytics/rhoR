test_that("verify precision kappa combinations", {
  kappa_dist <- seq(0.01, 0.9, length = 100)
  prec_dist <- 0.21
  
  pk <- genPKcombo(kappa_dist, NULL, prec_dist, NULL, 0.2)
  expect_lte(pk$kappa, 0.02)
  expect_equal(pk$precision, 0.21)
})

test_that("create KP values", {
  kps_with_bell <- generateKPs(10, 0.2, 0.7, 0.9, 0.6, 0.9, distributionType = 'BELL')
  kp_kappas <- sapply(kps_with_bell, `[[`, "kappa")
  kp_precs <- sapply(kps_with_bell, `[[`, "precision")
  
  expect_false(any(is.na(c(kp_kappas, kp_precs))))
  expect_true(all(kp_kappas < 0.9 & kp_kappas > 0.7))
})

test_that("force KP error", {
  expect_error(generateKPs(10, 0.2, 0.7, 0.7, 0.1, 0.1), regexp = "Could not.*?ranges of kappa and precision")
})