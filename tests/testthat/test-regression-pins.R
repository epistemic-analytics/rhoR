library(testthat)
context("Regression pins (rhoR 1.3.2 outputs)")

# Pins the outputs of rhoR 1.3.2 so the move of its core into the qestats C++
# headers can be checked against them: deterministic values to 1e-12, seeded
# values exactly. A pin that changes means the port changed a result someone
# may have published; update it only on purpose.
#
# Known quirks pinned as they are, not as they should be:
# - rho is tie-sensitive: a bootstrap kappa can equal the observed kappa, and
#   getBootPvalue counts `>=`. kappa_ct (C++) and calcKappa (R) differ in the
#   last bit for the same table, which moves rho from 0.135 to 0.13625 below.
# - kappa_ct returns NaN for a table with a single non-empty cell; calcKappa
#   (kappaCT / kappaSet) returns 1.
# - sample_contingency_table (C++ path only) draws s in [0, total] and can pick
#   an empty cell; the seeded C++ pins below include that behaviour.
# - rhoCT ignores `method` (always the R path); rhoSet and rhoK honour it.

ns <- asNamespace("rhoR")
pin_seed <- function(seed) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
}
expect_pin <- function(object, expected) {
  expect_equal(as.vector(unlist(object)), expected, tolerance = 1e-12)
}

ct <- matrix(c(40L, 5L, 3L, 52L), 2)  # TP 40, FP 5, FN 3, TN 52
ct_set <- ns$contingencyToSet(40, 5, 3, 52)

test_that("recall, kappa and precision formulas", {
  expect_pin(ns$getR(0.8, 0.2, 0.9), 0.78260869565217406)
  expect_pin(ns$getR(0.5, 0.05, 0.7), 0.41176470588235298)
  expect_pin(ns$getR(0.95, 0.5, 1), 0.94999999999999996)
  expect_pin(recall(0.8, 0.2, 0.9), 0.78260869565217406)
  expect_pin(recall(0.5, 0.05, 0.7), 0.41176470588235298)

  expect_true(ns$checkBRPKcombo(0.2, 0.9, 0.8))
  expect_false(ns$checkBRPKcombo(0.2, 0.3, 0.8))
  expect_false(ns$check_BRK_combo(0.2, 0.3, 0.8))

  expect_pin(baserateCT(ct), c(0.43, 0.45, 0.44))
  expect_pin(ns$rating_set_recall(ct), 0.93023255813953487)
  expect_pin(ns$rating_set_precision(ct), 0.88888888888888884)
})

test_that("kappa from tables and sets", {
  # Hex literals: the two kappas are adjacent doubles, closer than R's decimal
  # parser reliably resolves.
  expect_identical(kappa_ct(ct), 0x1.aceab5be926f3p-1)
  expect_identical(kappaCT(ct), 0x1.aceab5be926f2p-1)
  expect_identical(kappaSet(ct_set), 0x1.aceab5be926f2p-1)

  one_cell <- matrix(c(10L, 0L, 0L, 0L), 2)
  expect_true(is.nan(kappa_ct(one_cell)))
  expect_equal(kappaCT(one_cell), 1)
  expect_equal(kappa_ct(matrix(c(10L, 0L, 0L, 10L), 2)), 1)
  expect_equal(kappaCT(matrix(c(10L, 0L, 0L, 10L), 2)), 1)
  expect_pin(kappa_ct(matrix(c(0L, 5L, 5L, 90L), 2)), -0.052631578947367252)
  expect_pin(kappaCT(matrix(c(0L, 5L, 5L, 90L), 2)), -0.052631578947367252)
  expect_equal(kappa_ct(matrix(c(0L, 10L, 10L, 0L), 2)), -1)
})

test_that("contingency tables and sets from precision / recall", {
  r <- ns$getR(0.8, 0.2, 0.9)
  expect_pin(contingency_table(0.9, r, 10000, 0.2), c(1565, 174, 435, 7826))
  expect_pin(ns$pr.ct(0.9, r, 10000, 0.2), c(1565, 174, 435, 7826))
  expect_pin(contingency_table(0.6, 0.3, 50, 0.01), c(1, 1, 0, 48))
  expect_pin(ns$pr.ct(0.6, 0.3, 50, 0.01), c(1, 1, 0, 48))

  s <- ns$prset(0.9, r, 1000, 0.2)
  expect_pin(c(nrow(s), colSums(s), ns$calcKappa(s)), c(1000, 200, 174, 0.80289093298291703))
})

test_that("bootstrap p-value", {
  d <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  expect_pin(c(ns$getBootPvalue(d, 0.35), ns$getBootPvalue(d, 0.2), ns$getBootPvalue(d, 0.5)), c(0.4, 1, 0.2))
  expect_pin(c(getBootPvalue_c(d, 0.35), getBootPvalue_c(d, 0.2), getBootPvalue_c(d, 0.5)), c(0.4, 1, 0.2))
})

test_that("seeded kappa / precision draws (R path)", {
  pin_seed(42)
  expect_pin(ns$genPKcombo(seq(0.4, 0.9, length = 1000), NULL, seq(0.6, 1, length = 1000), NULL, 0.2),
             c(0.99879879879879874, 0.68028028028028031))
  pin_seed(42)
  expect_pin(ns$generateKPs(5, 0.2, 0.4, 0.9, 0.6, 1), c(
    0.6947294729472947, 0.70706307063070639, 0.65004500450045, 0.61028710287102872,
    0.93155315531553162, 0.77180871808718088, 0.90799079907990798, 0.72054820548205489,
    0.81610161016101612, 0.44545045450454507))
  pin_seed(42)
  expect_pin(ns$generateKPs(5, 0.2, 0.4, 0.9, 0.6, 1, distributionType = "BELL"), c(
    0.6947294729472947, 0.72786827868278681, 0.9715971597159716, 0.7626386263862639,
    0.95303530353035304, 0.8295142951429515, 0.96795679567956794, 0.80517905179051796,
    0.90799079907990798, 0.71191311913119137))
  pin_seed(42)
  expect_pin(ns$generateKPs(5, 0.01, 0.4, 0.9, 0.6, 1), c(
    0.6947294729472947, 0.70706307063070639, 0.65004500450045, 0.61028710287102872,
    0.9401540154015402, 0.77180871808718088, 0.81610161016101612, 0.44545045450454507,
    0.82546254625462545, 0.85554455544555452))
})

test_that("seeded hand sets (R path)", {
  r <- ns$getR(0.8, 0.2, 0.9)
  pin_seed(7)
  s <- ns$prset(0.9, r, 1000, 0.2)
  expect_pin(c(getHandSet(s, 80, 0), getHandSet(s, 80, 0.2)), c(0.60975609756097582, 0.72356599861783))

  pin_seed(7)
  s <- ns$prset(0.9, r, 200, 0.2)
  expect_pin(as.vector(unclass(getHandSet(s, 20, 0.25, returnSet = TRUE))), c(
    1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0,
    1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0))

  pin_seed(7)
  s <- ns$prset(0.9, r, 200, 0.2)
  expect_pin(getHandSetIndices(s, 20, 0.25), c(
    184, 2, 180, 123, 25, 141, 31, 36, 153, 72, 45, 19, 28, 199, 64, 173, 139, 15, 13, 95))

  pin_seed(3)
  expect_pin(ns$createRandomSet(500, 0.2, 0.6, 0.9, type = "ct"), c(73, 10, 27, 390))
  pin_seed(3)
  expect_pin(colSums(createSimulatedCodeSet(500, 0.2, 0.6, 0.9, 0.6, 1)), c(100, 72))
})

test_that("seeded rho (default R path)", {
  pin_seed(1); expect_identical(ns$calcRho(0.88, 0.2, 80), 0.1025)
  pin_seed(1); expect_identical(rhoK(0.88, 0.2, 80), 0.1025)
  pin_seed(1); expect_identical(rho(0.88, 0.2, 80), 0.1025)
  pin_seed(1); expect_identical(rhoK(0.88, 0.2, 80, testSetBaserateInflation = 0.25), 0.085)
  pin_seed(1); expect_equal(rhoK(0.7, 0.05, 140, replicates = 300), 127 / 300)

  # Same table, same seed: rhoCT (kappa_ct) and rhoSet (calcKappa) differ only
  # through the last bit of kappa (see the header).
  pin_seed(1); expect_pin(rhoCT(ct), c(0.135, 0.83772819472616644, 0.93023255813953487, 0.88888888888888884))
  pin_seed(1); expect_pin(rho(ct), c(0.135, 0.83772819472616644, 0.93023255813953487, 0.88888888888888884))
  pin_seed(1); expect_pin(rhoSet(ct_set), c(0.13625, 0.83772819472616633, 0.93023255813953487, 0.88888888888888884))
  pin_seed(1); expect_pin(rho(ct_set), c(0.13625, 0.83772819472616633, 0.93023255813953487, 0.88888888888888884))

  pin_seed(1); expect_identical(rhoMin(0.2, inc = 10, replicates = 200), 40)
})

test_that("seeded C++ path", {
  pin_seed(42)
  expect_pin(sample_contingency_table(ct, 20, TRUE),
             c(4, 4, 1, 4, 4, 4, 4, 1, 4, 4, 2, 4, 4, 1, 2, 4, 4, 1, 2, 4))
  pin_seed(42)
  expect_pin(generate_kp_list(5, 0.2, 0.4, 0.9, 0.6, 1), c(
    0.97483748374837487, 0.93219321932193222, 0.80762076207620759, 0.89674967496749669,
    0.78309830983098305, 0.85744574457445744, 0.54306430643064307, 0.72088208820882094,
    0.76828682868286835, 0.75253525352535255))
  pin_seed(42)
  expect_pin(generate_kp_list(5, 0.2, 0.4, 0.9, 0.6, 1, 1), c(
    0.97483748374837487, 0.93219321932193222, 0.80762076207620759, 0.90411041104110412,
    0.78309830983098305, 0.72788278827882791, 0.86334633463346333, 0.80814081408140814,
    0.78818881888188819, 0.79528952895289529))
  pin_seed(42)
  expect_pin(random_contingency_table(500, 0.2, 0.6, 0.9), c(86, 6, 14, 394))

  full <- contingency_table(0.9, ns$getR(0.8, 0.2, 0.9), 1000, 0.2)
  pin_seed(7); expect_pin(ns$getHand_ct(full, 80, 0.2), c(17, 1, 7, 55))
  pin_seed(7); expect_pin(getHand_kappa(full, 80, 0.2), 0.74358974358974361)
  pin_seed(7); expect_pin(getHandCT(full, 80, 0.2), 0.74358974358974361)

  pin_seed(1); expect_identical(ns$calcRho_c(0.88, 0.2, 80), 0.11125)
  pin_seed(1); expect_identical(rhoK(0.88, 0.2, 80, method = "c"), 0.11125)
  pin_seed(1); expect_identical(rhoK(0.88, 0.2, 80, testSetBaserateInflation = 0.25, method = "c"), 0.10125)
  pin_seed(1); expect_pin(rhoSet(ct_set, method = "c"), c(0.165, 0.83772819472616633, 0.93023255813953487, 0.88888888888888884))
})
