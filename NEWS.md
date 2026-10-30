## rhoR 1.3.3 (development version)
  * Regression tests pin the 1.3.2 outputs of every rho building block
    (tests/testthat/test-regression-pins.R), ahead of moving the core to
    the qestats C++ headers
  * rho's computation now lives in libqe-stats (namespace qestats), vendored
    in inst/include/qestats/ (tools/sync-qestats.sh updates it): rhoK's
    default method runs qestats::rho_standard in C++ instead of R (~15x
    faster), and the C++ functions (`method = "c"`, kappa_ct(),
    sample_contingency_table(), getHand_kappa(), ...) are thin wrappers over
    qestats::rho_fast. Seeded results are unchanged: every regression pin
    passes, and old and new builds agree on 2,046 seeded calls on macOS and
    Linux. Two edge cases differ: when no valid kappa / precision pair can be
    drawn, the error comes after at most 10,000 retries (R's recursion limit
    before), and a test set of one item now gets a kappa instead of an error.
    The bell-shaped generate_kp_list(distributionType = 1) still runs on
    RcppArmadillo, unchanged.

## rhoR 1.3.2
  * Built and published by cranqe (qe-libs.org/cran); CI now requests cranqe
    builds via the shared trigger-cranqe template

## rhoR 1.3.1
  * Eased thresholds on tests to pass all CRAN checks

## rhoR 1.3.0.3
  * Eased thresholds on tests to pass all CRAN checks

## rhoR 1.3.0.2

## rhoR 1.3.0.1

* Fixes for R-devel moving to R 4.0.0

## rhoR 1.3.0.0
* Rcpp implmentation using Contingency Tables for the basis of calculating rho

## rhoR 1.2.1.2
* `getHandSetIndices()` bug fixed for sampling positive indices
* `createSimulatedCodeSet()` floating point error fixed for exact matching kappa min and max values
* Implented full test suite for package

## rhoR 1.2.1.1

## rhoR 1.2.1.0

## rhoR 1.2.0.0

* `getHandSetIndices()` added as a new function. See `?rhoR::gethandSetIndices`
* `createSimulatedCodeSet()` bug fixed when generating the set to match criteria
* `rhoSet(),getTestSet()` now accept data frames, not just matrices