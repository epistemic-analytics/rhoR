## rhoR 1.3.3 (development version)
  * Regression tests pin the 1.3.2 outputs of every rho building block
    (tests/testthat/test-regression-pins.R), ahead of moving the core to
    the qestats C++ headers

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