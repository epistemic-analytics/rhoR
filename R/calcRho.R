###
# This function calculates rho given the parameters of the initial set and the kappa of the observed handset and is
# called by rhoK()
#
# The computation is libqe-stats' qestats::rho_standard (src/rhor.cpp): the
# same draws and arithmetic as this function's former R implementation
# (generateKPs, getR, prset, getHandSet, getBootPvalue), so seeded results
# are unchanged. KPs, when given, is a list of genPKcombo() results.
###
calcRho = function(
  x, 
  OcSBaserate, 
  testSetLength, 
  testSetBaserateInflation = 0,
  OcSLength = 10000, 
  replicates = 800, 
  ScSKappaThreshold = 0.9, 
  ScSKappaMin = 0.40, 
  ScSPrecisionMin = 0.6, 
  ScSPrecisionMax = 1, 
  KPs = NULL
) {
  if (!is.null(KPs)) {
    KPs = cbind(
      vapply(KPs, function(KP) KP$precision, numeric(1)),
      vapply(KPs, function(KP) KP$kappa, numeric(1))
    )
  }

  return(calcRho_standard_c(
    x, OcSBaserate, testSetLength, testSetBaserateInflation, OcSLength, replicates,
    ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax, KPs
  ));
}
