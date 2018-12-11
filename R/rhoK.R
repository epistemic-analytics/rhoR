###
#' @include rho.R
#' @title Rho (kappa)
#' 
#' @description 
#' This function calculates rho for an observed kappa value with associated set parameters (testSetLength and OcSBaserate). Called by \code{\link{rho}}. 
#' 
#' @export
#'
#' @param observedKappa The observed kappa value used to calculate rho
#' @param OcSBaserate The \code{\link{baserate}} of the observed \code{\link{codeSet}} (defaults to \code{\link{baserate}} of \code{\link[=getTestSet]{testSet}} or \code{\link{contingencyTable}})
#' @param testSetLength The length of the \code{\link[=getTestSet]{testSet}} (ignored unless \emph{data} is an observed kappa value)
#' @param testSetBaserateInflation The minimum \code{\link{baserate}} from the sampling procedure
#' @param OcSLength The length of the observed \code{\link{codeSet}}
#' @param replicates The number of simulated \code{\link[=getTestSet]{codeSets}} to use in the null hypothesis distribution for rho; similar to replicates in a Monte Carlo study
#' @param ScSKappaThreshold The maximum kappa value used to generate simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSKappaMin The minimum kappa value used to generate simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSPrecisionMin The minimum precision to be used for generation of simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSPrecisionMax The maximum precision to be used for generation of simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' 
#' 
#' @keywords rhoK
#' 
#' @seealso \code{\link{rho}} and \code{\link{rhoSet}} and \code{\link{rhoCT}}
#' 
#' @return rho for the given parameters
###

rhoK = function(
  observedKappa, 
  OcSBaserate, 
  testSetLength, 
  testSetBaserateInflation = 0, 
  OcSLength = 10000, 
  replicates = 800, 
  ScSKappaThreshold = .65, 
  ScSKappaMin = .40, 
  ScSPrecisionMin = 0.6, 
  ScSPrecisionMax = 1
) {
  return(calcRho(observedKappa, OcSBaserate, testSetLength, testSetBaserateInflation, OcSLength, replicates, ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax))
}

rhoK_useC = function(
  observedKappa,
  OcSBaserate,
  testSetLength,
  testSetBaserateInflation = 0,
  OcSLength = 10000,
  replicates = 800,
  ScSKappaThreshold = .65,
  ScSKappaMin = .40,
  ScSPrecisionMin = 0.6,
  ScSPrecisionMax = 1
) {
  calcRho_c(observedKappa, OcSBaserate, testSetLength, testSetBaserateInflation, OcSLength, replicates, ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax)
}

