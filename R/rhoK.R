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
){
  err = F
#   if(observedKappa > 1 | observedKappa < 0){
# 		warning("Observed kappa must be between 0 and 1.")
#     err=T
#   }
#   if(OcSBaserate < 0){
# 		warning("OcSBaserate must be positive.")
# 		err=T
# 	}
#   if(testSetLength%%1 != 0) {
# 		warning("testSetLength value must be a positive integer.")
# 		err=T
# 	}
#   if(OcSLength%%1 != 0) {
# 		warning("OcSLength value must be a positive integer.")
# 		err=T
# 	}
#   if(replicates%%1 != 0) {
# 		warning("Replicates value must be a positive integer.")
# 		err=T
# 	}
#   if(ScSKappaThreshold < 0) {
# 		warning("ScSKappaThreshold must be positive.")
# 		err=T
# 	}
#   if(ScSKappaMin < 0) {
# 		warning("ScSKappaMin must be positive.")
# 		err=T
# 	}
#   if(ScSKappaThreshold < ScSKappaMin) {
# 		warning("ScSKappaThreshold must be greater than ScSKappaMin.")
# 		err=T
# 	}
#   if(ScSPrecisionMin < 0 | ScSPrecisionMin > 1) {
# 		warning("ScSPrecisionMin must be between 0 and 1.")
# 		err=T
# 	}
#   if(ScSPrecisionMax < 0 | ScSPrecisionMax > 1) {
# 		warning("ScSPrecisionMax must be between 0 and 1.")
# 		err=T
# 	}
#   if(ScSPrecisionMax < ScSPrecisionMin) {
# 		warning("ScSPrecisionMax must be greater than ScSPrecisionMin.")
# 		err=T
# 	}
  
  if(err == F) {
    return(calcRho2(observedKappa, OcSBaserate, testSetLength, testSetBaserateInflation, OcSLength, replicates, ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax))
  } else {
    return(list(kappa = NA, rho = NA))
  }
  
}

