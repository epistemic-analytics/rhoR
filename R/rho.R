###
#' @include rhoR.R
#' @title Rho
#' 
#' @description 
#' This function will calculate rho given the parameters of an observed \code{\link{testSet}}
#'
#' @export
#'
#' @param observedKappa The observed kappa value that will be tested with rho
#' @param OcSBaserate The \code{\link{baserate}} of the observed \code{\link{codeSet}}
#' @param testSetLength The length of the \code{\link{testSet}} the \emph{observedKappa} was calculated from
#' @param testSetBaserateInflation The minimum \code{\link{baserate}} gaurunteed for the \code{\link{testSet}}
#' @param OcSLength The length of the observed \code{\link{codeSet}}
#' @param replicates The number of simulated \code{\link{codeSet}}s to use in the null hypothesis distribution for rho; similar to replicates in a Monte Carlo study
#' @param ScSKappaThreshold The maximum kappa value used to generate simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSKappaMin The minimum kappa value used to generate simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSPrecisionMin The minimum precision to be used for generation of simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSPrecisionMax The maximum precision to be used for generation of simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' 
#' @keywords rho
#' 
#' @examples
#' # Given only required parameters
#' rho(observedKappa = 0.88, OcSBaserate = 0.2, testSetLength = 80)
#' 
#' # Given required parameters plus testSetBaserateInflation and runs
#' rho(
#'  observedKappa = 0.88, OcSBaserate = 0.2, testSetLength = 80, 
#'  testSetBaserateInflation = 0.2, replicates = 500
#')
#'
#' 
#' @return rho for the given parameters
###
rho = function(
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
  if(observedKappa > 1 | observedKappa < 0)stop("Observed kappa must be between 0 and 1.")
  if(OcSBaserate < 0)stop("OcSBaserate must be positive.")
  if(testSetLength%%1 != 0) stop("testSetLength value must be a positive integer.")
  if(OcSLength%%1 != 0) stop("OcSLength value must be a positive integer.")
  if(replicates%%1 != 0) stop("Replicates value must be a positive integer.")
  if(ScSKappaThreshold < 0) stop("ScSKappaThreshold must be positive.")
  if(ScSKappaMin < 0) stop("ScSKappaMin must be positive.")
  if(ScSKappaThreshold < ScSKappaMin) stop("ScSKappaThreshold must be greater than ScSKappaMin.")
  if(ScSPrecisionMin < 0 | ScSPrecisionMin > 1) stop("ScSPrecisionMin must be between 0 and 1.")
  if(ScSPrecisionMax < 0 | ScSPrecisionMax > 1) stop("ScSPrecisionMax must be between 0 and 1.")
  if(ScSPrecisionMax < ScSPrecisionMin) stop("ScSPrecisionMax must be greater than ScSPrecisionMin.")
  
  return(calcRho(observedKappa, OcSBaserate, testSetLength, testSetBaserateInflation, OcSLength, replicates, ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax))
  
}

