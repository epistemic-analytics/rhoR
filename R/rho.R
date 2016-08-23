###
#' @include rhoR.R
#' @title Rho (Set)
#' 
#' @description 
#' This function will calculate rho given the parameters of an observed \code{\link{testSet}}, which it returns as a double.
#'
#' @export
#'
#' @param observedKappa The observed kappa value that will be tested with rho, as a double
#' @param ScSBaserate The \code{\link{baserate}} for a simulated \code{\link{codeSet}}
#' @param testSetLength The number of items evaluated by two raters to establish inter rater reliability
#' @param testSetBaserateInflation The percent, as a decimal, of the \code{\link{testSet}} that is guaranteed to be a positive for the first rater
#' @param ScSLength The length of the simulated \code{\link{codeSet}}s used to calculate rho
#' @param replicates The number of simulated \code{\link{codeSet}}s to use in the null hypothesis distribution for rho; similar to replicates in a Monte Carlo study
#' @param kappaThreshold The maximum kappa value used to generate simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSKappaMin The minimum kappa value used to generate simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSPrecisionMin The minimum precision to be used for generation of simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSPrecisionMax The maximum precision to be used for generation of simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' 
#' @keywords rho
#' 
#' @examples
#' # Given only required parameters
#' rho(observedKappa = 0.88, ScSBaserate = 0.2, testSetLength = 80)
#' 
#' # Given required parameters plus testSetBaserateInflation and runs
#' rho(
#'  observedKappa = 0.88, ScSBaserate = 0.2, testSetLength = 80, 
#'  testSetBaserateInflation = 0.2, replicates = 500
#')
#'
#' 
#' @return rho, as a double
###
rho = function(
  observedKappa, 
  ScSBaserate, 
  testSetLength, 
  testSetBaserateInflation = 0, 
  ScSLength = 10000, 
  replicates = 800, 
  kappaThreshold = .65, 
  ScSKappaMin = .40, 
  ScSPrecisionMin = 0.6, 
  ScSPrecisionMax = 1
){
  if(observedKappa > 1 | observedKappa < 0)stop("Observed kappa must be between 0 and 1.")
  if(ScSBaserate < 0)stop("ScSBaserate must be positive.")
  if(testSetLength%%1 != 0) stop("testSetLength value must be a positive integer.")
  if(ScSLength%%1 != 0) stop("ScSLength value must be a positive integer.")
  if(replicates%%1 != 0) stop("Replicates value must be a positive integer.")
  if(kappaThreshold < 0) stop("kappaThreshold must be positive.")
  if(ScSKappaMin < 0) stop("CSKappaMin must be positive.")
  if(kappaThreshold < ScSKappaMin) stop("kappaThreshold must be greater than CSKappaMin.")
  if(ScSPrecisionMin < 0 | ScSPrecisionMin > 1) stop("ScSPrecisionMin must be between 0 and 1.")
  if(ScSPrecisionMax < 0 | ScSPrecisionMax > 1) stop("ScSPrecisionMax must be between 0 and 1.")
  if(ScSPrecisionMax < ScSPrecisionMin) stop("ScSPrecisionMax must be greater than ScSPrecisionMin.")
  
  return(calcRho(observedKappa, ScSBaserate, testSetLength, testSetBaserateInflation, ScSLength, replicates, kappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax))
  
}

