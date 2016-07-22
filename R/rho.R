###
#' @include rhoR.R
#' @title Calculate rho
#' 
#' @description 
#' This function will calculate rho given the parameters of an observed test set, which it returns as a double.
#'
#' @export
#'
#' @param observedKappa The observed kappa value that will be tested with rho, as a double
#' @param SDSBaserate The \code{\link{baserate}} for a simulated data set
#' @param testSetLength The number of items evaluated by two raters to establish inter rater reliability
#' @param testSetBaserateInflation The percent, as a decimal, of the test set that is guaranteed to be a positive for the first rater
#' @param SDSLength The length of the simulated data sets used to calculate rho
#' @param replicates The number of simulated data sets to use in the null hypothesis distribution for rho
#' @param kappaThreshold The maximum kappa value used to generate simulated data sets in the null hypothesis distribution for rho
#' @param SDSKappaMin The minimum kappa value used to generate simulated data sets in the null hypothesis distribution for rho
#' @param precisionMin The minimum precision to be used for generation of simulated data sets in the null hypothesis distribution for rho
#' @param precisionMax The maximum precision to be used for generation of simulated data sets in the null hypothesis distribution for rho
#'
#' @note replicates as used in monte carlo studies.
#' 
#' @keywords rho
#' 
#' @examples
#' # Given only required parameters
#' rho(observedKappa = 0.88, SDSBaserate = 0.2, testSetLength = 80)
#' 
#' # Given required parameters plus testSetBaserateInflation and runs
#' rho(
#'  observedKappa = 0.88, SDSBaserate = 0.2, testSetLength = 80, 
#'  testSetBaserateInflation = 0.2, runs = 500
#')
#'
#' 
#' @return rho, as a double
###
rho = function(
  observedKappa, 
  SDSBaserate, 
  testSetLength, 
  testSetBaserateInflation = 0, 
  SDSLength = 10000, 
  runs = 800, 
  kappaThreshold = .65, 
  SDSKappaMin = .40, 
  precisionMin = 0.6, 
  precisionMax = 1
){
  if(observedKappa > 1 | observedKappa < 0)stop("Observed kappa must be between 0 and 1.")
  if(SDSBaserate < 0)stop("SDSBaserate must be positive.")
  if(testSetLength%%1 != 0) stop("testSetLength value must be a positive integer.")
  if(SDSLength%%1 != 0) stop("SDSLength value must be a positive integer.")
  if(runs%%1 != 0) stop("Runs value must be a positive integer.")
  if(kappaThreshold < 0) stop("kappaThreshold must be positive.")
  if(SDSKappaMin < 0) stop("CSKappaMin must be positive.")
  if(kappaThreshold < SDSKappaMin) stop("kappaThreshold must be greater than CSKappaMin.")
  if(precisionMin < 0 | precisionMin > 1) stop("precisionMin must be between 0 and 1.")
  if(precisionMax < 0 | precisionMax > 1) stop("precisionMax must be between 0 and 1.")
  if(precisionMax < precisionMin) stop("prcisionMax must be greater than precisionMin.")
  
  return(calcRho(observedKappa, SDSBaserate, testSetLength, testSetBaserateInflation, SDSLength, runs, kappaThreshold, SDSKappaMin, precisionMin, precisionMax))
  
}

