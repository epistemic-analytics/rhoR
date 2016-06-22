###
#' @include rhoR.R
#' @title Calculate rho
#' 
#' @description 
#' This function will calculate rho given the parameters of the observed test set, which it returns as a double.
#' 
#' @export
#'
#' @param observedKappa The observed kappa value that will be tested with rho, as a double
#' @param codeSetBaserate The frequency of ones for the first rater in the code set, if NULL, assume the baserate of the test set is the same as the code set
#' @param testSetLength The number of items evaluated by two raters to establish inter rater reliability
#' @param testSetBaserateInflation The percent, as a decimal, of the test set that is guaranteed to be a positive for the first rater
#' @param codeSetLength The length of the code sets used to calculate rho
#' @param runs The number of code sets to use in the null hypothesis distribution for rho
#' @param CSKappaMax The maximum kappa value used to generate code sets in the null hypothesis distribution for rho
#' @param CSKappaMin The minimum kappa value used to generate code sets in the null hypothesis distribution for rho
#' @param precisionMin The minimum precision to be used for generation of code sets in the null hypothesis distribution for rho
#' @param precisionMax The maximum precision to be used for generation of code sets in the null hypothesis distribution for rho
#'
#' @keywords rho
#' 
#' @examples
#' # Given only required parameters
#' rho(observedKappa = 0.88, codeSetBaserate = 0.2, testSetLength = 80)
#' 
#' # Given required parameters plus testSetBaserateInflation and runs
#' rho(
#'  observedKappa = 0.88, codeSetBaserate = 0.2, testSetLength = 80, 
#'  testSetBaserateInflation = 0.2, runs = 500
#')
#'
#' 
#' @return The calcuated rho, as a double
###
rho = function(
  observedKappa, 
  codeSetBaserate, 
  testSetLength, 
  testSetBaserateInflation = 0, 
  codeSetLength = 10000, 
  runs = 800, 
  CSKappaMax = .65, 
  CSKappaMin = .40, 
  precisionMin = 0.6, 
  precisionMax = 1
){
  if(observedKappa > 1 | observedKappa < 0)stop("Observed kappa must be between 0 and 1.")
  if(codeSetBaserate < 0)stop("codeSetBaserate must be positive.")
  if(testSetLength%%1 != 0) stop("testSetLength value must be a positive integer.")
  if(codeSetLength%%1 != 0) stop("codeSetLength value must be a positive integer.")
  if(runs%%1 != 0) stop("Runs value must be a positive integer.")
  if(CSKappaMax < 0) stop("CSKappaMax must be positive.")
  if(CSKappaMin < 0) stop("CSKappaMin must be positive.")
  if(CSKappaMax < CSKappaMin) stop("CSKappaMax must be greater than CSKappaMin.")
  if(precisionMin < 0 | precisionMin > 1) stop("precisionMin must be between 0 and 1.")
  if(precisionMax < 0 | precisionMax > 1) stop("precisionMax must be between 0 and 1.")
  if(precisionMax < precisionMin) stop("prcisionMax must be greater than precisionMin.")
  
  return(calcRho(observedKappa, codeSetBaserate, testSetLength, testSetBaserateInflation, codeSetLength, runs, CSKappaMax, CSKappaMin, precisionMin, precisionMax))
  
}

