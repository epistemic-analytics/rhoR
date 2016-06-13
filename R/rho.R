###
#' @title Calculate rho
#' 
#' @description 
#' This function will calculate rho given the parameters of the initial set, which it returns as a double.
#' 
#' @export
#'
#' @param observedKappa The observed kappa value that will be tested with rho, as a double
#' @param codeSetBaserate The frequency of one's for the first rater in the code set, if NULL, assume the baserate of the test set is the same as the code set
#' @param testSetLength The length of the test set for the observedKappa
#' @param testSetBaserateInflation The percent, as a decimal, of the test set that is gauranteed to be a positive for the first rater
#' @param codeSetLength The length of the code set used to calculate rho
#' @param runs The number of code sets to use in the null hypothesis distribution for rho
#' @param kappaThreshold Threshold to compare against, as a double
#' @param nullHypMin The minimum kappa in the null hypothesis distribution for rho
#' @param precisionMin The minimum percision to be used for generation of sets in the null hypothesis distribution for rho
#' @param precisionMax The maximum percision to be used for generation of sets in the null hypothesis distribution for rho
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
  kappaThreshold = .65, 
  nullHypMin = .40, 
  precisionMin = 0.6, 
  precisionMax = 1
){
  return(calcRho(observedKappa, codeSetBaserate, testSetLength, testSetBaserateInflation, codeSetLength, runs, kappaThreshold, nullHypMin, precisionMin, precisionMax))
  
}

