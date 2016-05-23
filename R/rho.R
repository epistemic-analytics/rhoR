###
#' @title Calculate rho
#' 
#' @description 
#' This function will calculate rho given the parameters of the initial set, which it returns as a double.
#' 
#' @export
<<<<<<< HEAD
#' @param observedKappa The kappa value that is used to determine rho, as a double
#' @param fullSetBaserate The frequency of one's for the first rater in the full set, if NULL, assume the baserate of the hand set is the same as the full set
#' @param handSetLength The length of the hand set chosen
#' @param handSetBaserateInflation The percent, as a decimal, of the hand set that is gauranteed to be a positive for the first rater
#' @param fullSetLength The length of the full set that the hand set was taken from
#' @param runs The number of full sets to use in the null hypothesis distribution for rho
#' @param kappaThreshold Threshold to compare against, as a double
#' @param kappaMin The minimum kappa in the null hypothesis distribution for rho
#' @param precisionMin The minimum percision to be used for generation of sets in null hypothesis distribution for rho
#' @param precisionMax The maximum percision to be used for generation of sets in null hypothesis distribution for rho
=======
#' @param observedKappa - The kappa value that is used to determine rho, as a double
#' @param fullSetBaserate - The baserate that the parent set is at; if NULL, assume the baserate of the child set is the same as the parent set
#' @param handSetLength - The length of the handset chosen
#' @param handSetBaserateInflation - The percent, as a decimal, of the handset that is gauranteed to be a positive
#' @param fullSetLength - The length of the fullset that the handset was taken from
#' @param runs - The number of sets to use in the distribution
#' @param kappaThreshold - Threshold to compare against, as a double
#' @param kappaMin - The minimum kappa in the distribution
#' @param precisionMin - The minimum percision to be used for generation of sets in rho distribution
#' @param precisionMax - The maximum percision to be used for generation of sets in rho distribution
>>>>>>> f0e0042a0786e87f7cf5cb337e256a254ddbd3bc
#' @keywords rho
#' 
#' @examples
#' # Given only required parameters
#' rho(observedKappa = 0.88, fullSetBaserate = 0.2, handSetLength = 80)
#' 
#' # Given required parameters plus handSetBaserateInflation and runs
#' rho(
#'  observedKappa = 0.88, fullSetBaserate = 0.2, handSetLength = 80, 
#'  handSetBaserateInflation = 0.2, runs = 500
#')
#'
#' 
#' @return The calcuated rho, as a double
###
rho = function(
  observedKappa, 
  fullSetBaserate, 
  handSetLength, 
  handSetBaserateInflation = 0, 
  fullSetLength = 10000, 
  runs = 800, 
  kappaThreshold = .65, 
  kappaMin = .40, 
  precisionMin = 0.6, 
  precisionMax = 1
){
  return(calcRho(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation, fullSetLength, runs, kappaThreshold, kappaMin, precisionMin, precisionMax))
}