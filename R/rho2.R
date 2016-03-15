###
#' @title Calculate rho
#' 
#' @description 
#' This function will calculate rho given the parameters of the initial set
#' 
#' @export
#' @param observedKappa
#' @param fullSetBaserate
#' @param handSetLength
#' @param handSetBaserateInflation
#' @param fullSetLength
#' @param runs
#' @param kappaThreshold
#' @param kappaMin
#' @param precisionMin
#' @param precisionMax
#' @param handSetFunction
#' @keywords rho
#' 
#' @examples
#' rho2(observedKappa = 0.88, fullSetBaserate = 0.2, handSetLength = 80)
#' rho2(observedKappa = 0.88, fullSetBaserate = 0.2, handSetLength = 80, handSetBaserateInflation = 0.2, runs = 500)
#' 
#' @return The calcuated rho, as a double
###
rho2 = function(
  observedKappa, 
  fullSetBaserate, 
  handSetLength, 
  handSetBaserateInflation = 0, 
  fullSetLength = 10000, 
  runs = 800, 
  kappaThreshold = .65, 
  kappaMin = .40, 
  precisionMin = 0.6, 
  precisionMax = 1, 
  handSetFunction = getHandSet
){
  return(calcRho(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation, fullSetLength, runs, kappaThreshold, kappaMin, precisionMin, precisionMax, handSetFunction))
}