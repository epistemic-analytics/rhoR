###
#' rho 2
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
#' @return a value
###
rho2 = function(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation=0, fullSetLength = 10000, runs=800, kappaThreshold=.65, kappaMin=.40, precisionMin = 0.6, precisionMax = 1, handSetFunction = getHandSet){
  return(calcRho(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation, fullSetLength, runs, kappaThreshold, kappaMin, precisionMin, precisionMax, handSetFunction))
}
