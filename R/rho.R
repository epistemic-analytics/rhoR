###
#' @title Calculate rho
#' 
#' @description 
#' This function will calculate rho given the parameters of the initial set to
#' determine if it is mathmatically possible to achieve a rho at a given 
#' hand set length and baserate.
#' 
#' @export
#' @param observedKappa Double
#' @param fullSetBaserate Double
#' @param handSetLength Integer
#' @param handSetBaserateInflation Double
#' @param fullSetLength Integer
#' @param runs Integer
#' @param kappaThreshold Double
#' @param kappaMin Double
#' @param precisionMin Double
#' @param precisionMax Double
#' @param handSetFunction Function
#' @keywords rho
#' 
#' @examples
#' rho.power(observedKappa = 0.88, fullSetBaserate = 0.2, handSetLength = 80)
#' rho.power(
#'  observedKappa = 0.88, fullSetBaserate = 0.2, handSetLength = 80, 
#'  handSetBaserateInflation = 0.2, runs = 500
#')
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