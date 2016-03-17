#' @title Calculate rho (and kappa)
#' 
#' @description 
#' This function will calculate rho given the parameters of the initial set,
#' and return rho and the calculated kappa.
#' 
#' @export
#' @param set set or contigency table
#' @param isSet Boolean
#' @param fullSetBaserate Double
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
#' # Given contingency getting baserate from set
#' rho(contingencyTable, isSet = FALSE)
#' 
#' # Given set giving baserate
#' rho(set, isSet = TRUE, fullSetBaserate = 0.2)
#' 
#' @return A list containing the calculated rho and kappa values
###
rho = function(
  set,
  isSet = TRUE,
  fullSetBaserate = NULL,
  handSetBaserateInflation = 0,
  fullSetLength = 10000,
  runs = 800,
  kappaThreshold = .65,
  kappaMin = .40,
  precisionMin = 0.6,
  precisionMax = 1,
  handSetFunction = getHandSet
){
  kappa = kappa(set, isSet, NULL);

  if(is.null(fullSetBaserate)) {
    fullSetBaserate = length(which(set[,1] == 1))/length(set);
    
    if(isSet != TRUE) {
      fullSetBaserate = (sum(contingencyTable[1,]))/(sum(contingencyTable));
    }
  }

  return(list(
    rho = rho2(
      kappa, 
      fullSetBaserate,
      length(set),
      handSetBaserateInflation,
      fullSetLength,
      runs,
      kappaThreshold, kappaMin, precisionMin, precisionMax, handSetFunction
    ), 
    kappa = kappa
  ));
}