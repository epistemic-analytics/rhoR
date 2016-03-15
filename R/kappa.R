###
#' @title Calculate kappa
#' 
#' @description 
#' This function calculates kappa given a set
#' 
#' @export
#' @param set
#' @param isSet
#' @param kappaThreshold
#' @keywords kappa
#' 
#' @examples
#' # given contingency Table and no threshold
#' kappa(contingencyTable, isSet = FALSE, kappaThreshold = NULL)
#' 
#' # given contingency with threshold
#' kappa(contingencyTable, isSet = FALSE, kappaThreshold = 0.65)
#' 
#' # given set and no threshold
#' kappa(set, isSet = TRUE, kappaThreshold = NULL)
#' 
#' # given set and threshold
#' kappa(set, isSet = TRUE, kappaThreshold = 0.65)
#' 
#' @return 
#' If kappaThreshold is NULL, a double, else a list containing 
#' both the kappa, as a double, and a boolen indicating if it is
#' above the provided threshold.
###
kappa = function(set, isSet = TRUE, kappaThreshold = NULL) {
  return(calcKappa(set,isSet,kappaThreshold));
}
