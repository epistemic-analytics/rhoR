###
#' Calculate kappa
#' 
#' @export
#' @param set
#' @param isSet
#' @param kappaThreshold
#' @keywords kappa
#' @return a value
###
kappa = function(set, isSet = TRUE, kappaThreshold = NULL) {
  return(calcKappa(set,isSet,kappaThreshold));
}
