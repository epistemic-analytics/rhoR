###
#' @title Calculate kappa
#' 
#' @description 
#' This function calculates kappa given a set or contingency table and can evaluate if it is greater than a threshold
#' 
#' @export
#' @param set A \code{\link{set}} or \code{\link{contingencyTable}}
#' @param isSet An optional boolean indicating if `set` is, in fact, a set or it is a contingency table
#' @param kappaThreshold Optional Threshold to compare against, as a double
#' @keywords kappa
#' 
#' @examples
#' # Given contingency Table and no threshold
#' kappa(contingencyTable, isSet = FALSE, kappaThreshold = NULL)
#' 
#' # Given contingency with threshold
#' kappa(contingencyTable, isSet = FALSE, kappaThreshold = 0.65)
#' 
#' # Given set and no threshold
#' kappa(set, isSet = TRUE, kappaThreshold = NULL)
#' 
#' # Given set and threshold
#' kappa(set, isSet = TRUE, kappaThreshold = 0.65)
#' 
#' @return 
#' If kappaThreshold is NULL: kappa as a double
#' If kappathreshold is not NULL: a list containing both the kappa, as a double, and a boolen indicating if it is
#' above the provided threshold.
###
kappa = function(set, isSet = NULL, kappaThreshold = NULL) {
  if(is.null(isSet)) {
    isSet = (nrow(set) != ncol(set))
  }
  return(calcKappa(set,isSet,kappaThreshold));
}
