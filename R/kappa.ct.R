###
#' @title Calculate kappa (contingency Table)
#' 
#' @description 
#' This function calculates kappa given a contingency table and can evaluate if it is greater than a threshold
#' 
#' @export
#' @param ct A \code{\link{contingencyTable}}
#' @param kappaThreshold Optional Threshold to compare against, as a double
#' @keywords kappa.ct
#' 
#' @examples
#' # Given contingency Table and no threshold
#' kappa.ct(contingencyTable, isSet = FALSE, kappaThreshold = NULL)
#' 
#' # Given contingency with threshold
#' kappa.ct(contingencyTable, isSet = FALSE, kappaThreshold = 0.65)
#' 
#' @return 
#' If kappaThreshold is NULL: kappa as a double
#' If kappathreshold is not NULL: a list containing both the kappa, as a double, and a boolen indicating if it is
#' above the provided threshold.
###
kappa.ct = function(ct, kappaThreshold = NULL) {
  if(ncol(ct) != 2 || nrow(ct) != 2) {
    stop("Incorrect number of dimensions: Contingency table must be 2x2")
  }
  return(calcKappa(ct,isSet  = FALSE,kappaThreshold));
}
