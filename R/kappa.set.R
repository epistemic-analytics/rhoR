###
#' @title Calculate kappa (Set)
#' 
#' @description 
#' This function calculates kappa given a set and can evaluate if it is greater than a threshold
#' 
#' @export
#' @param set A \code{\link{set}}
#' @param kappaThreshold Optional Threshold to compare against, as a double
#' @keywords kappa.set
#' 
#' @examples
#' # Given set and no threshold
#' kappa.set(set, isSet = TRUE, kappaThreshold = NULL)
#' 
#' # Given set and threshold
#' kappa.set(set, isSet = TRUE, kappaThreshold = 0.65)
#' 
#' @return 
#' If kappaThreshold is NULL: kappa as a double
#' If kappathreshold is not NULL: a list containing both the kappa, as a double, and a boolen indicating if it is
#' above the provided threshold.
###
kappa.set = function(set, kappaThreshold = NULL) {
  if(length(set[(set > 0 && set < 1) | set > 1]) != 0) {
    stop("Each set must only consist of zeros and ones")
  }
  return(calcKappa(set,isSet = TRUE,kappaThreshold));
}

