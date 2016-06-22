###
#' @include rho.ct.R
#' @title Calculate kappa (Set)
#' 
#' @description 
#' This function calculates kappa given a set and can evaluate if it is greater than a threshold
#' 
#' @export
#' @param set A \code{\link{set}}
#' @param CSKappaMax Optional. The maximum kappa value used to generate code sets in the null hypothesis distribution for rho
#' @keywords kappa.set
#' 
#' @examples
#' # Given set and no CSKappaMax
#' kappa.set(set, CSKappaMax = NULL)
#' 
#' # Given set and CSKappaMax
#' kappa.set(set, CSKappaMax = 0.65)
#' 
#' @return 
#' If CSKappaMax is NULL: kappa as a double.
#' If CSKappaMax is not NULL: a list containing both the kappa, as a double, and a boolen indicating if it is
#' above the provided threshold.
###
kappa.set = function(set, CSKappaMax = NULL) {
  if(length(set[(set > 0 && set < 1) | set > 1 | set < 0]) != 0) stop("Each set must only consist of zeros and ones.")
  if(!is.null(CSKappaMax)) {
    if(CSKappaMax < 0 | CSKappaMax > 1) stop("CSKappaMax must be between 0 and 1.")
  }
  
  return(calcKappa(set,isSet = TRUE,CSKappaMax));
}

