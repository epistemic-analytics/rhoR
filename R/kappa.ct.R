###
#' @include kappa.set.R
#' @title Calculate kappa (contingency Table)
#' 
#' @description 
#' This function calculates kappa given a contingency table and can evaluate if it is greater than a threshold
#' 
#' @export
#' @param ct A \code{\link{contingencyTable}}
#' @param CSKappaMax Optional. The maximum kappa value used to generate code sets in the null hypothesis distribution for rho
#' @keywords kappa.ct
#' 
#' @examples
#' # Given contingency Table and no CSKappaMax
#' kappa.ct(contingencyTable, CSKappaMax = NULL)
#' 
#' # Given contingency with threshold
#' kappa.ct(contingencyTable, CSKappaMax = 0.65)
#' 
#' @return 
#' If CSKappaMax is NULL: kappa as a double.
#' If CSKappaMax is not NULL: a list containing both the kappa, as a double, and a boolen indicating if it is
#' above the provided threshold.
###
kappa.ct = function(ct, CSKappaMax = NULL) {
  if(ncol(ct) != 2 || nrow(ct) != 2) {
    stop("Incorrect number of dimensions: Contingency table must be 2x2")
  }
  return(calcKappa(ct,isSet  = FALSE,CSKappaMax));
}
