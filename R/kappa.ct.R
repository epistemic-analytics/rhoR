###
#' @include kappa.set.R
#' @title Calculate kappa (contingency Table)
#' 
#' @description 
#' This function calculates kappa given a contingency table.
#' 
#' @export
#' @param ct A \code{\link{contingencyTable}}
#' 
#' @keywords kappa.ct
#' 
#' @examples
#' #Given a contingency Table
#' kappa.ct(contingencyTable)
#' 
#' @return 
#' Kappa as a double.
#'
###
kappa.ct = function(ct) {
  if(ncol(ct) != 2 || nrow(ct) != 2) stop("Incorrect number of dimensions: Contingency table must be 2x2.")
  for(i in 1:nrow(ct)) {
    for(j in 1:ncol(ct)) {
      if (ct[i,j]%%1 != 0) stop("Contingency table values must be positive integers.")
    }  
  }
  
  return(calcKappa(ct,isSet = FALSE,kappaThreshold = NULL));
}
