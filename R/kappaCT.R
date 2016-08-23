###
#' @include kappaSet.R
#' @title Calculate kappa (contingency Table)
#' 
#' @description 
#' This function calculates Cohen's kappa given a \code{\link{contingencyTable}}
#' 
#' @export
#' @param ct A \code{\link{contingencyTable}}
#' 
#' @keywords kappaCT
#' 
#' @seealso \code{\link{kappaSet}}
#' 
#' @examples
#' #Given a contingency Table
#' kappaCT(contingencyTable)
#' 
#' @return 
#' Kappa as a double.
###
kappaCT = function(ct){
  if(ncol(ct) != 2 || nrow(ct) != 2) stop("Incorrect number of dimensions: Contingency table must be 2x2.")
  for(i in 1:nrow(ct)) {
    for(j in 1:ncol(ct)) {
      if (ct[i,j]%%1 != 0) stop("Contingency table values must be positive integers.")
    }  
  }
  
  return(calcKappa(ct,isSet = FALSE,kappaThreshold = NULL));
}
