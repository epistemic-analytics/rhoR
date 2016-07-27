###
#' @include rho_ct.R
#' @title Calculate kappa (Set)
#' 
#' @description 
#' This function calculates kappa given a set.
#' 
#' @export kappa_set
#' @param set A \code{\link{set}}
#' 
#' @keywords kappa_set
#' 
#' @examples
#' # Given set
#' kappa_set(set)
#' 
#' @return 
#' Kappa as a double.
#' 
###
kappa_set = function(set){
  if(length(set[(set > 0 && set < 1) | set > 1 | set < 0]) != 0) stop("Each set must only consist of zeros and ones.")
  
  return(calcKappa(set,isSet = TRUE,kappaThreshold = NULL));
}

