###
#' @include rhoCT.R
#' @title Calculate kappa (Set)
#' 
#' @description 
#' This function calculates kappa given a set.
#' 
#' @export
#' @param set A \code{\link{set}}
#' 
#' @keywords kappaSet
#' 
#' @examples
#' # Given set
#' kappaSet(set)
#' 
#' @return 
#' Kappa as a double.
#' 
###
kappaSet = function(set){
  if(length(set[(set > 0 && set < 1) | set > 1 | set < 0]) != 0) stop("Each set must only consist of zeros and ones.")
  
  return(calcKappa(set,isSet = TRUE,kappaThreshold = NULL));
}

