###
#' @include rhoCT.R
#' @title Calculate kappa (Set)
#' 
#' @description 
#' This function calculates Cohen's kappa for a given \code{\link{codeSet}}
#' 
#' @export
#' @param set A \code{\link{codeSet}}
#' 
#' @keywords kappaSet
#' 
#' @seealso \code{\link{kappaCT}}
#' 
#' @examples
#' # Given set
#' kappaSet(codeSet)
#' 
#' @return 
#' Kappa as a double.
#' 
###
kappaSet = function(set){
  if(length(set[(set > 0 && set < 1) | set > 1 | set < 0]) != 0) stop("Each set must only consist of zeros and ones.")
  
  return(calcKappa(set,isSet = TRUE,kappaThreshold = NULL));
}

