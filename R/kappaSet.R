###
#' @include kappa.R
#' @title Calculate kappa (Set)
#' 
#' @description 
#' This function calculates Cohen's kappa for a given \code{\link{codeSet}}. Called by \code{\link{kappa}}.
#' 
#' @export
#' @param set A \code{\link{codeSet}}
#' 
#' @keywords kapppaSet
#' 
#' @seealso \code{\link{kappa}} and \code{\link{kappaCT}}
#' 
#' @return 
#' The kappa of the \code{\link{codeSet}}
#' 
###
kappaSet = function(set){
  if(length(set[(set > 0 && set < 1) | set > 1 | set < 0]) != 0) {
    stop("Each set must only consist of zeros and ones.")
  }
  
  return(calcKappa(set,isSet = TRUE,kappaThreshold = NULL));
}

