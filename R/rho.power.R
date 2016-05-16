###
#' @title Rho Power
#' 
#' @description 
#' This function calculates the minimum handSetLength required to get a valid rho
#' 
#' @export
#' @param baserate - A double indicating the frequency a code occurs for the first rater in the overall set
#' @param alpha - A double indicating the threshold of significance for rho (similar to an alpha level for a p value)
#' @param inc - An integer indicating by how much the handSetBaserate should increment each iteration
#' @param ... - Any additional parameters passed into \code{\link{rho}}
#' @keywords rho.power
#' 
#' @examples
#' #Add \code{handSetBaserate} as an additional parameter
#' rho.power(0.2, handSetBaserate = 0.2)
#' 
#' #Add \code{handSetBaserate} as well as changing \code{inc}
#' rho.power(0.2, inc = 5, handSetBaserate = 0.2)
#' 
#' @return 
#' An integer giving the minimum length of handSet, as a denomination of \code{inc}, that would give a
#' value where rho becomes mathematically possible
###
rho.power = function(baserate, alpha = 0.05, inc = 10, ...){
  rhoVal = 1
  handSetLength = 10
  while(rhoVal > alpha){
    handSetLength = handSetLength + inc
    rhoVal = rho(1, baserate, handSetLength, ...)
    cat(handSetLength, rhoVal,"\n")
  }
  return(handSetLength)
}