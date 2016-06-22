###
#' @include kappa.ct.R
#' @title Rho Min testSetLength
#' 
#' @description 
#' This function calculates the minimum testSetLength where it is possible to get a rho less than alpha, which it returns as an integer.
#' 
#' @export
#' @param baserate A double indicating the frequency a code occurs for the first rater in the code set
#' @param alpha A double indicating the threshold of significance for rho (similar to an alpha level for a p value), defaulted to 0.05
#' @param inc An integer indicating by how much the testSetLength should increase each iteration
#' @param printInc A boolean indicating whether to print out each increment value with it's corresponding significance for rho
#' @param ... Any additional parameters passed into \code{\link{rho}}
#' @keywords rho.minTSLength
#' 
#' @examples
#' #Add testSetBaserate as an additional parameter
#' rho.minTSLength(0.2, testSetBaserate = 0.2)
#' 
#' #Add testSetBaserate as well as changing inc and selecting printInc
#' rho.minTSLength(0.2, inc = 5, printInc = TRUE, testSetBaserate = 0.2)
#' 
#' @return 
#' An integer giving the minimum length of testSet, as a denomination of inc, that would give a
#' value where rho becomes mathematically possible
###
rho.minTSLength = function(baserate, alpha = 0.05, inc = 10, printInc = FALSE, ...){
  rhoVal = 1
  testSetLength = 10
  if(inc%%1 != 0 | inc <= 0) stop("Inc value must be a positive integer.")
  if(alpha > 1 | alpha < 0) stop("Alpha must be between 0 and 1.")
  
  
  while(rhoVal > alpha){
    testSetLength = testSetLength + inc
    rhoVal = rho(1, baserate, testSetLength, ...)
    if(printInc) {
      cat(testSetLength, rhoVal,"\n")
    }
  }
  return(testSetLength)
  
}