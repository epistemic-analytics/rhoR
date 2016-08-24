###
#' @include createScS.R
#' @title Get Test Set
#' 
#' @description
#' This function gets a \emph{testSet} from a larger \code{\link{codeSet}} given certain sampling parameters.
#' 
#' @details
#' A \emph{testSet} is a \code{\link{codeSet}} that is a subset of a larger \code{\link{codeSet}} with a given set of properties.  The process for taking a \emph{testSet} is that the \emph{testSetBaserateInflation} guarantees a certain percentage of the \emph{testSet} to be positive and the rest of the set is randomly sampled from the original \code{\link{codeSet}}
#' 
#' @export
#' 
#' @param set The \code{\link{codeSet}} for which the \emph{testSet} is taken from
#' @param testSetLength The length of the \emph{testSet} to be taken
#' @param testSetBaserateInflation The minimum garunteed \code{\link{baserate}} of the \emph{testSet}
#' 
#' @keywords testSet
#' 
#' @return A \code{\link{codeSet}} with the properties specified
#' 
###

testSet = function(set, testSetLength, testSetBaserateInflation){
  if(length(set) < testSetLength){stop("testSetLength must be less than the length of set")}
  if(testSetBaserateInflation < 0){stop("testSetBaserateInfation must be positive")}
  return(getHandSet(set = set, handSetLength = testSetLength, handSetBaserate = testSetBaserateInflation, returnSet = T))
}