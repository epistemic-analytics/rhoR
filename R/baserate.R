###
#' @include rhoMinTSLength.R
#' @title Calculate Baserate
#' 
#' @description
#' This function will calculate the baserate of the first and second rater as well as the average of both the raters.
#' 
#' @details
#' A baserate is the percentage, as a decimal, that a positive code appears in a set for a given rater.  It is assumed that the first rater is more experienced and thus provides a better estimation of the actual baserate for a given code.  If the raters are assumed to have the same experience level, the average baserate may give a more accurate approximation.  If the second rater is more experienced, the second rater's baserate would be assumed to have the better estimation.  Functions assume that the first rater is the more experienced rater and thus uses the first rater's baserate as the overall baserate estimation.
#' 
#' @export
#' 
#' @param set The \code{\link{set}} to calculate the baserate of
#' 
#' @keywords baserate
#' 
#' @return A list of baserates; first being the first rater's, second being the second rater's, and the third being the average of the two raters.
#' 
###

baserate = function(set){
  firstBaserate = length(which(set[,1] == 1))/length(set)
  secondBaserate = length(which(set[,2] == 1))/length(set)
  averageBaserate = mean(c(firstBaserate,secondBaserate))
  return(list(firstBaserate = firstBaserate, secondBaserate = secondBaserate, averageBaserate = averageBaserate))
}