###
#' @include baserate.R
#' @title Calculate Baserate (CT)
#' 
#' @description
#' This function will calculate the baserate of the first rater, second rater, and the average of both the raters.
#' 
#' @details
#' A baserate is the percentage, as a decimal, that a positive code appears in a \code{\link{codeSet}} for a given rater.  It is assumed that the first rater is more experienced and thus provides a better estimation of the actual baserate for a given code, so the first rater's baserate is often used as if it is the actual baserate.  If the raters are assumed to have the same experience level, the average baserate may give a better estimation.  If the second rater is more experienced, the second rater's baserate may give a better estimation.  Functions assume that the first rater is the more experienced rater and thus uses the first rater's baserate as the overall baserate estimation.
#' 
#' @export
#' 
#' @param CT The \code{\link{contingencyTable}} for which the baserate is calculated
#' 
#' @keywords baserate
#' 
#' @seealso \code{\link{baserate}}
#' 
#' @return A list of the format:\describe{
#' \item{firstBaserate}{The percentage that a positive code, or a 1, appears in the first rater}
#' \item{secondBaserate}{The percentage that a positive code, or a 1, appears in the second rater}
#' \item{averageBaserate}{The average percentage that a positive code, or a 1, appears in either of the two raters}
#' }
#' 
###

baserateCT = function(CT){
  firstBaserate = sum(CT[1,])/sum(CT)
  secondBaserate = sum(CT[,1])/sum(CT)
  averageBaserate = mean(c(firstBaserate,secondBaserate))
  return(list(firstBaserate = firstBaserate, secondBaserate = secondBaserate, averageBaserate = averageBaserate))
}