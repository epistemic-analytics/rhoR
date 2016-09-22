###
<<<<<<< HEAD
#' @include baserate.R
#' @title Calculate Baserate (Set)
#' 
#' @description
#' This function calculates the baserate of the first rater, second rater, and the average of both the raters. Called by \code{\link{baserate}}.
=======
#' @include rhoMinTSLength.R
#' @title Calculate Baserate (Set)
#' 
#' @description
#' This function will calculate the baserate of the first rater, second rater, and the average of both the raters.
#' 
#' @details
#' A baserate is the percentage, as a decimal, that a positive code appears in a \code{\link{codeSet}} for a given rater.  It is assumed that the first rater is more experienced and thus provides a better estimation of the actual baserate for a given code, so the first rater's baserate is often used as if it is the actual baserate.  If the raters are assumed to have the same experience level, the average baserate may give a better estimation.  If the second rater is more experienced, the second rater's baserate may give a better estimation.  Functions assume that the first rater is the more experienced rater and thus uses the first rater's baserate as the overall baserate estimation.
>>>>>>> 2ffddd8ce732e2913c1699a2e3dba55ae060bc9e
#' 
#' @export
#' 
#' @param set The \code{\link{codeSet}} for which the baserate is calculated
#' 
<<<<<<< HEAD
#' @keywords baserateSet
#' 
#' @seealso \code{\link{baserate}} and \code{\link{baserateCT}}
=======
#' @keywords baserate
#' 
#' @seealso \code{\link{baserateCT}}
>>>>>>> 2ffddd8ce732e2913c1699a2e3dba55ae060bc9e
#' 
#' @return A list of the format:\describe{
#' \item{firstBaserate}{The percentage that a positive code, or a 1, appears in the first rater}
#' \item{secondBaserate}{The percentage that a positive code, or a 1, appears in the second rater}
#' \item{averageBaserate}{The average percentage that a positive code, or a 1, appears in either of the two raters}
#' }
#' 
###

baserateSet = function(set){
  firstBaserate = length(which(set[,1] == 1))/length(set)
  secondBaserate = length(which(set[,2] == 1))/length(set)
  averageBaserate = mean(c(firstBaserate,secondBaserate))
  return(list(firstBaserate = firstBaserate, secondBaserate = secondBaserate, averageBaserate = averageBaserate))
}