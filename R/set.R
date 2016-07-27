#' @include rho.minTSLength.R
#' @title set sample
#' 
#' @description A sample test set \cr
#' gold = c(1,1,1,1,rep(0,36))
#' silver = c(1,1,1,0,1,1,rep(0,34)) \cr
#' set = cbind(gold,silver)
#' 
#' @format An object of class data.frame with 40 rows and 2 columns.
#' 
#' @details A set can be any length of vectors that are bound using \code{\link{cbind}}.
#' The length of each of these vectors becomes the set length. The first column of the set is defaulted as the first rater.
"set"