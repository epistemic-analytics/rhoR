#' @include contingencyTable.R
#' @title Set
#' 
#' @description A set can be any length of vectors that are bound using \code{\link{cbind}}.
#' The length of each of these vectors becomes the set length. The first column of the set is defaulted as the first rater.
#' 
#' @format The set is an object of class data.frame with n rows and two columns. 
#' 
#' @examples An example test set \cr
#' gold = c(1,1,1,1,rep(0,36))
#' silver = c(1,1,1,0,1,1,rep(0,34)) \cr
#' exampleSet = cbind(gold,silver) \cr
#' This set is included in the package under the variable name "exampleSet".
"set"
