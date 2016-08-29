#' @include baserateCT.R
#' @title contingencyTable
#' 
#' @description A contingency Table is a 2x2 matrix that contains the counts of all combinations of positive and negative ratings made by two raters. If put in the form of a \code{\link{codeSet}}, the sum of all the combinations is equivilant to its length.
#' 
#' @format The contingency Table is an object of class matrix with two rows and two columns.  The ordering of the combination vector input to the matrix is as follows: c(PP, NP, PN, NN), where the first letter represents the first rater and the second letter represents the second rater.
#' 
#' @examples 
#' #A example contingencyTable
#' ct = matrix(c(3,2,1,34), nrow = 2, ncol = 2)
#' 
#' #This contingencyTable is included in the package under the variable name "contingencyTable".
#' 
"contingencyTable"