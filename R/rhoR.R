#' rhoR: A package for computating rho.
#' 
#' rho is used to test the generalization of inter rater reliability (IRR)
#' statistics, in this case Cohen's Kappa.
#' 
#' Rho starts with an observed kappa value, calculated on a subsample of data, known as an observed test set, and a kappa threshold which indicates significant agreement between raters, known as the \emph{code set kappa max}.
#' 
#' It then generates a collection of simulated, 
#' fully-coded data sets, or \emph{code sets}, all 
#' of which have a kappa value below the code set kappa max.
#' 
#' Then kappa is calculated on a \emph{test set} sampled from each of the code sets in the
#' collection to create a null hypothesis distribution. These test sets mirror the observed test set in their size and sampling method.
#' 
#' The null hypothesis is that the observed test set, was sampled from a code set within a population of code sets which have kappas less than code set kappa max, or that the observed kappa is not representative of the level of agreement that would be reached if both raters were to code the entire data set.
#' 
#' If less than five percent of the distribution of samples from the code 
#' sets is greater than the observed kappa, the null hypothesis is 
#' rejected and one can conclude that two raters would have acceptable agreement 
#' had they coded the rest of the data.
#' 
#' @section rho: 
#' Use \code{\link{rho.set}} \cr \code{\link{rho.ct}}
#' 
#' @section kappa: 
#' Use \code{\link{kappa.set}} \cr \code{\link{kappa.ct}}
#' 
#' @section rho.minTSLength: 
#' Use \code{\link{rho.minTSLength}}
#'   
#' @docType package
#' @name rhoR
NULL