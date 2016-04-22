#' rhoR: A package for computating rho.
#' 
#' rho is used to test the generalization of inter rater reliability (IRR)
#' statistics.
#' 
#' Calculating rho starts by generating a large number of simulated, 
#' fully-coded data sets: a sizable collection of hypothetical populations, all 
#' of which have a kappa value below a given threshold - which indicates 
#' unacceptable agreement.
#' 
#' Then kappa is calculated on a sample from each of those sets in the 
#' collection to see if it is equal to or higher than the kappa in then real 
#' sample.
#' 
#' If less than five percent of the distribution of samples from the simulated 
#' data sets is greater than actual observed kappa, the null hypothesis is 
#' rejected and one can conclude that two raters would have acceptable agreement 
#' had they coded all of the rest of the data.
#' 
#' @section rho: 
#' Use \code{\link{rho}}
#' 
#' @section kappa: 
#' Use \code{\link{kappa}}
#' 
#' @section rho.power: 
#' Use \code{\link{rho.power}}
#'   
#' @docType package
#' @name rhoR
NULL