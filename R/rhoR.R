#' rhoR: A package for computating rho.
#' 
#' rho is used to test the generalization of inter rater reliability (IRR)
#' statistics, in this case Cohen's Kappa.
#' 
#' Rho starts with an observed kappa value, calculated on a subsample of data, known as an observed \code{\link{testSet}}, and a \emph{kappa threshold} which indicates significant agreement between raters.
#' 
#' It then generates a collection of fully-coded, simulated 
#' \code{\link{codeSet}}s (ScS), further described in \code{\link{createScS}}, all 
#' of which have a kappa value below the kappa threshold.
#' 
#' Then kappa is calculated on a simulated \code{\link{testSet}} sampled from each of the ScSs in the
#' collection to create a null hypothesis distribution. These simulated \code{\link{testSet}}s mirror the observed \code{\link{testSet}} in their size and sampling method, which is futher described in \code{\link{testSet}}
#' 
#' The null hypothesis is that the observed \code{\link{testSet}}, was sampled from a ScS within a population of simulated \code{\link{codeSet}}s which have kappas less than ScS kappa max, or that the observed kappa is not representative of the level of agreement that would be reached if both raters were to code the entire \code{\link{codeSet}}.
#' 
#' Researchers can set their own alpha level. If alpha is greater than the percentage of the distribution of samples from the code
#' sets that is greater than the observed kappa, the null hypothesis is 
#' rejected and one can conclude that two raters would have acceptable agreement 
#' had they coded the rest of the data.
#' 
#' @section rho: 
#' Use \code{\link{rhoSet}} \cr \code{\link{rhoCT}}
#' 
#' @section kappa: 
#' Use \code{\link{kappaSet}} \cr \code{\link{kappaCT}}
#' 
#' @section rhoMinTSLength: 
#' Use \code{\link{rhoMinTSLength}}
#'   
#' @docType package
#' @name rhoR
NULL