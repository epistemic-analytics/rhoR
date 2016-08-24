###
#' @include rho.R
#' @title Rho (set)
#' 
#' @description 
#' This function calculates rho and kappa for a given set, and returns a list containing both values.
#' 
#' @export
#' @param set The test \code{\link{codeSet}} used to calculate rho 
#' @param ScSBaserate The \code{\link{baserate}} of a simulated \code{\link{codeSet}}, if NULL, assume the baserate of the simulated \code{\link{codeSet}} is the same as the test
#' @param testSetBaserateInflation The minimum \code{\link{baserate}} gaurunteed for the \code{\link{testSet}}
#' @param ScSLength The length of the simulated \code{\link{codeSet}}s used to calculate rho
#' @param replicates The number of simulated \code{\link{codeSet}}s to use in the null hypothesis distribution for rho; similar to replicates in a Monte Carlo study
#' @param kappaThreshold The maximum kappa value used to generate simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSKappaMin The minimum kappa value used to generate simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSPrecisionMin The minimum precision to be used for generation of simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' @param ScSPrecisionMax The maximum precision to be used for generation of simulated \code{\link{codeSet}}s in the null hypothesis distribution for rho
#' 
#' @keywords rhoSet
#' 
#' @examples
#' # Given testSetBaserateInflation
#' rhoSet(codeSet, testSetBaserateInflation = 0.2)
#' 
#' # Given ScSBaserate and testSetBaserateInflation
#' rhoSet(codeSet, ScSBaserate = 0.1, testSetBaserateInflation = 0.2)
#' 
#' @return A list of the format:\describe{
#' \item{rho}{The rho of the \emph{table}}
#' \item{kappa}{The Cohen's Kappa of the \emph{table}}
#' }
###
rhoSet = function(
  set,
  ScSBaserate = NULL,
  testSetBaserateInflation = 0,
  ScSLength = 10000,
  replicates = 800,
  kappaThreshold = .65,
  ScSKappaMin = .40,
  ScSPrecisionMin = 0.6,
  ScSPrecisionMax = 1
){
  kappa = kappaSet(set);
  
  if(is.null(ScSBaserate)) {
    ScSBaserate = length(which(set[,1] == 1))/length(set);
  }
  
  return(list(
    rho = rho(
      kappa, 
      ScSBaserate,
      length(set),
      testSetBaserateInflation,
      ScSLength,
      replicates,
      kappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax
    ), 
    kappa = kappa
  ));
}