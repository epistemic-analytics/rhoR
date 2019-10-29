###
#' @include rhoK.R
#' @title Rho (set)
#' 
#' @description 
#' This function calculates rho and kappa for a given \code{\link[=getTestSet]{testSet}}, and returns a list containing both values. Called by \code{\link{rho}}.
#' 
#' @export
#' @param set The \code{\link[=getTestSet]{testSet}} used to calculate rho 
#' @param OcSBaserate The \code{\link{baserate}} of the observed \code{\link{codeSet}} (defaults to \code{\link{baserate}} of \code{\link[=getTestSet]{testSet}})
#' @param testSetBaserateInflation The minimum \code{\link{baserate}} from the sampling procedure
#' @param OcSLength The length of the observed \code{\link{codeSet}}
#' @param replicates The number of simulated \code{\link[=getTestSet]{codeSets}} to use in the null hypothesis distribution for rho; similar to replicates in a Monte Carlo study
#' @param ScSKappaThreshold The maximum kappa value used to generate simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSKappaMin The minimum kappa value used to generate simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSPrecisionMin The minimum precision to be used for generation of simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSPrecisionMax The maximum precision to be used for generation of simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' 
#' @keywords rhoSet
#' 
#' @seealso \code{\link{rho}} and \code{\link{rhoK}} and \code{\link{rhoCT}}
#' 
#' @return A list of the format:\describe{
#' \item{rho}{The rho of the \code{\link{codeSet}}}
#' \item{kappa}{The Cohen's Kappa of the \code{\link{codeSet}}}
#' }
###
rhoSet = function(
  set,
  OcSBaserate = NULL,
  testSetBaserateInflation = 0,
  OcSLength = 10000,
  replicates = 800,
  ScSKappaThreshold = .65,
  ScSKappaMin = .40,
  ScSPrecisionMin = 0.6,
  ScSPrecisionMax = 1
){
  kappa = kappaSet(set);
  
  row.count = nrow(set);
  if(is.null(OcSBaserate)) {
    OcSBaserate = length(which(set[,1] == 1)) / row.count;
  }
  return(list(
    rho = rhoK(
      kappa, 
      OcSBaserate,
      row.count,
      testSetBaserateInflation,
      OcSLength,
      replicates,
      ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax
    ), 
    kappa = kappa
  ));
}