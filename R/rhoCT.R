###
#' @include rhoSet.R
#' @title Rho (contingency Table)
#' 
#' @description 
#' This function calculates rho and kappa for a given \code{\link{contingencyTable}}, and returns a list containing both values. Called by \code{\link{rho}}.
#' 
#' @export
#' @param contingencyTable The \code{\link{contingencyTable}} used to calculate rho 
#' @param OcSBaserate The \code{\link{baserate}} of the observed \code{\link{codeSet}} (defaults to \code{\link{baserate}} of the \code{\link{contingencyTable}})
#' @param testSetBaserateInflation The minimum \code{\link{baserate}} from the sampling procedure
#' @param OcSLength The length of the observed \code{\link{codeSet}}
#' @param replicates The number of simulated \code{\link[=getTestSet]{codeSets}} to use in the null hypothesis distribution for rho; similar to replicates in a Monte Carlo study
#' @param ScSKappaThreshold The maximum kappa value used to generate simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSKappaMin The minimum kappa value used to generate simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSPrecisionMin The minimum precision to be used for generation of simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#' @param ScSPrecisionMax The maximum precision to be used for generation of simulated \code{\link[=getTestSet]{codeSets}} in the null hypothesis distribution for rho
#'
#' @keywords rhoCT
#' 
#' @seealso \code{\link{rho}} and \code{\link{rhoK}} and \code{\link{rhoSet}}
#' 
#' @return A list of the format:\describe{
#' \item{rho}{The rho of the \code{\link{contingencyTable}}}
#' \item{kappa}{The Cohen's Kappa of the \code{\link{contingencyTable}}}
#' }
###
rhoCT = function(
  contingencyTable,
  OcSBaserate = NULL,
  testSetBaserateInflation = 0,
  OcSLength = 10000,
  replicates = 800,
  ScSKappaThreshold = .65,
  ScSKappaMin = .40,
  ScSPrecisionMin = 0.6,
  ScSPrecisionMax = 1
){
  if(any(contingencyTable < 0)){stop("Values in Contingency Table must be positive")}
  kappa = kappaCT(contingencyTable);
  
  
  if(is.null(OcSBaserate)) {
      OcSBaserate = (sum(contingencyTable[1,]))/(sum(contingencyTable));
  }
  
  return(list(
    rho = rhoK(
      kappa, 
      OcSBaserate,
      sum(contingencyTable),
      testSetBaserateInflation,
      OcSLength,
      replicates,
      ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax
    ), 
    kappa = kappa
  ));
}