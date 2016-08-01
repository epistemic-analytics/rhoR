###
#' @include rhoSet.R
#' @title Rho (contingency Table)
#' 
#' @description 
#' This function calculates rho and kappa for a given contingency table, and returns a list containing both values.
#' 
#' @export
#' @param table The \code{\link{contingencyTable}} used to calculate rho 
#' @param SDSBaserate The \code{\link{baserate}} of a simulated data set, if NULL, assume the baserate of the simulated data set is the same as the test
#' @param testSetBaserateInflation The percent, as a decimal, of the test set that is guaranteed to be a positive for the first rater
#' @param SDSLength The length of the simulated data sets used to calculate rho
#' @param replicates The number of simulated data sets to use in the null hypothesis distribution for rho
#' @param kappaThreshold The maximum kappa value used to generate simulated data sets in the null hypothesis distribution for rho
#' @param SDSKappaMin The minimum kappa value used to generate simulated data sets in the null hypothesis distribution for rho
#' @param SDSPrecisionMin The minimum precision to be used for generation of simulated data sets in the null hypothesis distribution for rho
#' @param SDSPrecisionMax The maximum precision to be used for generation of simulated data sets in the null hypothesis distribution for rho
#'
#' @note replicates as used in monte carlo studies.
#' @keywords rhoCT
#' 
#' @examples
#' # Given testSetBaserateInflation
#' rhoCT(contingencyTable, testSetBaserateInflation = 0.2)
#' 
#' # Given SDSBaserate and testSetBaserateInflation
#' rhoCT(contingencyTable, SDSBaserate = 0.1, testSetBaserateInflation = 0.2)
#' 
#' @return 
#' A list containing rho and kappa
###
rhoCT = function(
  table,
  SDSBaserate = NULL,
  testSetBaserateInflation = 0,
  SDSLength = 10000,
  replicates = 800,
  kappaThreshold = .65,
  SDSKappaMin = .40,
  SDSPrecisionMin = 0.6,
  SDSPrecisionMax = 1
){
  kappa = kappaCT(table);
  
  if(is.null(SDSBaserate)) {
      SDSBaserate = (sum(table[1,]))/(sum(table));
  }
  
  return(list(
    rho = rho(
      kappa, 
      SDSBaserate,
      sum(table),
      testSetBaserateInflation,
      SDSLength,
      replicates,
      kappaThreshold, SDSKappaMin, SDSPrecisionMin, SDSPrecisionMax
    ), 
    kappa = kappa
  ));
}