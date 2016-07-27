###
#' @include rho.R
#' @title Rho (set)
#' 
#' @description 
#' This function calculates rho and kappa for a given set, and returns a list containing both values.
#' 
#' @export
#' @param set The test \code{\link{set}} used to calculate rho 
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
#' @keywords rho_set
#' 
#' @examples
#' # Given testSetBaserateInflation
#' rho_set(set, testSetBaserateInflation = 0.2)
#' 
#' # Given SDSBaserate and testSetBaserateInflation
#' rho_set(set, SDSBaserate = 0.1, testSetBaserateInflation = 0.2)
#' 
#' @return 
#' A list containing rho and kappa
###
rho_set = function(
  set,
  SDSBaserate = NULL,
  testSetBaserateInflation = 0,
  SDSLength = 10000,
  replicates = 800,
  kappaThreshold = .65,
  SDSKappaMin = .40,
  SDSPrecisionMin = 0.6,
  SDSPrecisionMax = 1
){
  kappa = kappa_set(set);
  
  if(is.null(SDSBaserate)) {
    SDSBaserate = length(which(set[,1] == 1))/length(set);
  }
  
  return(list(
    rho = rho(
      kappa, 
      SDSBaserate,
      length(set),
      testSetBaserateInflation,
      SDSLength,
      replicates,
      kappaThreshold, SDSKappaMin, SDSPrecisionMin, SDSPrecisionMax
    ), 
    kappa = kappa
  ));
}