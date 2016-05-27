###
#' @title Rho (set)
#' 
#' @description 
#' This function calculates rho and kappa for a given set, and returns a list containing both values.
#' 
#' @export
#' @param set The \code{\link{set}} used to calculate rho 
#' @param fullSetBaserate The frequency of one's for the first rater in the full set, if NULL, assume the baserate of the hand set is the same as the full set
#' @param handSetBaserateInflation The percent, as a decimal, of the hand set that is gauranteed to be a positive for the first rater
#' @param fullSetLength The length of the full set
#' @param runs The number of full sets to use in the null hypothesis distribution for rho
#' @param kappaThreshold Threshold to compare against, as a double
#' @param kappaMin The minimum kappa in the null hypothesis distribution for rho
#' @param precisionMin The minimum percision to be used for generation of sets in null hypothesis distribution for rho
#' @param precisionMax The maximum percision to be used for generation of sets in null hypothesis distribution for rho
#' @keywords rho.set
#' 
#' @examples
#' # Given handSetBaserateInflation
#' rho.set(set, handSetBaserateInflation = 0.2)
#' 
#' # Given fullSetBaserate and handSetBaserateInflation
#' rho.set(set, fullSetBaseate = 0.1, handSetBaserateInflation = 0.2)
#' 
#' @return 
#' A list containing rho and kappa
###
rho.set = function(
  set,
  fullSetBaserate = NULL,
  handSetBaserateInflation = 0,
  fullSetLength = 10000,
  runs = 800,
  kappaThreshold = .65,
  kappaMin = .40,
  precisionMin = 0.6,
  precisionMax = 1
){
  kappa.set = kappa(set);
  
  if(is.null(fullSetBaserate)) {
    fullSetBaserate = length(which(set[,1] == 1))/length(set);
  }
  
  return(list(
    rho = rho(
      kappa, 
      fullSetBaserate,
      length(set),
      handSetBaserateInflation,
      fullSetLength,
      runs,
      kappaThreshold, kappaMin, precisionMin, precisionMax
    ), 
    kappa = kappa
  ));
}