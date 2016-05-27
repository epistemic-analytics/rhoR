###
#' @title Rho (contingency Table)
#' 
#' @description 
#' This function calculates rho and kappa for a given contingency Table, and returns a list containing both values.
#' 
#' @export
#' @param table The \code{\link{contingencyTable}} used to calculate rho 
#' @param fullSetBaserate The frequency of one's for the first rater in the full set, if NULL, assume the baserate of the hand set is the same as the full set
#' @param handSetBaserateInflation The percent, as a decimal, of the hand set that is gauranteed to be a positive for the first rater
#' @param fullSetLength The length of the full set
#' @param runs The number of full sets to use in the null hypothesis distribution for rho
#' @param kappaThreshold Threshold to compare against, as a double
#' @param kappaMin The minimum kappa in the null hypothesis distribution for rho
#' @param precisionMin The minimum percision to be used for generation of sets in null hypothesis distribution for rho
#' @param precisionMax The maximum percision to be used for generation of sets in null hypothesis distribution for rho
#' @keywords rho.ct
#' 
#' @examples
#' # Given handSetBaserateInflation
#' rho.ct(table, handSetBaserateInflation = 0.2)
#' 
#' # Given fullSetBaserate and handSetBaserateInflation
#' rho.ct(table, fullSetBaseate = 0.1, handSetBaserateInflation = 0.2)
#' 
#' @return 
#' A list containing rho and kappa
###
rho.ct = function(
  table,
  fullSetBaserate = NULL,
  handSetBaserateInflation = 0,
  fullSetLength = 10000,
  runs = 800,
  kappaThreshold = .65,
  kappaMin = .40,
  precisionMin = 0.6,
  precisionMax = 1
){
  kappa.ct = kappa(table);
  
  if(is.null(fullSetBaserate)) {
      fullSetBaserate = (sum(table[1,]))/(sum(table));
  }
  
  return(list(
    rho = rho(
      kappa, 
      fullSetBaserate,
      sum(table),
      handSetBaserateInflation,
      fullSetLength,
      runs,
      kappaThreshold, kappaMin, precisionMin, precisionMax
    ), 
    kappa = kappa
  ));
}