###
#' @title Rho (contingency Table)
#' 
#' @description 
#' This function calculates rho and kappa for a given contingency Table
#' 
#' @export
#' @param table - The contingency table to calculate rho on \code{\link{contingencyTable}}
#' @param fullSetBaserate The baserate that the parent set is at; if NULL, assume the baserate of the child set is the same as the parent set
#' @param handSetBaserateInflation The baserate of the handset that is to be taken
#' @param fullSetLength The length of the parent set
#' @param runs The number of sets to use in the distribution
#' @param kappaThreshold The maximum kappa in the distribution (Often the threshold for a valid kappa)
#' @param kappaMin The minimum kappa in the distribution
#' @param precisionMin The minimum precision to be used for generation of sets in rho distribution
#' @param precisionMax The maximum precision to be used for generation of sets in rho distribution
#' @keywords rho.ct
#' 
#' @examples
#' # Given \code{handSetBaserateInflation}
#' rho.ct(table, handSetBaserateInflation = 0.2)
#' 
#' # Given \code{fullSetBaserate} and \code{handSetBaserateInflation}
#' rho.ct(table, fullSetBaseate = 0.1, handSetBaserateInflation = 0.2)
#' 
#' @return 
#' A list containting rho and kappa
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
  kappa = kappa(table, FALSE);
  
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