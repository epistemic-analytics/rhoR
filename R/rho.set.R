###
#' @title Rho (set)
#' 
#' @description 
#' This function calculates rho and kappa for a given set
#' 
#' @export
#' @param set - The set to calculate rho on \code{\link{set}}
#' @param fullSetBaserate The baserate that the parent set is at; if NULL, assume the baserate of the child set is the same as the parent set
#' @param handSetBaserateInflation The baserate of the handset that is to be taken
#' @param fullSetLength The length of the parent set
#' @param runs The number of sets to use in the distribution
#' @param kappaThreshold The maximum kappa in the distribution (Often the threshold for a valid kappa)
#' @param kappaMin The minimum kappa in the distribution
#' @param precisionMin The minimum precision to be used for generation of sets in rho distribution
#' @param precisionMax The maximum precision to be used for generation of sets in rho distribution
#' @keywords rho.set
#' 
#' @examples
#' # Given \code{handSetBaserateInflation}
#' rho.set(set, handSetBaserateInflation = 0.2)
#' 
#' # Given \code{fullSetBaserate} and \code{handSetBaserateInflation}
#' rho.set(set, fullSetBaseate = 0.1, handSetBaserateInflation = 0.2)
#' 
#' @return 
#' A list containting rho and kappa
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
  kappa = kappa(set);
  
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