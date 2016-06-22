###
#' @include rho.set.R
#' @title Rho (contingency Table)
#' 
#' @description 
#' This function calculates rho and kappa for a given contingency table, and returns a list containing both values.
#' 
#' @export
#' @param table The \code{\link{contingencyTable}} used to calculate rho 
#' @param codeSetBaserate The frequency of ones for the first rater in the code set, if NULL, assume the baserate of the test set is the same as the code set
#' @param testSetBaserateInflation The percent, as a decimal, of the test set that is guaranteed to be a positive for the first rater
#' @param codeSetLength The length of the code set used to calculate rho
#' @param runs The number of code sets to use in the null hypothesis distribution for rho
#' @param CSKappaMax The maximum kappa value used to generate code sets in the null hypothesis distribution for rho
#' @param CSKappaMin The minimum kappa value used to generate code sets in the null hypothesis distribution for rho
#' @param precisionMin The minimum precision to be used for generation of code sets in the null hypothesis distribution for rho
#' @param precisionMax The maximum precision to be used for generation of code sets in the null hypothesis distribution for rho
#' @keywords rho.ct
#' 
#' @examples
#' # Given testSetBaserateInflation
#' rho.ct(contingencyTable, testSetBaserateInflation = 0.2)
#' 
#' # Given codeSetBaserate and testSetBaserateInflation
#' rho.ct(contingencyTable, codeSetBaseate = 0.1, testSetBaserateInflation = 0.2)
#' 
#' @return 
#' A list containing rho and kappa
###
rho.ct = function(
  table,
  codeSetBaserate = NULL,
  testSetBaserateInflation = 0,
  codeSetLength = 10000,
  runs = 800,
  CSKappaMax = .65,
  CSKappaMin = .40,
  precisionMin = 0.6,
  precisionMax = 1
){
  kappa = kappa.ct(table);
  
  if(is.null(codeSetBaserate)) {
      codeSetBaserate = (sum(table[1,]))/(sum(table));
  }
  
  return(list(
    rho = rho(
      kappa, 
      codeSetBaserate,
      sum(table),
      testSetBaserateInflation,
      codeSetLength,
      runs,
      CSKappaMax, CSKappaMin, precisionMin, precisionMax
    ), 
    kappa = kappa
  ));
}