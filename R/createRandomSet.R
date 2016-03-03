#' Create A Random Set
#'
#' This creates a set with given parameters
#' @param setLength The length of the set to be created
#' @param baserate The baserate of the set to be created
#' @param kappa The kappa of the set to be created
#' @param minPrecision The minimum precision accepted in this set. Defaults to 0
#' @param maxPrecision The maximum precision accepted in this set. Defaults to 1
#' @keywords random, set, generation, kappa, ranges
#' @return This returns a set with the given parameters
#' createRandomSet()

createRandomSet = function(setLength, baserate, kappa, minPrecision = 0, maxPrecision = 1){
  #generates a valid precision with the given kappa and baserat
  precision = genPcombo(kappa, minPrecision, maxPrecision, baserate);
  #gets the correspoding recall from the kappa, baserate, and precision
  recall = getR(kappa, baserate, precision);
  #calls prset to generate the set
  return(prset(precision, recall, setLength, baserate));
}
