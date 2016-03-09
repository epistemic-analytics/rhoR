#' Calculate Rho
#'
#' This function calculates rho given the parameters of the initial set and the kappa of the observed handset
#' @param observedKappa The kappa of the handset that rho is calculated on
#' @param handSetBaserate The baserate that the handset should be inflated to
#' @param fullSetBaserate The baserate that the parent set is at
#' @param handSetLength The length of the handset to be taken
#' @param fullSetLength The length of the parent set
#' @param runs The number of sets to add to the distribution
#' @param kappaThreshold The maximum kappa in the distribution
#' @param kappaMin The minimum kappa in the distributiond
#' @keywords rho, calculate, observedKappa, kappa, disribution
#' @return a P value is returned and if this value is less than 0.05, it is said that the handset does generalize to the entire set
#' calcRho()

calcRho = function(observedKappa, fullSetBaserate, handSetLength, handSetBaserateInflation=0, fullSetLength = 10000, runs=800, kappaThreshold=.65, kappaMin=.40, precisionMin = 0.6, precisionMax = 1, handSetFunction = getHandSet)
{
  #creats distribution from random kappa sets
  savedKappas = rep(0,runs);
  KPs = generateKPs(runs, fullSetBaserate, kappaMin, kappaThreshold, precisionMin, precisionMax)
  for (i in 1:runs) {
    KP = KPs[i]
    kappa = KP[[1]]$kappa
    precision = KP[[1]]$precision
    recall = getR(kappa, fullSetBaserate, precision)
    fullSet = prset(precision = precision, recall = recall, length = fullSetLength, baserate = fullSetBaserate);

    #computes kappa of handset to add to distribution
    savedKappas[i] = handSetFunction(set = fullSet, handSetLength = handSetLength, handSetBaserate = handSetBaserate);
  }

  # compare observedKappa to distribution
  return(getBootPvalue(savedKappas, observedKappa));
};
