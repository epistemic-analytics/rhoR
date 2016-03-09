rho = function(set, isSet = TRUE, fullSetBaserate = NULL, handSetBaserateInflation=0, fullSetLength = 10000, runs=800, kappaThreshold=.65, kappaMin=.40, precisionMin = 0.6, precisionMax = 1, handSetFunction = getHandSet){
  kappa = kappa(set, isSet, NULL);
  if(is.null(fullSetBaserate)){
    fullSetBaserate = length(which(set[,1] == 1))/length(set)
  }
  return(list(rho = rho2(kappa, fullSetBaserate,length(set), handSetBaserateInflation, fullSetLength, runs, kappaThreshold, kappaMin, precisionMin, precisionMax, handSetFunction), kappa = kappa))
}
