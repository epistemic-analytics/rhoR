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
    
    if(isSet != TRUE) {
      fullSetBaserate = (sum(set[1,]))/(sum(set));
    }
  }
  
  return(list(
    rho = rho.power(
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