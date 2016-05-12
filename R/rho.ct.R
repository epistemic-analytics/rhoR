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