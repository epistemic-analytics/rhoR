rho = function(data, OcSBaserate = NULL,
               testSetLength = NULL,
               testSetBaserateInflation = 0,
               OcSLength = 10000,
               replicates = 800,
               ScSKappaThreshold = .65,
               ScSKappaMin = .40,
               ScSPrecisionMin = 0.6,
               ScSPrecisionMax = 1){
  if(is.numeric(data)){
    return(rho(data, OcSBaserate,
               testSetLength,
               testSetBaserateInflation,
               OcSLength,
               replicates,
               ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax))
  }else if(nrow(data) != 2){
    return(rhoSet(data, OcSBaserate,
                  length(set),
                  testSetBaserateInflation,
                  OcSLength,
                  replicates,
                  ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax))
  }else{
    return(rhoCT(data, OcSBaserate,
                 sum(data),
                 testSetBaserateInflation,
                 OcSLength,
                 replicates,
                 ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax))
  }
}