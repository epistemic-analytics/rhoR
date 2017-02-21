
calcRhoGeneral = function(observedMetric, handSetBaserate=.2, fullSetBaserate=.1, handSetLength=30, fullSetLength = 10000, runs=800, metric = list(calcKappa), metricThreshold=.65, thresholdMin=.40, otherParameters = list())
{
  #creats distribution from random kappa sets
  savedKappas = rep(0,runs);
  sets = setGeneration(numSets = runs, setLength = fullSetLength, metric = metric, range = list(c(thresholdMin, metricThreshold)), otherParameters = otherParameters, bucketSize = 0.05, baserate = fullSetBaserate)
  for (i in 1:runs) {
    fullSet = sets[[i]]
    
    #computes kappa of handset to add to distribution
    savedKappas[i] = getHandSet(set = fullSet, handSetLength, handSetBaserate);
  }
  
  # compare observedMetric to distribution
  return(getBootPvalue(savedKappas, observedMetric));
}
