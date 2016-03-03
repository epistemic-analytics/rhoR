generateKPs = function(numNeeded, baserate, kappaMin, kappaMax, precisionMin, precisionMax, distributionType = 'FLAT', distributionLength = 100000){

  kappaDistribution = seq(kappaMin, kappaMax, length = distributionLength)

  if(distributionType == 'FLAT'){
    #probability for a flat distribution
    kappaProbability = NULL
  }else if(distributionType == 'BELL'){
    #probability for a bell curve
    kappaProbability = dnorm(kappaDistribution, mean = 0.65, sd = 0.1)
  }

  precisionDistribution = seq(precisionMin, precisionMax, length = 10000)
  precisionProbability = NULL

  KPs = list()
  for(i in 1:numNeeded){
    KPs[[length(KPs) + 1]] = genPKcombo(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate)
  }

  return (KPs);
}
