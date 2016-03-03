genPKcombo = function(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate, bucketSize = 0.05){

  currKappa = sample(kappaDistribution, 1, replace = FALSE, prob = kappaProbability)
  currPrecision = sample(precisionDistribution, 1, replace = FALSE, prob = precisionProbability)

  if(!checkBRPKcombo(baserate, currPrecision, currKappa)){
    bucketMin = currKappa - (currKappa %% bucketSize)
    bucketMax = bucketMin + bucketSize
    indices = which(kappaDistribution < bucketMax & bucketMin < kappaDistribution)
    kappaDistribution = kappaDistribution[indices]
    #kappaProbability = kappaProbability[indices]
    #if flat in distribution then
    #kappaProbability = NULL
    genPKcombo(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate, bucketSize)
  }else{
    return(list(precision = currPrecision, kappa = currKappa))
  }
}
