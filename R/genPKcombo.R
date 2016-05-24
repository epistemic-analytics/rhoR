genPKcombo = function(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate, bucketSize = 0.05){

  currKappa = sample(kappaDistribution, 1, replace = FALSE, prob = kappaProbability)
  currPrecision = sample(precisionDistribution, 1, replace = FALSE, prob = precisionProbability)

  if(!checkBRPKcombo(baserate, currPrecision, currKappa)){
    precisionMin = (2*baserate*currKappa - 2*baserate - currKappa)/(currKappa - 2)
    indicies = which(precisionDistribution > precisionMin)
    precisionDistribution = precisionDistribution[indicies]
    precisionProbability = precisionProbability[indicies]
    currPrecision = sample(precisionDistribution, 1, replace = FALSE, prob = precisionProbability)
    return(list(precision = currPrecision, kappa = currKappa))
  }else{
    return(list(precision = currPrecision, kappa = currKappa))
  }
}
