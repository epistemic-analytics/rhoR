genPKcomboPFocus = function(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate){
  
  currKappa = sample(kappaDistribution, 1, replace = FALSE, prob = kappaProbability)
  currPrecision = sample(precisionDistribution, 1, replace = FALSE, prob = precisionProbability)
  
  if(!checkBRPKcombo(baserate, currPrecision, currKappa)){
    kappaMax = (2*baserate - 2*currPrecision) / (2*baserate - 2* currPrecision - 1)
    indicies = which(kappaDistribution < kappaMax)
    if(length(indicies) == 0){
      return(genPKcombo(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate))
    }
    kappaDistribution = kappaDistribution[indicies]
    kappaProbability = kappaProbability[indicies]
    currKappa = sample(kappaDistribution, 1, replace = FALSE, prob = kappaProbability)
    return(list(precision = currPrecision, kappa = currKappa))
  }else{
    return(list(precision = currPrecision, kappa = currKappa))
  }
}
