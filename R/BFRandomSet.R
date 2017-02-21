generateRandomSet = function(setLength, baserate = NULL){
  #percentMatch = sample(dnorm(seq(0,1,10000), mean = 0.65, sd = 0.1),1)
  percentMatch = runif(1,0,1)
  percentagePositiveMatch = runif(1,0,1)
  if(is.null(baserate)){
    #baserate = sample(dnorm(seq(0,0.8,10000), mean = 0.15, sd = 0.1),1)
    baserate = runif(1, 0, 0.8)
  }
  matches = trunc(percentMatch * setLength)
  positives = trunc(baserate * setLength)
  positiveMatches = trunc(percentagePositiveMatch*matches)
  if(positiveMatches > positives){
    positiveMatches = positives
  }
  negativeMatches = matches - positiveMatches
  negatives = setLength - positives
  if(negatives < negativeMatches){
    negativeMatches = negatives
  }
  nonMatchingPositives = positives - positiveMatches
  nonMatchingNegatives = negatives - negativeMatches
  
  #GIVES TOO HIGH OF BASERATE
  #gold = c(sample(scale, (setLength - positives), replace = TRUE), rep(scale[[1]], positives))
  #silver = c(gold[1:matches], sample(scale, (setLength - matches), replace = TRUE))
  
  gold = c(rep(1, positives), rep(0, setLength - positives))
  silver = c(rep(1, positiveMatches), rep(0, nonMatchingPositives + negativeMatches), rep(1, nonMatchingNegatives))
  return(cbind(gold, silver));
}