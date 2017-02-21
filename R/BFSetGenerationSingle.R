setGeneration = function(numSets = 800, setLength = 10000, metric = list(calcKappa), range = list(c(0.4,0.65)), otherParameters = list(), bucketSize = 0.05, baserate = NULL){
  numBuckets = round((range[[1]][[2]] - range[[1]][[1]])/bucketSize)
  buckets = rep(0,numBuckets)
  bucketMax = ceiling(numSets / numBuckets)
  sets = list()
  done = FALSE
  while(!done){
    currSetHolder = BFSetGeneration(metric, range, otherParameters, setLength, baserate)
    bucketValue = currSetHolder[[2]][1]
    bucketValue = bucketValue - range[[1]][[1]]
    bucketIndex = trunc(bucketValue / bucketSize) + 1
    if(bucketIndex > numBuckets){
      bucketIndex = bucketIndex - 1
    }
    if(buckets[bucketIndex] < bucketMax){
      sets[[length(sets) + 1]] = currSetHolder[[1]]
      buckets[bucketIndex]= buckets[bucketIndex] + 1
      #print(paste("Set", length(sets), "found"))
    }
    if(length(sets) == numSets){
      done = TRUE
    }
  }
  return(sets)
}