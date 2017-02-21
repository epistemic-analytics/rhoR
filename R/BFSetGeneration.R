BFSetGeneration = function(metric = list(calcKappa), range = list(c(0.4,0.65)), otherParameters = list(), setLength = 10000, baserate = NULL){
  #testSet = randomKappaSet(setLength, runif(1,0,0.5), runif(1,0,1), 0, 1)
  #Sys.sleep(0.001)
  testSet = generateRandomSet(setLength, baserate);
  #testSet = generateRandomContingencyTable(setLength, scale)
  #assign("testSet", testSet, envir = .GlobalEnv)
  #print(calcKappa(testSet))
  results = rep(0, length(metric))
  for(i in 1:length(metric)){
    FUN = metric[[i]]
    if(length(otherParameters) < i){
      params = list()
    }else{
      params = otherParameters[[i]]
    }
    
    if(length(params) != 0){
      paramHolder = paste(params, sep="", collapse = ",")
      call = paste(deparse(quote(FUN)), "(testSet,",paramHolder,")",sep = "")
      #print(call)
      metricResults = eval(parse(text = call))
      
    }else{
      metricResults = FUN(testSet)
    }
    #print(metricResults)
    currRange = range[[i]]
    lower = currRange[[1]]
    upper = currRange[[2]]
    if(is.nan(metricResults)){
      metricResults = lower - 1
    }
    goodSet = (lower <= metricResults) & (metricResults <= upper)
    if(!goodSet){
      #print("did not work... creating new set")
      #print(paste(metricResults, "...", lower, ":", upper))
      results = rep(0, length(metric))
      return(BFSetGeneration(metric, range, otherParameters, setLength, baserate));
    }
    results[i] = metricResults
  }
  #print(numTimes)
  #print("SET GENERATED")
  return(list(set = testSet, vals = results));
}