getTestSet = function(myKappa, myTestSetLength, range = 0.2, kappa = F){
  x = createSimulatedCodeSet(
    length = myTestSetLength, 
    baserate = 0.2, 
    kappaMin = myKappa, 
    kappaMax = myKappa + range, 
    precisionMin = 0, 
    precisionMax = 1
  )
  
  if(kappa == T) {
    return(kappa(x)) 
  } else {
    return(x[sample(1:nrow(x), size = nrow(x), replace = F),])
  }
}

testK = function(
  k = .7, 
  l = 40,
  range = 0.2
) {
  myData = getTestSet(myKappa =  k, myTestSetLength = l, range = range, kappa = T)
  
  under = myData < k
  over = myData > (k + range)
  return(c(kappa = myData, under = under, over = over, passed = (!under & !over)))
}

myData = getTestSet(
  myKappa = .7,
  myTestSetLength = 40,
  range = 0.01
)

checkFailureRate = function(n = 40, reps = 10000, Kmin = 0.4, Krange = 0.25) {
  res = data.frame(t(sapply(rep(Kmin, reps), testK, range = Krange, l = n)))
  fail.rate = length(which(res$passed == 0)) / reps
  return(c(n = n, fail.rate = fail.rate))
}

l400 = data.frame(t(sapply(rep(0.4, 100), testK, range = 0.25, l = 400)))
l400rate = length(which(l400$passed == 0)) / nrow(l400)
