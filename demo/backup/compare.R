
ctFromSet <- function(set) {
  tp = length(which(set[,1] == 1 & set[,2] == 1))
  tn = length(which(set[,1] == 0 & set[,2] == 0))
  fp = length(which(set[,1] == 0 & set[,2] == 1))
  fn = length(which(set[,1] == 1 & set[,2] == 0))
  
  matrix(c(tp,fp,fn,tn), ncol=2, nrow=2)
}

compareMakers <- function(
  baserate = 0.5,
  threshold = 0.65,
  setLength = 10000,
  kappaMin = 0.4,
  kappaMax = 1.0,
  minPrecision = 0.6,
  maxPrecision = 1.0,
  handSetLength = 20,
  handSetBaserate = 0.2,
  replicates = 800
) {
  # ct = createRandomCT(
  #   setLength, baserate = baserate,
  #   kappaMin = kappaMin, kappaMax = kappaMax,
  #   minPrecision = minPrecision, maxPrecision = maxPrecision
  # )
  
  set = createRandomSet(
    setLength, baserate = baserate,
    kappaMin = kappaMin, kappaMax = kappaMax,
    minPrecision = minPrecision, maxPrecision = maxPrecision
  )
  
  # ctSet = ctFromSet(set)
  # ctHandset = getHand_ct(  ct,  handSetLength = handSetLength, handSetBaserate = handSetBaserate)
  
  setHandset = getHandSet(set, handSetLength = handSetLength, handSetBaserate = handSetBaserate, returnSet = T)
  setHandsetCT = ctFromSet(setHandset)
  
  rhoCT = rhoCT_c(setHandsetCT, testSetBaserateInflation = handSetBaserate, replicates = replicates)
  rhoCT2 = rhoCT(setHandsetCT, testSetBaserateInflation = handSetBaserate, replicates = replicates)
  rhoSet = rhoSet(setHandset, testSetBaserateInflation = handSetBaserate, replicates = replicates)
  # browser()
  list(
    # kappa = (rhoCT[1] == rhoSet$kappa), 
    # rho = (rhoCT[2] == rhoSet$rho), 
    CTnew = rhoCT[2],
    CT = rhoCT2$rho,
    set = rhoSet$rho
  )
}
compareMakers()