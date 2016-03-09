#' Calculate Rho
#'
#' This function calculates rho given the parameters of the initial set and the kappa of the observed handset
#' @param observedKappa The kappa of the handset that rho is calculated on
#' @param handSetBaserate The baserate that the handset should be inflated to
#' @param fullSetBaserate The baserate that the parent set is at
#' @param handSetLength The length of the handset to be taken
#' @param fullSetLength The length of the parent set
#' @param runs The number of sets to add to the distribution
#' @param kappaThreshold The maximum kappa in the distribution
#' @param kappaMin The minimum kappa in the distributiond
#' @keywords rho, calculate, observedKappa, kappa, disribution
#' @return a P value is returned and if this value is less than 0.05, it is said that the handset does generalize to the entire set
#' calcRho()

calcRho = function(observedKappa, handSetBaserate=.2, fullSetBaserate=.1, handSetLength=30, fullSetLength = 10000, runs=800, kappaThreshold=.65, kappaMin=.40, precisionMin = 0.6, precisionMax = 1, handSetFunction = getHandSet)
{
  #creats distribution from random kappa sets
  savedKappas = rep(0,runs);
  KPs = generateKPs(runs, fullSetBaserate, kappaMin, kappaThreshold, precisionMin, precisionMax)
  for (i in 1:runs) {
    KP = KPs[i]
    kappa = KP[[1]]$kappa
    precision = KP[[1]]$precision
    recall = getR(kappa, fullSetBaserate, precision)
    fullSet = prset(precision = precision, recall = recall, length = fullSetLength, baserate = fullSetBaserate);

    #computes kappa of handset to add to distribution
    savedKappas[i] = handSetFunction(set = fullSet, handSetLength = handSetLength, handSetBaserate = handSetBaserate);
  }

  # compare observedKappa to distribution
  return(getBootPvalue(savedKappas, observedKappa));
};

#' Calculate Kappa
#'
#' This function calculates kappa given a set
#' @param set This is the set to calculate kappa from
#' @param isSet TRUE if set FALSE if contigency table
#' @param kappaThreshold if null then return kappa otherwise return list of kappa and above or not
#' @keywords kappa, calculate
#' @return This returns the kappa of the set given
#' calcKappa()

calcKappa = function(set, isSet = TRUE, kappaThreshold = NULL) {
  if(!isSet){
    set = contingencyToSet(set[1,1],set[2,1],set[1,2],set[2,2])
  }
  #calculates kappa by calculating the agreement and the baserates and then creating the adjacenty matrix
  agreement = length(which(set[,1] == set[,2])) / nrow(set);
  baserate2 = length(which(1 == set[,2])) / nrow(set);
  baserate1 = length(which(1 == set[,1])) / nrow(set);
  randomAgreement = baserate1 * baserate2 + (1 - baserate1) * (1 - baserate2);
  kappa = (agreement - randomAgreement) / (1 - randomAgreement);

  if(is.null(kappaThreshold)){
    return(kappa);
  }else{
    return(list(kappa = kappa, above = kappa > kappaThreshold));
  }
  return(kappa);
};



contingencyToSet = function(TP, FP, FN, TN){
  setLength = TP + FP + FN + TN;

  gold1s = TP + FN;
  gold0s = setLength - gold1s;

  gold = c(rep(1, gold1s), rep(0, gold0s));
  silver = c(rep(1, TP),rep(0, gold1s - TP), rep(1, FP), rep(0, gold0s - FP));

  return(cbind(gold, silver));
};

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

#' Precision Recall Set
#'
#' This creates a set with a given recall and precision
#' @param precision This is the precision of the given set
#' @param recall This is the recall of the given set
#' @param length This is the length of the set
#' @param baserate This is the baserate of the set
#' @keywords prset, precision, recall, generation, set
#' @return returns a set with the given parameters
#' prset()

prset = function(precision, recall, length, baserate) {
  #generates a set given the different 4 quadrants
  gold1s = max(round(baserate * length , 0), 1); # TP + FN
  gold0s = length - gold1s; # FP + FN
  TP = max(round(gold1s * recall , 0) , 1); # TP
  FP = min(gold0s, max(round(TP * (1 - precision) / precision , 0), 1)); # FP
  gold = c(rep(1, gold1s), rep(0, gold0s));
  silver = c(rep(1, TP),rep(0, gold1s - TP), rep(1, FP), rep(0, gold0s - FP));

  return(cbind(gold, silver));
};

#' Get Recall
#'
#' This gets the recall from a pair
#' @param kappa This is the kappa of the pair
#' @param BR This is the baserate of the pair
#' @param P this is the precision of the pair
#' @keywords recall, get, kappa, baserate, precision
#' @return returns the recall given the parameters
#' getR()

getR = function(kappa, BR, P){
  #gets recall that corresponds with the kappa, baserate, and precision
  top = kappa*P;
  R = top / (2*P-2*BR-kappa+2*BR*kappa);
  return (R);
}

#' Get Handset
#'
#' This function is to get a handset of a set and calculate the kappa
#' @param set This is the set to take a handset of
#' @param handSetLength This is the length of the handset to take
#' @param handSetBaserate This is the minimum baserate to inflate the handset to
#' @param returnSet If TRUE, then return the handSet if FALSE, return the kappa of the handSet
#' @keywords handset, hand, inflation
#' @return The function returns the handSet if returnSet is TRUE or the kappa of the handSet if not
#' getHandSet()

getHandSet = function(set, handSetLength, handSetBaserate, returnSet=FALSE) {
  #positives is the minimum number of positive pairs in the first rater
  positives = round(handSetLength * handSetBaserate, 0);
  handSetKappa = NaN;

  #if there is an inflated baserate positives will be > 0, so if handSetBaserate > 0, positives > 0
  if (positives > 0) {
    #goes through and picks a set that fits the number of positives first then randomly samples from the rest
    #if the set created is not a valid set, then it reruns the process until a valid set is found
    while(is.nan(handSetKappa)) {
      tp = set[which(set[,1] == 1),];
      positiveIndices = sample(1:nrow(tp),size=positives,replace=FALSE);
      others = set[which(set[,1] == 0),];
      otherIndices = sample(1:nrow(others),size=(handSetLength - positives),replace=FALSE);
      handSet = rbind(tp[positiveIndices,],others[otherIndices,]);
      handSetKappa = calcKappa(handSet);
    }
  } else if (positives == 0){
    #if there is no restriction on positives, then the set is generated by randomly sampling from the entire set
    handSet=matrix(0,2,2);
    while (is.nan(handSetKappa) & (sum(handSet[,2]) == 0 | sum(handSet[,2]) == nrow(handSet))) {
      theseIndices = sample(1:nrow(set),size=handSetLength,replace=FALSE);
      handSet = set[theseIndices,];
      handSetKappa = calcKappa(handSet);
    }
  }

  #if the set generation still fails, print results and exit
  if(is.nan(handSetKappa)) {
    print(handSetLength);
    print(handSetBaserate);
    View(handSet);
  }

  #if return set is true return set rather than kappa
  if(returnSet){
    return(handSet);
  }

  #otherwise return the kappa of the set
  return(handSetKappa);
};

#' Get Boot P Value
#'
#' This function gets the p value of the result in the distribution
#' @param distribution The distribution that the result falls into
#' @param result The value that you are evaluating where in the distribution it falls
#' @keywords distribution, rho, p, boot
#' @return This returns the p value from the result into the distribution
#' getBootPvalue()

getBootPvalue = function(distribution, result) {
  #returns the percentage of the time that the distribution was greater or equal to the observed kappa
  N = length(distribution);
  if(result < mean(distribution)) {
    #if the result is less than the mean of the distribution, than the p value is 1
    return(1);
  } else {
    #return the number of times that the distribution is greater than the result as a percentage of the total number of items in the distribution
    return(length(which(distribution >= result)) / N);
  }
};

#' Generate Precision Combo
#'
#' This function generates a precision that fits within the range and will create a valid set
#' @param kappa This is the kappa of the desired pair
#' @param Pmin This is the minimum precision desired
#' @param Pmax This is the maximum precision desired
#' @param BR This is the baserate of the desired pair
#' @keywords generate, combo, kappa, precision, baserate
#' @return This returns a precision value that will generate a valid set with the other parameters
#' genPcombo()

genPcombo = function(kappa, Pmin, Pmax, BR){
  #gets a random precision in the precision range
  currPrecision = runif(1, Pmin, Pmax);
  #checks is the combination of precision, kappa, and baserate is valid.  If it is vaid, return the precision, if not, recall the function until a valid precision is found
  if(!checkBRPKcombo(BR, currPrecision, kappa)){
    genPcombo(kappa, Pmin, Pmax, BR);
  }else{
    return(currPrecision);
  }
}

#' Create A Random Set
#'
#' This creates a set with given parameters
#' @param setLength The length of the set to be created
#' @param baserate The baserate of the set to be created
#' @param kappa The kappa of the set to be created
#' @param minPrecision The minimum precision accepted in this set. Defaults to 0
#' @param maxPrecision The maximum precision accepted in this set. Defaults to 1
#' @keywords random, set, generation, kappa, ranges
#' @return This returns a set with the given parameters
#' createRandomSet()

createRandomSet = function(setLength, baserate, kappa, minPrecision = 0, maxPrecision = 1){
  #generates a valid precision with the given kappa and baserat
  precision = genPcombo(kappa, minPrecision, maxPrecision, baserate);
  #gets the correspoding recall from the kappa, baserate, and precision
  recall = getR(kappa, baserate, precision);
  #calls prset to generate the set
  return(prset(precision, recall, setLength, baserate));
}

#' Check Baserate, Precision, Kappa Combo
#'
#' This function checks to make sure the combination of BR, P, and K will create a valid set
#' @param BR This is the supplied baserate
#' @param P This is the supplied precision
#' @param K This is the supplied kappa
#' @keywords check, combo, baserate, precision, kappa
#' @return returns TRUE if the combination is valid, FALSE otherwise
#' checkBRPKcombo()

checkBRPKcombo = function(BR, P, K){
  #if right is less than P, then it is a valid combination. If not, then the pair of kappa, precision, and baserate does not correspond to a valid set
  right = (2*BR*K - 2*BR - K)/(K - 2);
  if(P > right){
    return(TRUE);
  }else{
    return(FALSE);
  }
}

