// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;

// ' @export
// [[Rcpp::export]]
arma::ivec toArmaVec(arma::imat m) {
  arma::ivec v = arma::ivec(m.size());
  int index = 0;
  for(int i = 0; i < m.n_cols; i++) {
    for(int j = 0; j < m.n_rows; j++) {
      v(index) = m(j, i);
      index++;
    }
  }
  return(v);
}

NumericMatrix toNumericMatrix(arma::imat x) {
  int nRows=x.n_rows;
  
  NumericMatrix y(nRows, x.n_cols);
  for (int j=0; j<x.n_rows; j++) {
    for (int i=0; i<x.n_cols; i++) {
      y(j,i) = x(j, i);
    }
  }
  
  return y;
}

// @name sample_weighted
// @title sample_weighted
// @param x vector
// @param n integer
// @export
//' @export
// [[Rcpp::export]]
NumericVector sample_weighted(SEXP x, int n) {
  NumericVector ret(n);
  NumericVector xx(x);
  IntegerVector rng = seq(0, xx.size()-1);
  for(int w = 0; w < n; w++) {
    int tot = sum(xx);
    NumericVector probs = xx / tot;
    IntegerVector s = sample(rng, 1, false,  probs);
    xx[s[0]] -= 1;
    ret[w] = s[0];
  }
  return(ret + 1);
}

// ' @export
// [[Rcpp::export]]
arma::ivec sample_weighted2(arma::imat xx, int n, bool forR = true) {
  arma::ivec ret = arma::ivec(n);
  NumericMatrix x = toNumericMatrix(xx);
  IntegerVector rng = seq(0, x.size() - 1);
  
  for(int w = 0; w < n; w++) {
    double tot = accu(xx);
    NumericVector probs = x / tot;
    IntegerVector s = sample(rng, 1, false,  probs);
    x[s[0]] -= 1;
    ret[w] = s[0];
  }
  
  if(forR == true) {
    return(ret + 1);
  } else {
    return(ret);
  }
}


// ' @export
// [[Rcpp::export]]
arma::ivec sample_weighted3(arma::imat xx, int n, bool forR = true) {
  arma::ivec ret = arma::ivec(n);
  arma::ivec sums = arma::ivec(4);
  
  int maxInt;
  int s;
  for(int w = 0; w < n; w++) {
    maxInt = accu(xx);
    s = randi<int>(distr_param(0, maxInt));
    ret[w] = (s <= xx(0) ? 0 : (s <= (xx(0)+xx(1))) ? 1 : (s <= (xx(0)+xx(1)+xx(2))) ? 2 : 3);
    xx(ret[w])--;
  }
  
  if(forR == true) return(ret + 1);
  else return(ret);
}

// returns the percentage of the time that the distribution was greater or equal to the observed kappa
// ' @export
// [[Rcpp::export]]
double getBootPvalue_c(arma::vec distribution, double result) {
  // if the result is less than the mean of the distribution, than the p value is 1
  if(result < mean(distribution)) {
    return(1.0);
  } 
  
  // return the number of times that the distribution is greater than the result as a percentage of the total number of items in the distribution
  else {
    arma::uvec matched = find(distribution >= result);
    double rho = (matched.size() * 1.0) / distribution.size();
    return( rho );
  }
}

// [[Rcpp::export]]
bool checkBRPKcombo_c(float BR, float P, float K) {
  float right = (2*BR*K - 2*BR - K)/(K - 2);
  
  return(P > right);
}

// [[Rcpp::export]]
float getR_c(float kappa, float BR, float P) {
  float top = kappa * P;
  float R = top / (2*P-2*BR-kappa+2*BR*kappa);
  return(R);
}
  
// [[Rcpp::export]]
NumericVector genPKcombo_c(
  arma::colvec kappaDistribution, arma::vec kappaProbability, 
  arma::colvec precisionDistribution, arma::vec precisionProbability, 
  float baserate
) {
  IntegerVector kappaRng = seq(0, kappaDistribution.size()-1);
  NumericVector kappaProbs = NumericVector(kappaProbability.begin(), kappaProbability.end());
  
  IntegerVector whKappa;
  if(kappaProbability.size() > 0) {
    whKappa = sample(kappaRng, 1, false, kappaProbs);
  } else {
    whKappa = sample(kappaRng, 1, false);
  }
  float currKappa = kappaDistribution[whKappa[0]];

  IntegerVector precRng = seq(0, precisionDistribution.size()-1);
  NumericVector precProbs; 
  
  IntegerVector whPrec = sample(precRng, 1, false); //precProbs);
  float currPrec = precisionDistribution[whPrec[0]];

  if(!checkBRPKcombo_c(baserate, currPrec, currKappa)) {
    float precisionMin = (2*baserate*currKappa - 2*baserate - currKappa)/(currKappa - 2);
    arma::uvec indicies = find(precisionDistribution > precisionMin);
    
    if(indicies.size() == 0){
      return(genPKcombo_c(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate));
    }
  
    precisionDistribution = precisionDistribution.elem(indicies);
    if(precisionProbability.size() > 0) {
      precisionProbability = precisionProbability.elem(indicies);
    }
    
    precRng = seq(0, precisionDistribution.size()-1);
    whPrec = sample(precRng, 1, false);
    currPrec = precisionDistribution[whPrec[0]];
  }
  
  return(Rcpp::NumericVector::create(currKappa, currPrec));
}

// [[Rcpp::export]]
Rcpp::NumericMatrix generateKPs_c(
    int numNeeded, float baserate, 
    float kappaMin, float kappaMax, 
    float precisionMin, float precisionMax, 
    int distributionType = 0, long distributionLength = 10000
){
  float kappaStep = ((kappaMax - kappaMin) / (distributionLength - 1));
  colvec kappaDistribution = regspace(kappaMin, kappaStep, kappaMax);

  arma::vec kappaProbability;
  if(distributionType == 1) {
    kappaProbability = normpdf(kappaDistribution, 0.65, 0.1);
  }

  float precStep = (precisionMax - precisionMin) / (10000 - 1);
  colvec precisionDistribution = regspace(precisionMin, precStep, precisionMax);

  arma::vec precisionProbability;
  Rcpp::NumericMatrix KPs(numNeeded,2);
  for(int i=0; i < numNeeded; i++) {
    KPs(i, _) = genPKcombo_c(kappaDistribution, kappaProbability, precisionDistribution, precisionProbability, baserate);
  }

  return (KPs);
}

// [[Rcpp::export]]
arma::imat ct_from_pr(float precision, float recall, int length, float baserate) {
  int gold1s = max(NumericVector::create(round(baserate * length), 1));
  int gold0s = length - gold1s;
  int TP = max(NumericVector::create(round(gold1s * recall), 1));
  int FP = min(NumericVector::create(gold0s, max(NumericVector::create(round(TP * (1 - precision) / precision),  1))));
  
  arma::imat ct = arma::imat(2,2);
  ct(0,0) = TP;
  ct(0,1) = gold1s - TP;
  ct(1,0) = FP;
  ct(1,1) = gold0s - FP;
  
  return(ct);
}

// @name createRandomCT
// @title createRandomCT
// @param setLength [TBD]
// @param baserate [TBD]
// @param kappaMin [TBD]
// @param kappaMax [TBD]
// @param minPrecision [TBD]
// @param maxPrecision [TBD]
// @export
//' @export
// [[Rcpp::export]]
arma::imat createRandomCT(
    int setLength,float baserate, 
    float kappaMin, float kappaMax, 
    float minPrecision = 0, float maxPrecision = 1
){
  // generates a valid precision with the given kappa and baserate
  NumericMatrix KP = generateKPs_c(1, baserate, kappaMin, kappaMax, minPrecision, maxPrecision);
  float kappa = KP(0,0);
  float precision = KP(0,1);
  float recall = getR_c(kappa, baserate, precision);
  
  arma::imat ct = ct_from_pr(precision, recall, setLength, baserate);
  
  return(ct);
}

//' @export
// [[Rcpp::export]]
double ct_kappa_c(arma::imat ct) {
  double a = ct(0,0); //gold 1 & silver 1
  double c = ct(0,1); //gold 1 & silver 0
  double b = ct(1,0); //gold 0 & silver 1
  double d = ct(1,1); //gold 0 & silver 0
  double size = accu(ct);
  
  double pZero = (a+d) / size;
  double pYes = ((a + b) / size) * ((a + c) / size);
  double pNo =  ((c + d) / size) * ((b + d) / size);
  double pE = (pYes + pNo);
  double k = (pZero - pE) / (1 - pE);
  
  return(k);
}

// @name getHand_ct
// @title getHand_ct
// @param ct [TBD]
// @param handSetLength [TBD]
// @param handSetBaserate [TBD]
//' @export
// [[Rcpp::export]]
arma::imat getHand_ct(arma::imat ct, int handSetLength, double handSetBaserate) {
  int positives = ceil(handSetLength * handSetBaserate);
  
  arma::ivec positiveIndices; 
  arma::imat otherCT = ct;
  
  if(positives > 0) {
    // Rcpp::Rcout << "Positives: " << positives << std::endl;
    // ct.print();
    arma::irowvec gold1s = ct.row(0);
    positiveIndices = sample_weighted3(gold1s, positives, false);
    // positiveIndices.print();
  
    double sumOnes = sum(positiveIndices == 0);
    double sumTwos = sum(positiveIndices == 1);
    positiveIndices *= 2;
    otherCT(0,0) = otherCT(0,0) - sumOnes;
    otherCT(0,1) = otherCT(0,1) - sumTwos;
  }
  
  arma::ivec otherAsVector = toArmaVec(otherCT);
  arma::ivec otherIndices = sample_weighted3(otherCT, handSetLength - positives, false);
  arma::ivec allIndices = join_cols<ivec>(positiveIndices, otherIndices);
  arma::imat newCT = arma::imat(2,2);
  
  newCT(0,0) = sum(allIndices == 0);
  newCT(1,0) = sum(allIndices == 1);
  newCT(0,1) = sum(allIndices == 2);
  newCT(1,1) = sum(allIndices == 3);
  return(newCT);
}

//' @export
// [[Rcpp::export]]
double getHand_kappa(arma::imat ct, int handSetLength, double handSetBaserate) {
  arma::imat newCT = getHand_ct(ct, handSetLength, handSetBaserate);
  
  double k = ct_kappa_c(newCT);
  return(k);
}

// @name calcRho_c
// @title calcRho_c
// @param observedKappa [TBD]
// @param fullSetBaserate [TBD]
// @param handSetLength [TBD]
// @param handSetBaserateInflation [TBD]
// @param fullSetLength [TBD]
// @param runs [TBD]
// @param kappaThreshold [TBD]
// @param kappaMin [TBD]
// @param precisionMin [TBD]
// @param precisionMax [TBD]
//' @export
// [[Rcpp::export]]
double calcRho_c(
  double observedKappa, 
  double fullSetBaserate, 
  int handSetLength, 
  double handSetBaserateInflation = 0,
  int fullSetLength = 10000, 
  int runs = 800, 
  double kappaThreshold = 0.65,
  double kappaMin = 0.40,
  double precisionMin = 0.6, 
  double precisionMax = 1.0
) {
  NumericMatrix KPs = generateKPs_c(runs, fullSetBaserate, kappaMin, kappaThreshold, precisionMin, precisionMax);
  arma::vec savedKappas = arma::vec(KPs.nrow());
 
  for(int KProw = 0; KProw < KPs.nrow(); KProw++) {
    NumericVector KP = KPs(KProw, _);
    double kappa = KP[0];
    double precision = KP[1];
    double recall = getR_c(kappa, fullSetBaserate, precision);
    
    arma::imat fullCT = ct_from_pr(precision, recall, fullSetLength, fullSetBaserate);
    double kk = getHand_kappa(fullCT, handSetLength, handSetBaserateInflation);
    savedKappas[KProw] = kk;
  };
  
  // compare observedKappa to distribution
  return(getBootPvalue_c(savedKappas, observedKappa));
}

// @name rhoCT_c
// @title rhoCT_ct
// @param contingencyTable [TBD]
// @param testSetBaserateInflation [TBD]
// @param replicates [TBD]
// @param OcSLength [TBD]
// @param OcSBaserate [TBD]
// @param ScSKappaThreshold [TBD]
// @param ScSKappaMin [TBD]
// @param ScSPrecisionMin [TBD]
// @param ScSPrecisionMax [TBD]
//' @export
// [[Rcpp::export]]
arma::vec rhoCT_c(
  arma::imat contingencyTable,
  double testSetBaserateInflation = 0.2,
  int replicates = 800,
  int OcSLength = 10000,
  double OcSBaserate = -1.0,
  double ScSKappaThreshold = 0.65,
  double ScSKappaMin = 0.40,
  double ScSPrecisionMin = 0.6,
  double ScSPrecisionMax = 1
){
  // Observed kappa
  double kappa = ct_kappa_c(contingencyTable);
  double size = accu(contingencyTable);
  double gold1;
  
  if(OcSBaserate == -1.0) {
    gold1 = accu(contingencyTable.row(0));
    OcSBaserate = gold1 / size;
  }
  
  double rho = calcRho_c(
    kappa, OcSBaserate,
    size, testSetBaserateInflation,
    OcSLength, replicates,
    ScSKappaThreshold, ScSKappaMin, ScSPrecisionMin, ScSPrecisionMax
  );
  
  return(NumericVector::create(kappa, rho));
}


//' @export
// [[Rcpp::export]]
bool run_rho_in_c(
    double threshold,  double infl,
    int handsetLength = 20,
    double handsetBaserate = 0.2,
    int replicates = 10000
) {
  arma::imat ct = createRandomCT(10000, infl, 0.4, threshold, 0.2, 0.8);
  // handset --> testset
  arma::imat handset = getHand_ct(ct, handsetLength, handsetBaserate);
  arma::vec res = rhoCT_c(handset, handsetBaserate, replicates, 10000, -1, 0.65, 0.4, 0.6, 1);
  
  return(res[0]>threshold);
}

/*** R
#ct_from_pr(0.2, 0.4, 20, 0.2)
# generateKPs(10, 0.2, 0.4, 0.65, 0.4, 0.8)
# ct = createRandomCT(1000, 0.2, 0.2, 0.8, 0.2, 1)
# ct = createRandomCT(10000, 0.2, 0.2, 0.8, 0.4, 0.8)
# rhoCT_c(ct, testSetBaserateInflation = 0, replicates = 800)
run_rho_in_c(threshold = 0.65, infl = 0.2, handsetLength = 20, handsetBaserate = 0.2, replicates = 1)
*/
