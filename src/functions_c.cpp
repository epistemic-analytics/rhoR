#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
bool checkBRPKcombo_c(double BR, double P, double K) {
  double right = (2*BR*K - 2*BR - K)/(K - 2);
  if((P > right)){// & (getR_c(K, BR, P) < 1)
    //Rcpp::Rcout<<"R works: " << getR_c(K,BR,P)<<std::endl;
    return true;
  }else{
    return false;
  }
}

// [[Rcpp::export]]
Rcpp::List genPK_c(NumericVector kappaDistribution, NumericVector precisionDistribution, double baserate) {
  //double currKappa = RcppArmadillo::sample(kappaDistribution, 1, 0)[0]; //sample(kappaDistribution, 1, replace = FALSE, prob = kappaProbability)
  //double currPrecision = RcppArmadillo::sample(precisionDistribution, 1, 0)[0]; //sample(precisionDistribution, 1, replace = FALSE, prob = precisionProbability)
  double currKappa = kappaDistribution[floor(runif(1, 0, kappaDistribution.size() - 1)[0])];
  double currPrecision = precisionDistribution[floor(runif(1, 0 , precisionDistribution.size() - 1)[0])];
  //Rcpp::Rcout<<precisionDistribution.size()<<std::endl;
  
  if(!checkBRPKcombo_c(baserate, currPrecision, currKappa)){
    double precisionMin = (2*baserate*currKappa - 2*baserate - currKappa)/(currKappa - 2);
    LogicalVector indicies = precisionDistribution > precisionMin;
    NumericVector newPrecisionDistribution = precisionDistribution[indicies];
    //precisionProbability = precisionProbability[indicies];
    if(newPrecisionDistribution.size() == 0) {
      genPK_c(kappaDistribution, precisionDistribution, baserate);
    }
    currPrecision = newPrecisionDistribution[floor(runif(1, 0, newPrecisionDistribution.size() - 1)[0])]; //sample(precisionDistribution, 1, replace = FALSE, prob = precisionProbability)
  }
  //Rcpp::Rcout << "kappa" << currKappa << "precision " << currPrecision<<std::endl;
  return(Rcpp::List::create(Rcpp::Named("precision") = currPrecision, Rcpp::Named("kappa") = currKappa));
}

// [[Rcpp::export]]
int getFP(int gold0s, int TP, double precision) {
  int max1 = max(NumericVector::create(1, round(TP * (1 - precision)/precision)));
   return min(NumericVector::create(gold0s, max1));
}

// [[Rcpp::export]]
double getR_c(double K, double BR, double P){
  return ((K*P) / (2*P-2*BR-K+2*BR*K));
}