// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
float distIt(double min, double max, int len, int distributionType = 1) {
  float kappaStep = ((max - min) / 99);
  colvec kappaDistribution = regspace(min, kappaStep, max);
  
  arma::vec kappaProbability;
  if(distributionType == 1) {
    kappaProbability = normpdf(kappaDistribution, 0.65, 0.1);
  }
  
  IntegerVector kappaRng = seq(0, kappaDistribution.size()-1);
  NumericVector kappaProbs = NumericVector(kappaProbability.begin(), kappaProbability.end());
  
  IntegerVector whKappa;
  if(kappaProbability.size() > 0) {
    Rcpp::Rcout << "Use some probs.." << std::endl;
    whKappa = sample(kappaRng, 1, false,  kappaProbs);
  } else {
    Rcpp::Rcout << "No probs for you..." << std::endl;
    whKappa = sample(kappaRng, 1, false);
  }
  float currKappa = kappaDistribution[whKappa[0]];
  
  return(currKappa);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
kappas2 = distIt(0.4, 0.65, 100)
*/
