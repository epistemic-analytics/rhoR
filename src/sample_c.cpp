// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;

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