// rhoR's C++ functions, on libqe-stats: rho and its building blocks are
// qestats::rho_fast (this file's functions, as they were) and
// qestats::rho_standard (rhoK's default method), vendored in
// inst/include/qestats/ by tools/sync-qestats.sh. Given R's RNG they return
// exactly what rhoR returned before the move (tests/testthat/test-regression-pins.R).
//
// Only the bell-shaped kappa distribution (generate_kp_list(distributionType = 1))
// is still computed here, with RcppArmadillo's weighted sample().

#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "qestats/rho.hpp"
using namespace Rcpp;

namespace {

// R's RNG for qestats: unif_rand() for the fast method, R_unif_index() (what
// sample.int() draws with) for the standard one.
struct RRng {
  double unif_rand() { return ::unif_rand(); }
  double unif_index(double n) { return R_unif_index(n); }
};

// rhoR's tables are gold (rows) by silver (columns): [1,1] TP, [1,2] FN,
// [2,1] FP, [2,2] TN.
qestats::ConfusionMatrix to_cm(const IntegerMatrix& ct) {
  return {double(ct(0, 0)), double(ct(1, 0)), double(ct(0, 1)), double(ct(1, 1))};
}

IntegerMatrix to_ct(const qestats::ConfusionMatrix& cm) {
  IntegerMatrix ct(2, 2);
  ct(0, 0) = cm.tp;
  ct(0, 1) = cm.fn;
  ct(1, 0) = cm.fp;
  ct(1, 1) = cm.tn;
  return ct;
}

std::vector<qestats::KappaPrecision> to_kps(const NumericMatrix& KPs) {
  std::vector<qestats::KappaPrecision> kps(KPs.nrow());
  for (int i = 0; i < KPs.nrow(); i++) kps[i] = {KPs(i, 1), KPs(i, 0)};
  return kps;
}

NumericMatrix from_kps(const std::vector<qestats::KappaPrecision>& kps) {
  NumericMatrix KPs(kps.size(), 2);
  for (std::size_t i = 0; i < kps.size(); i++) {
    KPs(i, 0) = kps[i].precision;
    KPs(i, 1) = kps[i].kappa;
  }
  return KPs;
}

// find_valid_pk() with kappa probabilities (the bell-shaped distribution).
// Precision is drawn unweighted, as before.
NumericVector find_valid_pk_weighted(
  arma::colvec kappaDistribution, arma::vec kappaProbability,
  arma::colvec precisionDistribution, double baserate
) {
  for (int attempt = 0; attempt < qestats::RHO_MAX_ATTEMPTS; attempt++) {
    arma::ivec kappaRng = arma::regspace<arma::ivec>(0, kappaDistribution.size() - 1);
    arma::ivec whKappa = Rcpp::RcppArmadillo::sample(kappaRng, 1, false, kappaProbability);
    double currKappa = kappaDistribution[whKappa.at(0)];

    arma::ivec precRng = arma::regspace<arma::ivec>(0, precisionDistribution.size() - 1);
    arma::ivec whPrec = Rcpp::RcppArmadillo::sample(precRng, 1, false);
    double currPrec = precisionDistribution[whPrec.at(0)];

    if (!qestats::rho_fast::valid_combo(baserate, currPrec, currKappa)) {
      double precisionMin = (2 * baserate * currKappa - 2 * baserate - currKappa) / (currKappa - 2);
      arma::uvec indicies = arma::find(precisionDistribution > precisionMin);
      if (indicies.size() == 0) continue;
      arma::colvec above = precisionDistribution.elem(indicies);
      precRng = arma::regspace<arma::ivec>(0, above.size() - 1);
      whPrec = Rcpp::RcppArmadillo::sample(precRng, 1, false);
      currPrec = above[whPrec.at(0)];
    }
    return NumericVector::create(currPrec, currKappa);
  }
  stop(qestats::RHO_NO_VALID_SET);
}

} // namespace

//' @title sample_contingency_table
//' @name sample_contingency_table
//'
//' @param xx contingency table matrix
//' @param n int size of the contingency table
//' @param forR bool if true, add 1 to the results accounting for R indices starting at 1
//'
//' @export
// [[Rcpp::export]]
IntegerMatrix sample_contingency_table(IntegerMatrix xx, int n, bool forR = true) {
  RRng rng;
  std::vector<int> cells = qestats::rho_fast::sample_contingency_table(
    std::vector<int>(xx.begin(), xx.end()), n, rng);
  IntegerMatrix ret(cells.size(), 1);
  for (std::size_t w = 0; w < cells.size(); w++) ret[w] = cells[w] + (forR ? 1 : 0);
  return ret;
}

//' @name getBootPvalue_c
//' @title getBootPvalue_c
//'
//' @param distribution vector of calculated kappas
//' @param result double calculated kappa to compare against
//'
//' @description
//' returns the percentage of the time that the distribution was greater or equal to the observed kappa
//' if the result is less than the mean of the distribution, than the p value is 1
//' else return the number of times that the distribution is greater than the result as a percentage of the total number
//' of items in the distribution
//'
//' @return double calculated p-value
//'
//' @export
// [[Rcpp::export]]
double getBootPvalue_c(NumericVector distribution, double result) {
  return qestats::rho_fast::boot_pvalue(
    std::vector<double>(distribution.begin(), distribution.end()), result);
}

// Validate a Baserate/Kappa combo against the supplied Precision.
// Return true if Precision is greater than the calculated right value
// [[Rcpp::export]]
bool check_BRK_combo(double BR, double P, double K) {
  return qestats::rho_fast::valid_combo(BR, P, K);
}

//' @name recall
//' @title recall
//'
//' @param kappa double
//' @param BR double
//' @param P double
//'
//' @return Recall calculated from provided kappa, BR, and P
//'
//' @export
// [[Rcpp::export]]
double recall(double kappa, double BR, double P) {
  return qestats::rho_fast::recall(kappa, BR, P);
}

// Find a valid precision and kappa combo from a distribution of Kappas and Precisions given a baserate
//
// [[Rcpp::export]]
NumericVector find_valid_pk(
  NumericVector kappaDistribution, NumericVector kappaProbability,
  NumericVector precisionDistribution, NumericVector precisionProbability,
  double baserate
) {
  if (kappaDistribution.size() == 0 || precisionDistribution.size() == 0) {
    stop("find_valid_pk: empty kappa or precision distribution");
  }
  if (kappaProbability.size() > 0) {
    return find_valid_pk_weighted(as<arma::colvec>(kappaDistribution), as<arma::vec>(kappaProbability),
                                  as<arma::colvec>(precisionDistribution), baserate);
  }
  RRng rng;
  qestats::KappaPrecision kp = qestats::rho_fast::kappa_precision(
    std::vector<double>(kappaDistribution.begin(), kappaDistribution.end()),
    std::vector<double>(precisionDistribution.begin(), precisionDistribution.end()),
    baserate, rng);
  return NumericVector::create(kp.precision, kp.kappa);
}


//' @name generateKPs_c
//' @title generate_kp_list
//'
//' @param numNeeded int
//' @param baserate double
//' @param kappaMin double
//' @param kappaMax double
//' @param precisionMin double
//' @param precisionMax double
//' @param distributionType int 0 - normal (default), 1 - bell
//' @param distributionLength long
//'
//' @return matrix of kappa and precision values (column 1 as precision)
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix generate_kp_list(
    int numNeeded, double baserate,
    double kappaMin, double kappaMax,
    double precisionMin, double precisionMax,
    int distributionType = 0, long distributionLength = 10000
) {
  if (distributionType != 1) {
    RRng rng;
    return from_kps(qestats::rho_fast::generate_kps(
      numNeeded, baserate, kappaMin, kappaMax, precisionMin, precisionMax, rng, distributionLength));
  }

  double kappaStep = ((kappaMax - kappaMin) / (distributionLength - 1));
  arma::colvec kappaDistribution = arma::regspace(kappaMin, kappaStep, kappaMax);
  arma::vec kappaProbability = arma::normpdf(kappaDistribution, 0.9, 0.1);
  double precStep = (precisionMax - precisionMin) / (10000 - 1);
  arma::colvec precisionDistribution = arma::regspace(precisionMin, precStep, precisionMax);

  Rcpp::NumericMatrix KPs(numNeeded, 2);
  for (int i = 0; i < numNeeded; i++) {
    KPs(i, _) = find_valid_pk_weighted(kappaDistribution, kappaProbability, precisionDistribution, baserate);
  }
  return KPs;
}

//' @name contingency_table
//' @title contingency_table
//' @description Create a contingency table using the provied precision, recall, baserate, and length.
//'
//' @param precision double
//' @param rec double
//' @param length int
//' @param baserate double
//'
//' @export
// [[Rcpp::export]]
IntegerMatrix contingency_table(double precision, double rec, int length, double baserate) {
  return to_ct(qestats::rho_fast::contingency_table(precision, rec, length, baserate));
}

//' @name random_contingency_table
//' @title random_contingency_table
//' @param setLength [TBD]
//' @param baserate [TBD]
//' @param kappaMin [TBD]
//' @param kappaMax [TBD]
//' @param minPrecision [TBD]
//' @param maxPrecision [TBD]
//'
//' @export
// [[Rcpp::export]]
IntegerMatrix random_contingency_table(
    int setLength,double baserate,
    double kappaMin, double kappaMax,
    double minPrecision = 0, double maxPrecision = 1
) {
  RRng rng;
  qestats::KappaPrecision kp = qestats::rho_fast::generate_kps(
    1, baserate, kappaMin, kappaMax, minPrecision, maxPrecision, rng)[0];
  double rec = qestats::rho_fast::recall(kp.kappa, baserate, kp.precision);
  return to_ct(qestats::rho_fast::contingency_table(kp.precision, rec, setLength, baserate));
}

//' @title kappa_ct
//' @description Calculate kappa from a contingency table
//' @param ct [TBD]
//'
//' @export
// [[Rcpp::export]]
double kappa_ct(IntegerMatrix ct) {
  return qestats::rho_fast::kappa_ct(to_cm(ct));
}

// [[Rcpp::export]]
IntegerMatrix getHand_ct(IntegerMatrix ct, int handSetLength, double handSetBaserate) {
  RRng rng;
  return to_ct(qestats::rho_fast::hand_contingency_table(to_cm(ct), handSetLength, handSetBaserate, rng));
}

//' @name getHand_kappa
//' @title getHand_kappa
//'
//' @description This function returns kappa calculated from a Handset taken from  a larger Contingency Table
//'
//' @param ct KPs matrix of kappa (column 1) and precision (column 2) values
//' @param handSetLength The length of the \code{\link[=getTestSet]{testSet}} (ignored unless \emph{data} is an observed kappa value)
//' @param handSetBaserate baserate to inflate the sampled contingency table to
//'
//' @return Kappa as double
//'
//' @export
// [[Rcpp::export]]
double getHand_kappa(IntegerMatrix ct, int handSetLength, double handSetBaserate) {
  RRng rng;
  return qestats::rho_fast::kappa_ct(
    qestats::rho_fast::hand_contingency_table(to_cm(ct), handSetLength, handSetBaserate, rng));
}

// This function calculates rho given the parameters of the initial set and
// the kappa of the observed handset and is called by rhoK()
//
// [[Rcpp::export]]
double calcRho_c(
  double x,
  double OcSBaserate,
  int testSetLength,
  double testSetBaserateInflation = 0,
  int OcSLength = 10000,
  int replicates = 800,
  double ScSKappaThreshold = 0.9,
  double ScSKappaMin = 0.40,
  double ScSPrecisionMin = 0.6,
  double ScSPrecisionMax = 1.0,
  NumericMatrix KPs = NumericMatrix(0)
) {
  RRng rng;
  qestats::RhoOptions opts;
  opts.test_set_baserate_inflation = testSetBaserateInflation;
  opts.ocs_length = OcSLength;
  opts.replicates = replicates;
  std::vector<qestats::KappaPrecision> kps;
  if (KPs.size() == 1 && KPs[0] == 0) {
    kps = qestats::rho_fast::generate_kps(replicates, OcSBaserate, ScSKappaMin, ScSKappaThreshold,
                                          ScSPrecisionMin, ScSPrecisionMax, rng);
  } else {
    kps = to_kps(KPs);
  }
  return qestats::rho_fast::rho(x, OcSBaserate, testSetLength, kps, opts, rng);
}

// calcRho()'s computation (rhoK's default method): qestats::rho_standard.
// KPs, when given, is a matrix of precision (column 1) and kappa (column 2).
// [[Rcpp::export]]
double calcRho_standard_c(
  double x,
  double OcSBaserate,
  int testSetLength,
  double testSetBaserateInflation,
  int OcSLength,
  int replicates,
  double ScSKappaThreshold,
  double ScSKappaMin,
  double ScSPrecisionMin,
  double ScSPrecisionMax,
  Nullable<NumericMatrix> KPs = R_NilValue
) {
  RRng rng;
  qestats::RhoOptions opts;
  opts.test_set_baserate_inflation = testSetBaserateInflation;
  opts.ocs_length = OcSLength;
  opts.replicates = replicates;
  std::vector<qestats::KappaPrecision> kps;
  if (KPs.isNull()) {
    kps = qestats::rho_standard::generate_kps(replicates, OcSBaserate, ScSKappaMin, ScSKappaThreshold,
                                              ScSPrecisionMin, ScSPrecisionMax, rng);
  } else {
    kps = to_kps(NumericMatrix(KPs.get()));
  }
  return qestats::rho_standard::rho(x, OcSBaserate, testSetLength, kps, opts, rng);
}
