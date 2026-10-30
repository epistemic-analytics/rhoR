#pragma once
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>

// Functions that must reproduce R's arithmetic (rhoR parity) start their body
// with QESTATS_R_ARITHMETIC and carry QESTATS_R_ARITHMETIC_FN: R never fuses
// a * b + c, so these keep the compiler from contracting it into an FMA.
#if defined(__clang__)
#  define QESTATS_R_ARITHMETIC _Pragma("STDC FP_CONTRACT OFF")
#  define QESTATS_R_ARITHMETIC_FN
#elif defined(__GNUC__)
#  define QESTATS_R_ARITHMETIC
#  define QESTATS_R_ARITHMETIC_FN __attribute__((optimize("fp-contract=off")))
#else
#  define QESTATS_R_ARITHMETIC
#  define QESTATS_R_ARITHMETIC_FN
#endif

namespace qestats {

// Confusion matrix proportions (must sum to 1 after normalization).
struct ConfusionMatrix {
    double tp = 0, fp = 0, fn = 0, tn = 0;
};

// All derived classification metrics.
struct Metrics {
    // confusion matrix proportions
    double tp = 0, fp = 0, fn = 0, tn = 0;
    // derived
    double b1 = 0;        // classifier positive rate  (tp + fp)
    double b2 = 0;        // true positive rate        (tp + fn)
    double precision = 0; // tp / (tp + fp)
    double recall    = 0; // tp / (tp + fn)
    double accuracy  = 0; // tp + tn
    double fdr       = 0; // fp / (tp + fp)   — False Discovery Rate
    double for_      = 0; // fn / (fn + tn)   — False Omission Rate
    double kappa     = 0;
};

// Range result — min/max of a metric over the key variable interval.
struct MetricRange {
    double min = 0;
    double max = 0;
};

// Confidence interval from a hypergeometric estimate.
struct Interval {
    double lower = 0;
    double mean  = 0;
    double upper = 0;
};

// One row in the metrics_range output table.
struct MetricRangeRow {
    std::string metric;
    double lower = 0, mean = 0, upper = 0;
};

// ── Core arithmetic ───────────────────────────────────────────────────────────

// Cohen's kappa from confusion-matrix counts (or proportions), computed in
// the same order as rhoR's calcKappa() so the two agree to the bit: perfect
// agreement on one class (all TP or all TN) is 1.
QESTATS_R_ARITHMETIC_FN
inline double kappa_from_counts(double tp, double fp, double fn, double tn) {
    QESTATS_R_ARITHMETIC
    const double s = tp + fp + fn + tn;
    if (tp == s || tn == s) return 1.0;
    const double agreement = (tp + tn) / s;
    const double rate1 = (tp + fn) / s;   // first rater's positive rate
    const double rate2 = (tp + fp) / s;   // second rater's positive rate
    const double pe = rate1 * rate2 + (1 - rate1) * (1 - rate2);
    return (pe < 1.0) ? (agreement - pe) / (1 - pe) : 0.0;
}

// Rates are computed from the counts, not the normalized cells, so precision
// and recall match rhoR's TP / (TP + FP) and TP / (TP + FN) exactly.
inline Metrics metrics_from_cm(const ConfusionMatrix& cm) {
    const double s = cm.tp + cm.fp + cm.fn + cm.tn;
    if (s <= 0) throw std::domain_error("metrics_from_cm: confusion matrix is empty");
    Metrics m;
    m.tp = cm.tp / s; m.fp = cm.fp / s;
    m.fn = cm.fn / s; m.tn = cm.tn / s;
    const double pos1 = cm.tp + cm.fp, pos2 = cm.tp + cm.fn, neg1 = cm.fn + cm.tn;
    m.b1        = pos1 / s;
    m.b2        = pos2 / s;
    m.precision = (pos1 > 0) ? cm.tp / pos1 : 0.0;
    m.recall    = (pos2 > 0) ? cm.tp / pos2 : 0.0;
    m.accuracy  = (cm.tp + cm.tn) / s;
    m.fdr       = (pos1 > 0) ? cm.fp / pos1 : 0.0;
    m.for_      = (neg1 > 0) ? cm.fn / neg1 : 0.0;
    m.kappa     = kappa_from_counts(cm.tp, cm.fp, cm.fn, cm.tn);
    return m;
}

// Convenience: from raw counts (negative values clamped to 0).
inline Metrics calculate_metrics(double tp, double fp, double fn, double tn) {
    if (tp < 0) tp = 0; if (fp < 0) fp = 0;
    if (fn < 0) fn = 0; if (tn < 0) tn = 0;
    return metrics_from_cm({tp, fp, fn, tn});
}

// Retrieve a single metric value by name (supports alias names).
inline double get_metric(const Metrics& m, const std::string& name) {
    if (name == "b1" || name == "baserate")  return m.b1;
    if (name == "b2" || name == "baserate2") return m.b2;
    if (name == "precision")  return m.precision;
    if (name == "recall")     return m.recall;
    if (name == "accuracy")   return m.accuracy;
    if (name == "FDR")        return m.fdr;
    if (name == "FOR")        return m.for_;
    if (name == "kappa")      return m.kappa;
    if (name == "tp")         return m.tp;
    if (name == "fp")         return m.fp;
    if (name == "fn")         return m.fn;
    if (name == "tn")         return m.tn;
    throw std::invalid_argument("get_metric: unknown metric '" + name + "'");
}

} // namespace qestats
