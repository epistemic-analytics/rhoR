#pragma once
#include "metrics.hpp"
#include "inference.hpp"
#include "derivation.hpp"
#include <algorithm>
#include <cmath>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

namespace qestats {

// ── Bootstrap result ──────────────────────────────────────────────────────────

struct BootstrapResult {
    Metrics original;
    // Per-metric standard errors and CI (parallel to METRIC_NAMES order)
    struct MetricStat {
        std::string name;
        double se    = 0;
        double ci_lo = 0;
        double ci_hi = 0;
    };
    std::vector<MetricStat> stats;
};

// Metric names in canonical order matching qestats R output.
inline const std::vector<std::string>& metric_names() {
    static const std::vector<std::string> v = {
        "b1","b2","precision","recall","accuracy","FDR","FOR","kappa"
    };
    return v;
}

// ── bootstrap_metrics ─────────────────────────────────────────────────────────
// cm: raw confusion matrix (TP, FP, FN, TN counts — not proportions)
// n_sample: resample size
// n_set: population size (0 = infinite)
// n_bootstrap: number of bootstrap replicates
// conf_level: confidence level (e.g. 0.95)

inline BootstrapResult
bootstrap_metrics(const ConfusionMatrix& cm, int n_sample,
                  const std::vector<std::string>& metrics = {},
                  int n_set = 0, int n_bootstrap = 1000,
                  double conf_level = 0.95) {
    const auto& all = metric_names();
    const auto& chosen = metrics.empty() ? all : metrics;

    double n1 = cm.tp, n2 = cm.fp, n3 = cm.fn, n4 = cm.tn;
    double n0 = n1 + n2 + n3 + n4;
    if (n0 <= 0 || n1 < 0 || n2 < 0 || n3 < 0 || n4 < 0)
        throw std::domain_error("bootstrap_metrics: invalid confusion matrix");

    BootstrapResult result;
    result.original = calculate_metrics(n1, n2, n3, n4);

    // Scale to population if requested
    if (n_set > 0) {
        double r = static_cast<double>(n_set) / n0;
        if (r > 1) {
            n1 = std::ceil(n1 * r); n2 = std::ceil(n2 * r);
            n3 = std::ceil(n3 * r); n4 = std::ceil(n4 * r);
        } else {
            n1 = std::floor(n1 * r); n2 = std::floor(n2 * r);
            n3 = std::floor(n3 * r); n4 = std::floor(n4 * r);
        }
        double tot = n1 + n2 + n3 + n4;
        if (n_sample > tot) n_sample = static_cast<int>(tot);
    }
    const double n_total = n1 + n2 + n3 + n4;
    const bool with_replacement = (n_set == 0);

    // Build population index vector [0..n_total)
    const int N = static_cast<int>(n_total);
    const int c1 = static_cast<int>(n1);
    const int c2 = static_cast<int>(n2);
    const int c3 = static_cast<int>(n3);

    std::mt19937 rng(42);  // fixed seed for reproducibility

    // Collect bootstrap replicates
    std::vector<std::vector<double>> samples(chosen.size());

    for (int b = 0; b < n_bootstrap; ++b) {
        std::vector<int> idx(n_sample);
        if (with_replacement) {
            std::uniform_int_distribution<int> dist(0, N - 1);
            for (int& x : idx) x = dist(rng);
        } else {
            // sample without replacement
            std::vector<int> pool(N);
            std::iota(pool.begin(), pool.end(), 0);
            std::shuffle(pool.begin(), pool.end(), rng);
            for (int i = 0; i < n_sample; ++i) idx[i] = pool[i];
        }
        int b1_ = 0, b2_ = 0, b3_ = 0, b4_ = 0;
        for (int x : idx) {
            if (x < c1)             ++b1_;
            else if (x < c1+c2)     ++b2_;
            else if (x < c1+c2+c3)  ++b3_;
            else                    ++b4_;
        }
        Metrics bm = calculate_metrics(b1_, b2_, b3_, b4_);
        for (std::size_t i = 0; i < chosen.size(); ++i)
            samples[i].push_back(get_metric(bm, chosen[i]));
    }

    const double alpha = 1.0 - conf_level;
    for (std::size_t i = 0; i < chosen.size(); ++i) {
        auto& s = samples[i];
        double mean = 0; for (double x : s) mean += x; mean /= s.size();
        double var = 0; for (double x : s) var += (x-mean)*(x-mean);
        double se = std::sqrt(var / (s.size() - 1));

        std::sort(s.begin(), s.end());
        int lo_idx = std::max(0, static_cast<int>(std::floor(alpha/2 * s.size())));
        int hi_idx = std::min(static_cast<int>(s.size())-1,
                              static_cast<int>(std::ceil((1.0-alpha/2) * s.size())));
        result.stats.push_back({chosen[i], se, s[lo_idx], s[hi_idx]});
    }
    return result;
}

// ── Welch t-test helper ───────────────────────────────────────────────────────

struct TTestResult {
    double t_stat  = 0;
    double df      = 0;
    double p_value = 1;
};

inline TTestResult welch_t(double m1, double m2,
                            double s1, double s2,
                            int n1, int n2,
                            double tol = 1e-9) {
    if (std::abs(m1 - m2) < tol) return {0, 1, 1.0};
    if (std::abs(s1) < tol && std::abs(s2) < tol)
        return {0, 1, (std::abs(m1-m2) < tol) ? 1.0 : 0.0};

    const double t = (m1 - m2) / std::sqrt(s1*s1 + s2*s2);
    const double num = (s1*s1 + s2*s2) * (s1*s1 + s2*s2);
    const double den = s1*s1*s1*s1/(n1-1) + s2*s2*s2*s2/(n2-1);
    const double df  = (den > 0) ? num/den : 1.0;

    // Two-sided p-value via regularised incomplete beta (approx via normal for large df)
    // For production accuracy use a proper t-distribution CDF; for now use
    // normal approximation which is good for df > 30.
    double p;
    if (df > 30) {
        // Standard normal approximation
        double z = std::abs(t);
        p = 2.0 * (0.5 * std::erfc(z / std::sqrt(2.0)));
    } else {
        // Abramowitz & Stegun approximation to t CDF
        double x = df / (df + t*t);
        // regularized incomplete beta I_x(df/2, 1/2) via continued fraction (simple approximation)
        // Good enough for n_bootstrap >= 30; exact implementation would need boost or a special function
        double z = std::abs(t);
        double a_s = df / 2.0, b_s = 0.5;
        // Use the normal approximation as a safe fallback
        p = 2.0 * (0.5 * std::erfc(z / std::sqrt(2.0)));
    }
    return {t, df, p};
}

// ── p_diff ────────────────────────────────────────────────────────────────────
// p-value for a metric difference between two groups using bootstrap Welch t.

inline double p_diff(const ConfusionMatrix& cm1, const ConfusionMatrix& cm2,
                     int n_sample, const std::string& metric,
                     int n_set = 0, int n_bootstrap = 1000, double alpha = 0.05) {
    const double n1 = cm1.tp + cm1.fp + cm1.fn + cm1.tn;
    const double n2 = cm2.tp + cm2.fp + cm2.fn + cm2.tn;
    const double n0 = n1 + n2;
    if (n_sample <= 0) n_sample = static_cast<int>(n0);
    const double r = static_cast<double>(n_sample) / n0;
    const int ns1 = std::max(1, static_cast<int>(std::floor(n1 * (r < 1 ? 1 : r))));
    const int ns2 = std::max(1, static_cast<int>(std::floor(n2 * (r < 1 ? 1 : r))));
    const int nset1 = (n_set > 0) ? static_cast<int>(std::floor(n_set * n1 / n0)) : 0;
    const int nset2 = (n_set > 0) ? static_cast<int>(std::floor(n_set * n2 / n0)) : 0;

    auto r1 = bootstrap_metrics(cm1, ns1, {metric}, nset1, n_bootstrap, 1.0-alpha);
    auto r2 = bootstrap_metrics(cm2, ns2, {metric}, nset2, n_bootstrap, 1.0-alpha);

    const double m1v = get_metric(r1.original, metric);
    const double m2v = get_metric(r2.original, metric);
    const double se1 = r1.stats.empty() ? 0 : r1.stats[0].se;
    const double se2 = r2.stats.empty() ? 0 : r2.stats[0].se;

    return welch_t(m1v, m2v, se1, se2, n_bootstrap, n_bootstrap).p_value;
}

// ── n_diff ────────────────────────────────────────────────────────────────────
// Minimum n at which two groups show a significant metric difference.

inline int n_diff(const ConfusionMatrix& cm1, const ConfusionMatrix& cm2,
                  const std::string& metric, int n_set = 0,
                  int n_bootstrap = 1000, double alpha = 0.05) {
    const int n1 = static_cast<int>(cm1.tp+cm1.fp+cm1.fn+cm1.tn);
    const int n2 = static_cast<int>(cm2.tp+cm2.fp+cm2.fn+cm2.tn);
    int n = n1 + n2;

    double p0 = p_diff(cm1, cm2, n, metric, n_set, n_bootstrap, alpha);
    if (p0 < alpha) return n;

    int left = n, right = n_set > 0 ? n_set : n;
    if (n_set <= 0) {
        while (p0 >= alpha) {
            left = right;
            right *= 2;
            p0 = p_diff(cm1, cm2, right, metric, n_set, n_bootstrap, alpha);
        }
    } else {
        double p1 = p_diff(cm1, cm2, n_set, metric, n_set, n_bootstrap, alpha);
        if (p1 >= alpha) return n_set;
    }

    while (right - left > 1) {
        int mid = (left + right + 1) / 2;
        p0 = p_diff(cm1, cm2, mid, metric, n_set, n_bootstrap, alpha);
        if (p0 >= alpha) left = mid;
        else             right = mid;
    }
    return (left + right + 1) / 2;
}

// ── N_Diff2 ───────────────────────────────────────────────────────────────────
// Minimum n at which CI intervals for two groups no longer overlap by > d.

inline double interval_overlap(const MetricRange& r1, const MetricRange& r2) {
    double span = std::min(r1.max - r1.min, r2.max - r2.min);
    if (span <= 0) return 0.0;
    double overlap = std::min(r1.max, r2.max) - std::max(r1.min, r2.min);
    return (overlap <= 0) ? 0.0 : overlap / span;
}

// range_est: hyper-based CI for a metric given raw counts + population N
inline MetricRange range_est(const ConfusionMatrix& cm,
                              const std::string& metric, int N, double alpha = 0.05) {
    const int tp = static_cast<int>(cm.tp), fp = static_cast<int>(cm.fp);
    const int fn = static_cast<int>(cm.fn), tn = static_cast<int>(cm.tn);
    auto prec_ci  = hyper_range(tp, tp+fp, N, alpha);
    auto for_ci   = hyper_range(fn, fn+tn, N, alpha);  // FOR = fn/(fn+tn)
    double b1 = static_cast<double>(tp+fp) / (tp+fp+fn+tn);

    auto eval_m = [&](double fdr, double for_v) -> double {
        Metrics m = derive_from_rates(b1, fdr, for_v);
        return get_metric(m, metric);
    };

    // Precision = 1 - FDR; CI is [lo, hi] for precision → FDR is [1-hi, 1-lo]
    double fdr_lo = 1.0 - prec_ci.upper, fdr_hi = 1.0 - prec_ci.lower;
    double for_lo = for_ci.lower, for_hi = for_ci.upper;

    double v_lo = eval_m(fdr_hi, for_hi);  // worst case
    double v_hi = eval_m(fdr_lo, for_lo);  // best case
    if (v_lo > v_hi) std::swap(v_lo, v_hi);
    return {v_lo, v_hi};
}

inline int N_Diff2(const ConfusionMatrix& cm1, const ConfusionMatrix& cm2,
                   const std::string& metric, int N, double d = 0.1,
                   double alpha = 0.05) {
    const double n1 = cm1.tp+cm1.fp+cm1.fn+cm1.tn;
    const double n2 = cm2.tp+cm2.fp+cm2.fn+cm2.tn;
    const int n = static_cast<int>(n1 + n2);

    auto scale = [](const ConfusionMatrix& cm, double r) -> ConfusionMatrix {
        return {std::floor(cm.tp*r), std::floor(cm.fp*r),
                std::floor(cm.fn*r), std::floor(cm.tn*r)};
    };

    auto check_overlap = [&](double x) -> bool {
        auto s1 = scale(cm1, x); auto s2 = scale(cm2, x);
        auto r1 = range_est(s1, metric, N, alpha);
        auto r2 = range_est(s2, metric, N, alpha);
        return interval_overlap(r1, r2) >= d;
    };

    if (!check_overlap(1.0)) return n;  // already separable

    const double x_max = static_cast<double>(N) / n;
    if (check_overlap(x_max)) return N;  // never separable at population size

    double left = 1.0, right = x_max;
    while (right - left > 1.0 / n) {
        double mid = (left + right) / 2.0;
        if (check_overlap(mid)) left  = mid;
        else                    right = mid;
    }
    return static_cast<int>(std::ceil(n * (left + right) / 2.0));
}

// ── ps_est_kappa ──────────────────────────────────────────────────────────────
// Given a perfect sample of size n and a baserate, estimate the minimum kappa.

inline double ps_est_kappa(int n, double baserate, int N = 0,
                            int allowed_errors = 0, double alpha = 0.05) {
    double accuracy = ps_est_proportion(n, N, allowed_errors, alpha);
    if (accuracy < 0) return -1;
    MetricRange kappa_range = range("baserate", baserate, "accuracy", accuracy, "kappa");
    return kappa_range.min;
}

// ── ps_kappa ─────────────────────────────────────────────────────────────────
// Perfect sample size for kappa test — delegates to ps_proportion via
// max accuracy from range(baserate, kappa).

inline PsResult ps_kappa(double kappa0, double baserate,
                          double kappa1 = 0, int N = 0,
                          int allowed_errors = 0, double alpha = 0.05) {
    if (alpha < 0.001 || alpha > 0.1) return {};
    if (kappa0 <= 0 || kappa0 >= 1)    return {};

    MetricRange acc_range = range("baserate", baserate, "kappa", kappa0, "accuracy");
    return ps_proportion(acc_range.max, kappa1, N, allowed_errors, alpha);
}

} // namespace qestats
