#pragma once
#include "metrics.hpp"
#include <cmath>
#include <stdexcept>
#include <vector>

namespace qestats {

// ── Hypergeometric CDF ────────────────────────────────────────────────────────
// P(X <= q) where X ~ Hypergeometric(N, K, n)
//   K = number of successes in population
//   N - K = failures in population
//   n = sample size
//
// Computed in log-space to avoid overflow.  Equivalent to R's phyper(q,K,N-K,n).

inline double phyper(int q, int K, int N_minus_K, int n) {
    if (q < 0) return 0.0;
    const int N = K + N_minus_K;
    if (n > N || K < 0 || N_minus_K < 0) return 0.0;

    // log of C(a, b)
    auto lnchoose = [](int a, int b) -> double {
        if (b < 0 || b > a) return -1e300;
        if (b == 0 || b == a) return 0.0;
        return std::lgamma(a + 1) - std::lgamma(b + 1) - std::lgamma(a - b + 1);
    };

    const double log_denom = lnchoose(N, n);
    double sum = 0.0;
    const int hi = std::min(q, std::min(K, n));
    for (int k = std::max(0, n - N_minus_K); k <= hi; ++k) {
        double log_num = lnchoose(K, k) + lnchoose(N_minus_K, n - k);
        sum += std::exp(log_num - log_denom);
    }
    return std::min(sum, 1.0);
}

// ── Hypergeometric confidence interval for proportion ─────────────────────────
// Estimates CI for the proportion of positives in a population.
// R equivalent: hyper_range(n_pos, n, N, alpha)
//   n_pos = positives in sample
//   n     = sample size
//   N     = population size
//   alpha = significance level

inline Interval hyper_range(int n_pos, int n, int N, double alpha = 0.05) {
    if (n_pos < 0 || n_pos > n || n > N || alpha <= 0 || alpha >= 1)
        throw std::domain_error("hyper_range: invalid input parameters");

    const int n_neg = n - n_pos;
    int M_lower = n_neg;
    int M_upper = N - n_pos;

    // Shrink M_lower upward until phyper tail >= alpha/2
    {
        double p_lower = phyper(n_pos, N - M_lower, M_lower, n);
        if (p_lower < alpha / 2) {
            int left = M_lower, right = M_upper;
            while (right - left > 2) {
                M_lower = (left + right + 1) / 2;
                double p = phyper(n_pos - 1, N - M_lower, M_lower, n);
                if (p < alpha / 2) left = M_lower;
                else               right = M_lower;
            }
            M_lower = left;
        }
    }

    // Shrink M_upper downward until upper tail >= alpha/2
    {
        double p_upper = 1.0 - phyper(n_pos, N - M_upper, M_upper, n);
        if (p_upper < alpha / 2) {
            int left = M_lower, right = M_upper;
            while (right - left > 2) {
                M_upper = (left + right + 1) / 2;
                double p = 1.0 - phyper(n_pos - 1, N - M_upper, M_upper, n);
                if (p < alpha / 2) right = M_upper;
                else               left  = M_upper;
            }
            M_upper = right;
        }
    }

    return {
        1.0 - static_cast<double>(M_upper) / N,
        (n > 0) ? static_cast<double>(n_pos) / n : 0.0,
        1.0 - static_cast<double>(M_lower) / N
    };
}

// ── Perfect sampling: proportion ──────────────────────────────────────────────
// Minimum sample size n such that if all items in the sample satisfy the
// property, H0: x <= x0 is rejected at level alpha.
// Returns {n, power, cost}; n = -1 on invalid input.

struct PsResult {
    int    n     = -1;
    double power = 0;
    double cost  = 0;
};

inline PsResult ps_proportion(double x0, double x1 = 0,
                               int N = 0, int allowed_errors = 0,
                               double alpha = 0.05) {
    if (alpha < 0.001 || alpha > 0.1) return {};
    if (x0 < 0.5 || x0 >= 1.0)       return {};

    const int a = allowed_errors;
    if (N <= 0) N = 1000000;
    const int K = static_cast<int>(std::ceil(x0 * N));
    if (K == N) return {N, 1.0, 1.0};
    if (a >= static_cast<int>(K * (1.0 - x0))) return {};

    // Binary search for min n: P(X >= n - a | K, N-K, n) <= alpha
    // i.e.  1 - phyper(n-1-a, K, N-K, n) <= alpha
    int left = a, right = K;
    double p = 1.0;
    while (left < right - 1) {
        int n = (left + right + 1) / 2;
        p = 1.0 - phyper(n - 1 - a, K, N - K, n);
        if (p > alpha) left  = n;
        else           right = n;
    }
    const int n_out = right;

    if (x1 <= x0 || x1 >= 1.0) x1 = (x0 + 1.0) / 2.0;
    const double K1 = x1 * N;
    const int    K1i = static_cast<int>(K1);
    const double power = 1.0 - phyper(n_out - 1 - a, K1i, N - K1i, n_out);
    return {n_out, power, static_cast<double>(n_out) / power};
}

// ── Perfect sampling: kappa ───────────────────────────────────────────────────
// Derives the maximum achievable accuracy for (baserate, kappa) via range(),
// then delegates to ps_proportion.
// Declared here; implemented after derivation.hpp is included in qestats.hpp.
// Forward declaration only — see qestats.hpp for the full definition.

// ── ps_est_proportion ────────────────────────────────────────────────────────
// Given a perfect sample of size n, estimate the minimum proportion of
// positives in the population.

inline double ps_est_proportion(int n, int N = 0, int allowed_errors = 0,
                                 double alpha = 0.05) {
    if (alpha < 0.001 || alpha > 0.1) return -1;
    const int a = allowed_errors;
    if (a >= n) return -1;
    if (N <= 0) N = 1000000;
    if (n < 1 || n >= N) return -1;

    int left = 1, right = N;
    double x = 0;
    while (right - left > 1) {
        int K = (left + right + 1) / 2;
        double p = 1.0 - phyper(n - 1 - a, K, N - K, n);
        x = static_cast<double>(K) / N;
        if (p < alpha) left  = K;
        else           right = K;
    }
    return x;
}

// ── ps_est_kappa ──────────────────────────────────────────────────────────────
// Given a perfect sample of size n, estimate the minimum kappa achievable.
// Declared here; depends on range() from derivation.hpp — implemented in
// comparison.hpp after derivation.hpp is included.
// Forward-declared below; full definition is in comparison.hpp.

// ── metrics_range ─────────────────────────────────────────────────────────────
// Estimates CI for precision, recall, accuracy, FDR, FOR, and kappa given a
// sampled confusion matrix (raw counts) and population size N.
// Returns a vector of MetricRangeRow in the same order as R's metrics_range().

inline std::vector<MetricRangeRow>
metrics_range(double tp, double fp, double fn, double tn,
              int N, double baserate, double alpha = 0.05) {
    // Helper: derive metrics from (baserate, FDR, FOR)
    auto metrics_from_bFF = [](double b, double FDR, double FOR) -> Metrics {
        const double fp_ = FDR * b;
        const double tp_ = b - fp_;
        const double fn_ = FOR * (1.0 - b);
        const double tn_ = 1.0 - fp_ - tp_ - fn_;
        return metrics_from_cm({tp_, fp_, fn_, tn_});
    };

    // Precision CI via hyper_range (full alpha)
    Interval prec = hyper_range(static_cast<int>(tp),
                                static_cast<int>(tp + fp), N, alpha);
    Interval FDR_ci = {1.0 - prec.upper, 1.0 - prec.mean, 1.0 - prec.lower};

    // FOR CI: tn / (fn + tn) via hyper_range, then invert
    Interval one_minus_FOR = hyper_range(static_cast<int>(tn),
                                         static_cast<int>(fn + tn), N, alpha);
    Interval FOR_ci = {1.0 - one_minus_FOR.upper,
                       1.0 - one_minus_FOR.mean,
                       1.0 - one_minus_FOR.lower};

    // Derive recall, accuracy, kappa at lower/mean/upper FDR+FOR extremes
    Metrics lower_m = metrics_from_bFF(baserate, FDR_ci.upper, FOR_ci.upper);
    Metrics mean_m  = metrics_from_bFF(baserate, FDR_ci.mean,  FOR_ci.mean);
    Metrics upper_m = metrics_from_bFF(baserate, FDR_ci.lower, FOR_ci.lower);

    return {
        {"precision", prec.lower,        prec.mean,        prec.upper},
        {"recall",    lower_m.recall,    mean_m.recall,    upper_m.recall},
        {"accuracy",  lower_m.accuracy,  mean_m.accuracy,  upper_m.accuracy},
        {"FDR",       FDR_ci.lower,      FDR_ci.mean,      FDR_ci.upper},
        {"FOR",       FOR_ci.lower,      FOR_ci.mean,      FOR_ci.upper},
        {"kappa",     lower_m.kappa,     mean_m.kappa,     upper_m.kappa},
    };
}

} // namespace qestats
