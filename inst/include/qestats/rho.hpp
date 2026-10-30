#pragma once
#include "metrics.hpp"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

// Rho (Shaffer's rho): how likely a test set's kappa this high would be if the
// coding process's population kappa were below a threshold. A port of rhoR:
// given an adapter over R's RNG, a seeded rho() returns exactly what rhoR does.
//
// rhoR computes rho two ways, and both are kept, quirks included:
//
//   RhoMethod::Standard   rhoR's default path, written in R (calcRho): grids
//                         from seq(), draws from sample.int(), the hand set
//                         sampled as rows of the full set. qestats::rho_standard.
//   RhoMethod::Fast       rhoR's C++ path (calcRho_c, any other `method`):
//                         Armadillo regspace() grids, RcppArmadillo sample() /
//                         randi() draws, the hand set sampled from the
//                         contingency table. qestats::rho_fast.
//
// Random numbers come from an Rng: any type with
//
//   double unif_rand();          // uniform on (0, 1)          — R's unif_rand()
//   double unif_index(double n); // uniform integer in [0, n)  — R's R_unif_index()
//
// The Standard method draws only through unif_index, the Fast method only
// through unif_rand. StdRng is a standalone default.

namespace qestats {

// mt19937_64 with the draws derived by hand (the std distributions differ
// between standard libraries), so a seed gives the same rho everywhere.
struct StdRng {
    std::mt19937_64 gen;
    explicit StdRng(std::uint64_t seed = 5489u) : gen(seed) {}
    double unif_rand() {
        double u;
        do { u = static_cast<double>(gen() >> 11) * 0x1.0p-53; } while (u <= 0.0);
        return u;
    }
    // Rejection on the top bits, like R_unif_index.
    double unif_index(double n) {
        const auto m = static_cast<std::uint64_t>(n);
        if (m <= 1) return 0;
        int bits = 0;
        while (bits < 64 && (std::uint64_t(1) << bits) < m) ++bits;
        std::uint64_t v;
        do { v = gen() >> (64 - bits); } while (v >= m);
        return static_cast<double>(v);
    }
};

enum class RhoMethod { Standard, Fast };

// rhoR's rhoK() arguments after the observed kappa, base rate and test set
// length, with the same defaults.
struct RhoOptions {
    double test_set_baserate_inflation = 0;
    int    ocs_length          = 10000;   // OcSLength
    int    replicates          = 800;
    double scs_kappa_threshold = 0.9;     // ScSKappaThreshold
    double scs_kappa_min       = 0.40;    // ScSKappaMin
    double scs_precision_min   = 0.6;     // ScSPrecisionMin
    double scs_precision_max   = 1;       // ScSPrecisionMax
    RhoMethod method = RhoMethod::Standard;
};

struct KappaPrecision {
    double kappa = 0, precision = 0;
};

// Both rhoR paths recurse until a valid (kappa, precision) pair turns up and
// give up with this message; here the retries are capped instead.
inline constexpr int RHO_MAX_ATTEMPTS = 10000;
inline const char* const RHO_NO_VALID_SET =
    "Could not generate a valid set given ranges of kappa and precision";

// ── Standard method: rhoR's R implementation ─────────────────────────────────

namespace rho_standard {

// getR(): the recall that gives `kappa` at this base rate and precision.
QESTATS_R_ARITHMETIC_FN
inline double recall(double kappa, double baserate, double precision) {
    QESTATS_R_ARITHMETIC
    const double top = kappa * precision;
    return top / (2 * precision - 2 * baserate - kappa + 2 * baserate * kappa);
}

// checkBRPKcombo(): whether (base rate, precision, kappa) can form a set.
QESTATS_R_ARITHMETIC_FN
inline bool valid_combo(double baserate, double precision, double kappa) {
    QESTATS_R_ARITHMETIC
    const double right = (2 * baserate * kappa - 2 * baserate - kappa) / (kappa - 2);
    return precision > right;
}

// seq(from, to, length.out = n).
QESTATS_R_ARITHMETIC_FN
inline std::vector<double> seq(double from, double to, int n) {
    QESTATS_R_ARITHMETIC
    if (n <= 0) return {};
    if (n <= 2) {
        std::vector<double> v = {from, to};
        v.resize(n);
        return v;
    }
    if (from == to) return std::vector<double>(n, from);
    const double by = (to - from) / (n - 1);
    std::vector<double> v(n);
    v[0] = from;
    for (int i = 1; i < n - 1; ++i) v[i] = from + static_cast<double>(i) * by;
    v[n - 1] = to;
    return v;
}

// sample.int(n, k) without replacement, 0-based.
template <class Rng>
std::vector<std::size_t> sample_int(std::size_t n, std::size_t k, Rng& rng) {
    if (k > n)
        throw std::invalid_argument(
            "cannot take a sample larger than the population when 'replace = FALSE'");
    std::vector<std::size_t> out(k);
    if (k < 2) {
        for (std::size_t i = 0; i < k; ++i)
            out[i] = static_cast<std::size_t>(rng.unif_index(static_cast<double>(n)));
        return out;
    }
    std::vector<std::size_t> x(n);
    for (std::size_t i = 0; i < n; ++i) x[i] = i;
    for (std::size_t i = 0; i < k; ++i) {
        const auto j = static_cast<std::size_t>(rng.unif_index(static_cast<double>(n)));
        out[i] = x[j];
        x[j] = x[--n];
    }
    return out;
}

// genPKcombo(): one (kappa, precision) draw from the two grids. When the pair
// is invalid, precision is redrawn from the grid values above the minimum
// valid precision; when none are, kappa is redrawn too.
template <class Rng>
QESTATS_R_ARITHMETIC_FN
KappaPrecision kappa_precision(const std::vector<double>& kappas,
                               const std::vector<double>& precisions,
                               double baserate, Rng& rng) {
    QESTATS_R_ARITHMETIC
    for (int attempt = 0; attempt < RHO_MAX_ATTEMPTS; ++attempt) {
        const double kappa = kappas[sample_int(kappas.size(), 1, rng)[0]];
        double precision = precisions[sample_int(precisions.size(), 1, rng)[0]];
        if (valid_combo(baserate, precision, kappa)) return {kappa, precision};
        const double precision_min =
            (2 * baserate * kappa - 2 * baserate - kappa) / (kappa - 2);
        std::vector<double> above;
        for (double p : precisions) if (p > precision_min) above.push_back(p);
        if (above.empty()) continue;
        precision = above[sample_int(above.size(), 1, rng)[0]];
        return {kappa, precision};
    }
    throw std::runtime_error(RHO_NO_VALID_SET);
}

// generateKPs() with the flat distribution: kappa from a 100,000-point grid,
// precision from a 10,000-point grid.
template <class Rng>
std::vector<KappaPrecision>
generate_kps(int n, double baserate, double kappa_min, double kappa_max,
             double precision_min, double precision_max, Rng& rng) {
    const auto kappas = seq(kappa_min, kappa_max, 100000);
    const auto precisions = seq(precision_min, precision_max, 10000);
    std::vector<KappaPrecision> kps;
    kps.reserve(n > 0 ? n : 0);
    for (int i = 0; i < n; ++i) kps.push_back(kappa_precision(kappas, precisions, baserate, rng));
    return kps;
}

// prset() / pr.ct(): the full set's contingency table (counts). R's round()
// rounds halves to even.
QESTATS_R_ARITHMETIC_FN
inline ConfusionMatrix contingency_table(double precision, double recall,
                                         int length, double baserate) {
    QESTATS_R_ARITHMETIC
    const double gold1s = std::max(std::nearbyint(baserate * length), 1.0);
    const double gold0s = length - gold1s;
    const double tp = std::max(std::nearbyint(gold1s * recall), 1.0);
    const double fp = std::min(gold0s, std::max(std::nearbyint(tp * (1 - precision) / precision), 1.0));
    return {tp, fp, gold1s - tp, gold0s - fp};
}

// getHandSet(): kappa of a test set of `length` rows sampled from the full
// set laid out as prset() does (TP rows, FN rows, FP rows, TN rows). With an
// inflated base rate, ceiling(length * inflation) rows are first sampled from
// the first rater's positives.
template <class Rng>
QESTATS_R_ARITHMETIC_FN
double hand_kappa(const ConfusionMatrix& full, int length, double inflation, Rng& rng) {
    QESTATS_R_ARITHMETIC
    const auto tp = static_cast<std::size_t>(full.tp), fn = static_cast<std::size_t>(full.fn);
    const auto fp = static_cast<std::size_t>(full.fp), tn = static_cast<std::size_t>(full.tn);
    const std::size_t gold1s = tp + fn, rows = gold1s + fp + tn;
    double counts[4] = {0, 0, 0, 0};   // tp, fp, fn, tn
    auto add = [&](std::size_t row) {
        if (row < tp)                counts[0] += 1;
        else if (row < gold1s)       counts[2] += 1;
        else if (row < gold1s + fp)  counts[1] += 1;
        else                         counts[3] += 1;
    };
    const double positives = std::ceil(length * inflation);
    if (positives > static_cast<double>(gold1s))
        throw std::invalid_argument("Not enough positives in first rater to inflate to this level");
    if (positives > 0) {
        const auto k = static_cast<std::size_t>(positives);
        std::vector<bool> taken(rows, false);
        for (std::size_t row : sample_int(gold1s, k, rng)) { taken[row] = true; add(row); }
        std::vector<std::size_t> others;
        others.reserve(rows - k);
        for (std::size_t row = 0; row < rows; ++row) if (!taken[row]) others.push_back(row);
        for (std::size_t i : sample_int(others.size(), static_cast<std::size_t>(length) - k, rng))
            add(others[i]);
    } else {
        for (std::size_t row : sample_int(rows, static_cast<std::size_t>(length), rng)) add(row);
    }
    return kappa_from_counts(counts[0], counts[1], counts[2], counts[3]);
}

// mean(x, na.rm = TRUE): R sums in long double and adds a correction pass.
inline double r_mean(const std::vector<double>& x) {
    long double s = 0;
    std::size_t n = 0;
    for (double v : x) if (!std::isnan(v)) { s += v; ++n; }
    s /= static_cast<long double>(n);
    if (std::isfinite(static_cast<double>(s))) {
        long double t = 0;
        for (double v : x) if (!std::isnan(v)) t += (v - s);
        s += t / static_cast<long double>(n);
    }
    return static_cast<double>(s);
}

// getBootPvalue(): 1 below the distribution's mean, else the share of the
// distribution at or above `observed`.
inline double boot_pvalue(const std::vector<double>& distribution, double observed) {
    if (observed < r_mean(distribution)) return 1;
    std::size_t at_or_above = 0;
    for (double v : distribution) if (v >= observed) ++at_or_above;
    return static_cast<double>(at_or_above) / static_cast<double>(distribution.size());
}

// calcRho() with given (kappa, precision) pairs.
template <class Rng>
double rho(double observed_kappa, double baserate, int test_set_length,
           const std::vector<KappaPrecision>& kps, const RhoOptions& opts, Rng& rng) {
    const std::size_t replicates =
        std::min(kps.size(), static_cast<std::size_t>(std::max(opts.replicates, 0)));
    std::vector<double> kappas(replicates, 0.0);
    for (std::size_t i = 0; i < replicates; ++i) {
        const double r = recall(kps[i].kappa, baserate, kps[i].precision);
        const auto full = contingency_table(kps[i].precision, r, opts.ocs_length, baserate);
        kappas[i] = hand_kappa(full, test_set_length, opts.test_set_baserate_inflation, rng);
    }
    return boot_pvalue(kappas, observed_kappa);
}

} // namespace rho_standard

// ── Fast method: rhoR's C++ implementation ───────────────────────────────────
//
// Compiled as plain C++, like rhoR's src/rhor.cpp, so the compiler treats its
// arithmetic the way it treated rhoR's.

namespace rho_fast {

// recall(): the same formula as rho_standard::recall.
inline double recall(double kappa, double baserate, double precision) {
    const double top = kappa * precision;
    return top / (2 * precision - 2 * baserate - kappa + 2 * baserate * kappa);
}

// check_BRK_combo().
inline bool valid_combo(double baserate, double precision, double kappa) {
    const double right = ((2 * baserate * kappa) - (2 * baserate) - kappa) / (kappa - 2);
    return precision > right;
}

// Armadillo's regspace(start, delta, end).
inline std::vector<double> regspace(double start, double delta, double end) {
    if (((start < end) && (delta < 0)) || ((start > end) && (delta > 0)) || (delta == 0)) return {};
    const bool ascend = (start <= end);
    const double inc = (delta < 0) ? -delta : delta;
    const double m = (ascend ? (end - start) : (start - end)) / inc;
    const auto n = static_cast<std::size_t>(1 + static_cast<std::size_t>(std::floor(m)));
    // Two loops, as Armadillo writes them: a compiler that fuses
    // start + i * inc into an FMA then does it the same way here.
    std::vector<double> v(n);
    double* mem = v.data();
    if (ascend) {
        for (std::size_t i = 0; i < n; ++i) { mem[i] = double(start + double(i * inc)); }
    } else {
        for (std::size_t i = 0; i < n; ++i) { mem[i] = double(start - double(i * inc)); }
    }
    return v;
}

// One index from RcppArmadillo's sample(x, 1, FALSE): int(n * unif_rand()).
template <class Rng>
std::size_t sample_one(std::size_t n, Rng& rng) {
    const int n_orig = static_cast<int>(n);
    const int j = static_cast<int>(n_orig * rng.unif_rand());
    return static_cast<std::size_t>(j);
}

// Armadillo's randi<int>(distr_param(a, b)) on R's RNG (RcppArmadillo's
// Alt_R_RNG.h): runif(0, RAND_MAX) scaled to b - a + 1 values.
template <class Rng>
int randi(int a, int b, Rng& rng) {
    if (!(a <= b))
        throw std::logic_error("randi(): incorrect distribution parameters; a must be less than b");
    const double rand_max = static_cast<double>(RAND_MAX);
    const double draw = 0.0 + (rand_max - 0.0) * rng.unif_rand();
    if (a == 0 && b == RAND_MAX) return static_cast<int>(draw);
    const auto length = static_cast<unsigned long long>(b - a + 1);
    const double scale = double(length) / rand_max;
    return (std::min)(b, int(draw * scale) + a);
}

// find_valid_pk().
template <class Rng>
KappaPrecision kappa_precision(const std::vector<double>& kappas,
                               const std::vector<double>& precisions,
                               double baserate, Rng& rng) {
    for (int attempt = 0; attempt < RHO_MAX_ATTEMPTS; ++attempt) {
        const double kappa = kappas[sample_one(kappas.size(), rng)];
        double precision = precisions[sample_one(precisions.size(), rng)];
        if (!valid_combo(baserate, precision, kappa)) {
            const double precision_min =
                (2 * baserate * kappa - 2 * baserate - kappa) / (kappa - 2);
            std::vector<double> above;
            for (double p : precisions) if (p > precision_min) above.push_back(p);
            if (above.empty()) continue;
            precision = above[sample_one(above.size(), rng)];
        }
        return {kappa, precision};
    }
    throw std::runtime_error(RHO_NO_VALID_SET);
}

// generate_kp_list() with the flat distribution (distributionType 0); rhoR's
// bell-shaped option (distributionType 1) is not ported.
template <class Rng>
std::vector<KappaPrecision>
generate_kps(int n, double baserate, double kappa_min, double kappa_max,
             double precision_min, double precision_max, Rng& rng,
             long distribution_length = 10000) {
    const double kappa_step = (kappa_max - kappa_min) / (distribution_length - 1);
    const auto kappas = regspace(kappa_min, kappa_step, kappa_max);
    const double precision_step = (precision_max - precision_min) / (10000 - 1);
    const auto precisions = regspace(precision_min, precision_step, precision_max);
    if (kappas.empty() || precisions.empty())
        throw std::invalid_argument("generate_kps: empty kappa or precision range");
    std::vector<KappaPrecision> kps;
    kps.reserve(n > 0 ? n : 0);
    for (int i = 0; i < n; ++i) kps.push_back(kappa_precision(kappas, precisions, baserate, rng));
    return kps;
}

// contingency_table(): the full set's table (counts). C's round() rounds
// halves away from zero.
inline ConfusionMatrix contingency_table(double precision, double recall,
                                         int length, double baserate) {
    const int gold1s = static_cast<int>(std::max(std::round(baserate * length), 1.0));
    const int gold0s = length - gold1s;
    const int tp = static_cast<int>(std::max(std::round(gold1s * recall), 1.0));
    const int fp = static_cast<int>(std::min(static_cast<double>(gold0s),
                                             std::max(std::round(tp * (1 - precision) / precision), 1.0)));
    return {double(tp), double(fp), double(gold1s - tp), double(gold0s - fp)};
}

// sample_contingency_table(): draw n items without replacement from cells
// (a 1x2 row or a 2x2 table in column order: TP, FP, FN, TN), returning each
// draw's 0-based cell. Kept as rhoR has it: the draw spans sum + 1 values and
// the first cell takes the extra one, so an empty first cell can be drawn.
// The draw spans every cell given; draws past the third cell land in the
// fourth.
template <class Rng>
std::vector<int> sample_contingency_table(std::vector<int> cells, int n, Rng& rng) {
    if (cells.size() < 4) cells.resize(4, 0);
    std::vector<int> out(n > 0 ? n : 0);
    for (int w = 0; w < n; ++w) {
        int max_int = 0;
        for (int c : cells) max_int += c;
        const int s = randi(0, max_int, rng);
        const int cell = (s <= cells[0]) ? 0
                       : (s <= cells[0] + cells[1]) ? 1
                       : (s <= cells[0] + cells[1] + cells[2]) ? 2 : 3;
        out[w] = cell;
        cells[cell]--;
    }
    return out;
}

// getHand_ct(): the test set's table, drawn from the full set's table.
template <class Rng>
ConfusionMatrix hand_contingency_table(const ConfusionMatrix& full, int length,
                                       double inflation, Rng& rng) {
    const int positives = static_cast<int>(std::ceil(length * inflation));
    int tp = static_cast<int>(full.tp), fp = static_cast<int>(full.fp);
    int fn = static_cast<int>(full.fn), tn = static_cast<int>(full.tn);
    int counts[4] = {0, 0, 0, 0};   // column order: tp, fp, fn, tn
    if (positives > 0) {
        int pos_tp = 0, pos_fn = 0;
        for (int cell : sample_contingency_table({tp, fn}, positives, rng))
            (cell == 0 ? pos_tp : pos_fn)++;
        tp -= pos_tp;
        fn -= pos_fn;
        counts[0] += pos_tp;
        counts[2] += pos_fn;
    }
    for (int cell : sample_contingency_table({tp, fp, fn, tn}, length - positives, rng))
        counts[cell]++;
    return {double(counts[0]), double(counts[1]), double(counts[2]), double(counts[3])};
}

// kappa_ct(): kappa as rhoR's C++ computes it. Within an ulp of
// kappa_from_counts, but NaN (not 1) when one cell holds every item.
inline double kappa_ct(const ConfusionMatrix& ct) {
    const double a = ct.tp, c = ct.fn, b = ct.fp, d = ct.tn;
    const double size = a + b + c + d;
    const double p_zero = (a + d) / size;
    const double p_yes = ((a + b) / size) * ((a + c) / size);
    const double p_no = ((c + d) / size) * ((b + d) / size);
    const double p_e = (p_yes + p_no);
    return (p_zero - p_e) / (1 - p_e);
}

// Armadillo's mean(): two interleaved accumulators.
inline double arma_mean(const std::vector<double>& x) {
    const std::size_t n = x.size();
    if (n == 0) throw std::logic_error("mean(): object has no elements");
    double acc1 = 0, acc2 = 0;
    std::size_t j;
    for (j = 1; j < n; j += 2) { acc1 += x[j - 1]; acc2 += x[j]; }
    if ((j - 1) < n) acc1 += x[j - 1];
    const double mean = (acc1 + acc2) / double(n);
    if (std::isfinite(mean)) return mean;
    for (double v : x) if (!std::isfinite(v)) return mean;
    double r_mean = 0;
    for (std::size_t i = 0; i < n; ++i) r_mean = r_mean + (x[i] - r_mean) / double(i + 1);
    return r_mean;
}

// getBootPvalue_c().
inline double boot_pvalue(const std::vector<double>& distribution, double observed) {
    if (observed < arma_mean(distribution)) return 1.0;
    std::size_t at_or_above = 0;
    for (double v : distribution) if (v >= observed) ++at_or_above;
    return (at_or_above * 1.0) / distribution.size();
}

// calcRho_c() with given (kappa, precision) pairs.
template <class Rng>
double rho(double observed_kappa, double baserate, int test_set_length,
           const std::vector<KappaPrecision>& kps, const RhoOptions& opts, Rng& rng) {
    const std::size_t replicates =
        std::min(kps.size(), static_cast<std::size_t>(std::max(opts.replicates, 0)));
    std::vector<double> kappas(replicates);
    for (std::size_t i = 0; i < replicates; ++i) {
        const double r = recall(kps[i].kappa, baserate, kps[i].precision);
        const auto full = contingency_table(kps[i].precision, r, opts.ocs_length, baserate);
        kappas[i] = kappa_ct(hand_contingency_table(full, test_set_length,
                                                    opts.test_set_baserate_inflation, rng));
    }
    return boot_pvalue(kappas, observed_kappa);
}

} // namespace rho_fast

// ── rho ──────────────────────────────────────────────────────────────────────

// rhoK()'s argument checks, with its messages.
inline void check_rho_args(double observed_kappa, double baserate, const RhoOptions& o) {
    if ((observed_kappa > 1 || observed_kappa < 0) || (baserate > 1 || baserate < 0))
        throw std::invalid_argument("x and OcSBaserate must be between 0 and 1.");
    if (o.scs_kappa_threshold < 0 || o.scs_kappa_min < 0)
        throw std::invalid_argument("OcSBaserate, ScSKappaThreshold, and ScSKappaMin must be positive.");
    if (o.scs_kappa_threshold < o.scs_kappa_min)
        throw std::invalid_argument("ScSKappaThreshold must be greater than ScSKappaMin.");
    if ((o.scs_precision_min < 0 || o.scs_precision_min > 1) ||
        (o.scs_precision_max < 0 || o.scs_precision_max > 1))
        throw std::invalid_argument("ScSPrecisionMin and ScSPrecisionMax must be between 0 and 1.");
    if (o.scs_precision_max < o.scs_precision_min)
        throw std::invalid_argument("ScSPrecisionMax must be greater than ScSPrecisionMin.");
}

// The (kappa, precision) pairs rho() simulates: opts.replicates draws with
// kappa in [scs_kappa_min, scs_kappa_threshold] and precision in
// [scs_precision_min, scs_precision_max].
template <class Rng>
std::vector<KappaPrecision> rho_kps(double baserate, const RhoOptions& opts, Rng& rng) {
    if (opts.method == RhoMethod::Fast)
        return rho_fast::generate_kps(opts.replicates, baserate, opts.scs_kappa_min,
                                      opts.scs_kappa_threshold, opts.scs_precision_min,
                                      opts.scs_precision_max, rng);
    return rho_standard::generate_kps(opts.replicates, baserate, opts.scs_kappa_min,
                                      opts.scs_kappa_threshold, opts.scs_precision_min,
                                      opts.scs_precision_max, rng);
}

// rhoK(): rho for an observed kappa on a test set of `test_set_length` items
// from a population with this base rate.
template <class Rng>
double rho(double observed_kappa, double baserate, int test_set_length,
           const RhoOptions& opts, Rng& rng) {
    check_rho_args(observed_kappa, baserate, opts);
    const auto kps = rho_kps(baserate, opts, rng);
    if (opts.method == RhoMethod::Fast)
        return rho_fast::rho(observed_kappa, baserate, test_set_length, kps, opts, rng);
    return rho_standard::rho(observed_kappa, baserate, test_set_length, kps, opts, rng);
}

inline double rho(double observed_kappa, double baserate, int test_set_length,
                  const RhoOptions& opts = {}, std::uint64_t seed = 5489u) {
    StdRng rng(seed);
    return rho(observed_kappa, baserate, test_set_length, opts, rng);
}

} // namespace qestats
