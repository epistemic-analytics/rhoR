#pragma once
#include "metrics.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <stdexcept>
#include <string>
#include <vector>

// C++ port of the qestats 22-row formula table plus range() and derive().
//
// Each FormulaRow encodes a pair of independent metrics (var1, var2) and a
// "key" variable (var3).  Given var1 and var2, the key variable sweeps an
// interval [l3, u3] and three formulas produce tp, fp, fn for every key value.
// This lets derive() find the key value that matches a desired third metric
// (5-point interval narrowing), and lets range() find the min/max of any
// fourth metric over the full key interval.

namespace qestats {

// ── Formula table row ─────────────────────────────────────────────────────────

struct FormulaRow {
    std::string var1, var2, var3;  // names of the three independent variables

    // Bounds on var1 (for validation).
    std::function<double(double v1, double v2)> l1, u1;
    // Bounds on var2 (for validation).
    std::function<double(double v1, double v2)> l2, u2;
    // Bounds on the key (var3) given v1, v2.
    std::function<double(double v1, double v2)> l3, u3;
    // Confusion matrix from (v1, v2, key).
    std::function<double(double v1, double v2, double key)> tp, fp, fn;
};

// ── Build the 22-row table ────────────────────────────────────────────────────

inline std::vector<FormulaRow> make_formula_table() {
    using F2 = std::function<double(double, double)>;
    using F3 = std::function<double(double, double, double)>;

    auto c0 = [](double, double) { return 0.0; };
    auto c1 = [](double, double) { return 1.0; };

    std::vector<FormulaRow> t;

    // 1: b1, b2 — key=tp
    t.push_back({"b1","b2","tp",
        c0, c1, c0, c1,
        c0, [](double b1, double b2){ return std::min(b1,b2); },
        [](double,  double,  double k){ return k; },
        [](double b1, double, double k){ return b1 - k; },
        [](double,  double b2, double k){ return b2 - k; }
    });

    // 2: b1, precision — key=fn
    t.push_back({"b1","precision","fn",
        c0, c1, c0, c1,
        c0, [](double b1, double){ return 1.0 - b1; },
        [](double b1, double pr, double){ return b1 * pr; },
        [](double b1, double pr, double){ return b1 - b1*pr; },
        [](double,   double,   double k){ return k; }
    });

    // 3: b1, recall — key=tp
    t.push_back({"b1","recall","tp",
        c0, c1, c0, c1,
        c0, [](double b1, double rc){ return std::min(b1, rc); },
        [](double,   double,   double k){ return k; },
        [](double b1, double,  double k){ return b1 - k; },
        [](double,   double rc, double k){ return (rc > 0) ? k/rc - k : 0.0; }
    });

    // 4: b1, accuracy — key=tp
    t.push_back({"b1","accuracy","tp",
        c0, c1, c0, c1,
        [](double b1, double ac){ return std::max(0.0, b1 + ac - 1.0); },
        [](double b1, double){ return b1; },
        [](double,   double,   double k){ return k; },
        [](double b1, double,  double k){ return b1 - k; },
        [](double b1, double ac, double k){ return 1.0 - b1 - ac + k; }
    });

    // 5: b1, FOR — key=tp
    t.push_back({"b1","FOR","tp",
        c0, c1, c0, c1,
        c0, [](double b1, double){ return b1; },
        [](double,   double,   double k){ return k; },
        [](double b1, double,  double k){ return b1 - k; },
        [](double b1, double f, double){ return f * (1.0 - b1); }
    });

    // 6: b1, kappa — key=tp
    t.push_back({"b1","kappa","tp",
        c0, c1, c0, c1,
        [](double b1, double kp){
            double d = 2.0 - 2.0*b1 - (1.0 - 2.0*b1)*kp;
            return (std::abs(d) > 1e-12) ? std::max(0.0, b1*kp/d) : 0.0;
        },
        [](double b1, double kp){
            double d = 2.0 - 2.0*b1 - (1.0 - 2.0*b1)*kp;
            return (std::abs(d) > 1e-12)
                ? std::min(b1, (2.0*b1 + (1.0-b1)*kp)/d) : b1;
        },
        [](double,   double,   double k){ return k; },
        [](double b1, double,  double k){ return b1 - k; },
        [](double b1, double kp, double k){
            double d = (1.0 - 2.0*b1)*kp + 2.0*b1;
            return (std::abs(d) > 1e-12) ? (2.0*k - b1*kp)/d - k : 0.0;
        }
    });

    // 7: b2, precision — key=tp
    t.push_back({"b2","precision","tp",
        c0, c1, c0, c1,
        c0, [](double pr, double b2){ return std::min(pr, b2); },
        [](double,   double,   double k){ return k; },
        [](double,   double pr, double k){ return (pr > 0) ? k/pr - k : 0.0; },
        [](double b2, double,  double k){ return b2 - k; }
    });

    // 8: b2, recall — key=fp
    t.push_back({"b2","recall","fp",
        c0, c1, c0, c1,
        c0, [](double b2, double){ return 1.0 - b2; },
        [](double b2, double rc, double){ return b2 * rc; },
        [](double,   double,   double k){ return k; },
        [](double b2, double rc, double){ return b2 - b2*rc; }
    });

    // 9: b2, accuracy — key=tp
    t.push_back({"b2","accuracy","tp",
        c0, c1, c0, c1,
        [](double b2, double ac){ return std::max(0.0, ac + b2 - 1.0); },
        [](double b2, double ac){ return std::min(b2, ac); },
        [](double,   double,   double k){ return k; },
        [](double b2, double ac, double k){ return 1.0 - ac - b2 + k; },
        [](double b2, double,   double k){ return b2 - k; }
    });

    // 10: b2, FOR — key=tp
    t.push_back({"b2","FOR","tp",
        c0, c1, c0, c1,
        [](double b2, double f){ return (1.0-f > 1e-12) ? (b2-f)/(1.0-f) : 0.0; },
        [](double b2, double){ return b2; },
        [](double,   double,   double k){ return k; },
        [](double b2, double f, double k){
            return (std::abs(f) > 1e-12) ? 1.0 - (b2-k)/f - k : 0.0; },
        [](double b2, double,   double k){ return b2 - k; }
    });

    // 11: b2, kappa — key=tp
    t.push_back({"b2","kappa","tp",
        c0, c1, c0, c1,
        [](double b2, double kp){
            double d = 2.0 - 2.0*b2 - (1.0-2.0*b2)*kp;
            return (std::abs(d) > 1e-12) ? std::max(0.0, b2*kp/d) : 0.0;
        },
        [](double b2, double kp){
            double d = 2.0 - 2.0*b2 - (1.0-2.0*b2)*kp;
            return (std::abs(d) > 1e-12)
                ? std::min(b2, (2.0*b2 + (1.0-b2)*kp)/d) : b2;
        },
        [](double,   double,   double k){ return k; },
        [](double b2, double kp, double k){
            double d = (1.0-2.0*b2)*kp + 2.0*b2;
            return (std::abs(d) > 1e-12) ? (2.0*k - b2*kp)/d - k : 0.0; },
        [](double b2, double,   double k){ return b2 - k; }
    });

    // 12: precision, recall — key=tp
    t.push_back({"precision","recall","tp",
        c0, c1, c0, c1,
        c0,
        [](double pr, double rc){
            double x = (pr + rc - pr*rc > 1e-12)
                ? rc*pr / (pr + rc - rc*pr) : 1.0;
            return std::min({1.0, pr, rc, x});
        },
        [](double,   double,   double k){ return k; },
        [](double pr, double,  double k){ return (pr > 1e-12) ? k/pr - k : 0.0; },
        [](double,   double rc, double k){ return (rc > 1e-12) ? k/rc - k : 0.0; }
    });

    // 13: precision, accuracy — key=tp
    t.push_back({"precision","accuracy","tp",
        c0, c1, c0, c1,
        c0,
        [](double pr, double ac){
            double x = (1.0-pr > 1e-12) ? (1.0-ac)*pr/(1.0-pr) : 1.0;
            return std::min({1.0, ac, x});
        },
        [](double,   double,   double k){ return k; },
        [](double pr, double,  double k){ return (pr > 1e-12) ? k/pr - k : 0.0; },
        [](double pr, double ac, double k){ return 1.0 - k/pr - ac + k; }
    });

    // 14: precision, FOR — key=tp
    t.push_back({"precision","FOR","tp",
        c0, c1, c0, c1,
        c0, [](double pr, double){ return pr; },
        [](double,   double,   double k){ return k; },
        [](double pr, double,  double k){ return (pr > 1e-12) ? k/pr - k : 0.0; },
        [](double pr, double f, double k){
            double neg = (pr > 1e-12) ? 1.0 - k/pr : 0.0;
            return f * neg; }
    });

    // 15: precision, kappa — key=tp
    t.push_back({"precision","kappa","tp",
        c0, c1, c0, c1,
        c0,
        [](double pr, double kp){
            double d = 2.0*(1.0-kp);
            return (d > 1e-12)
                ? std::min(pr, (2.0*pr - (1.0+pr)*kp) / d) : pr;
        },
        [](double,   double,   double k){ return k; },
        [](double pr, double,  double k){ return (pr > 1e-12) ? k/pr - k : 0.0; },
        [](double pr, double kp, double k){
            double d = pr*kp + 2.0*k*(1.0-kp);
            return (std::abs(d) > 1e-12)
                ? k * ((2.0*pr - kp)/d - 1.0) : 0.0; }
    });

    // 16: recall, accuracy — key=tp
    t.push_back({"recall","accuracy","tp",
        c0, c1, c0, c1,
        c0,
        [](double rc, double ac){
            double x = (1.0-rc > 1e-12) ? (1.0-ac)*rc/(1.0-rc) : 1.0;
            return std::min({1.0, ac, x});
        },
        [](double,   double,   double k){ return k; },
        [](double rc, double ac, double k){
            return 1.0 - (rc > 1e-12 ? (1.0-rc)/rc : 0.0)*k - ac; },
        [](double rc, double,   double k){
            return (rc > 1e-12) ? (1.0-rc)/rc * k : 0.0; }
    });

    // 17: recall, FOR — key=tp
    t.push_back({"recall","FOR","tp",
        c0, c1, c0, c1,
        c0,
        [](double rc, double f){
            double d = 1.0 - rc + rc*f;
            return (d > 1e-12) ? std::min(rc, rc*f/d) : rc;
        },
        [](double,   double,   double k){ return k; },
        [](double rc, double f, double k){
            double d = (rc > 1e-12 && f > 1e-12)
                ? (1.0-rc+rc*f)/(rc*f) : 1.0;
            return 1.0 - d*k; },
        [](double rc, double,   double k){
            return (rc > 1e-12) ? (1.0-rc)/rc * k : 0.0; }
    });

    // 18: recall, kappa — key=tp
    t.push_back({"recall","kappa","tp",
        c0, c1, c0, c1,
        c0,
        [](double rc, double kp){
            return rc - (1.0-rc)/(2.0*(1.0-kp)) * kp;
        },
        [](double,   double,   double k){ return k; },
        [](double rc, double kp, double k){
            double d = 2.0*k*(1.0-kp) + kp*rc;
            return (std::abs(d) > 1e-12)
                ? (2.0*(rc-k)*(1.0-kp) - (1.0-rc)*kp) * k / d : 0.0; },
        [](double rc, double,   double k){
            return (rc > 1e-12) ? (1.0-rc)/rc * k : 0.0; }
    });

    // 19: accuracy, FOR — key=tp
    t.push_back({"accuracy","FOR","tp",
        c0, c1, c0, c1,
        [](double ac, double f){ return std::max(0.0, (ac + f - 1.0)/f); },
        [](double ac, double){ return ac; },
        [](double,   double,   double k){ return k; },
        [](double ac, double f, double k){
            return (1.0-f > 1e-12)
                ? 1.0 - ac - (ac - k)*f/(1.0-f) : 0.0; },
        [](double ac, double f, double k){
            return (1.0-f > 1e-12) ? (ac-k)*f/(1.0-f) : 0.0; }
    });

    // 20-A: accuracy, kappa — key=tp (branch A: fp uses + sqrt, fn uses - sqrt)
    {
        auto l3_ak = [](double ac, double kp){
            double disc = ac*ac - 2.0*(1.0-ac)*kp/(1.0-kp);
            return (disc >= 0)
                ? (ac - std::sqrt(disc)) / 2.0 : 0.0;
        };
        auto u3_ak = [](double ac, double kp){
            double disc = ac*ac - 2.0*(1.0-ac)*kp/(1.0-kp);
            return (disc >= 0)
                ? (ac + std::sqrt(disc)) / 2.0 : ac;
        };
        auto fp_A = [](double ac, double kp, double k) -> double {
            double inner = (1.0-ac)*(1.0-ac)
                + 2.0/(1.0-kp) * (2.0*(k-ac)*k*(1.0-kp) - (1.0-ac)*kp);
            if (inner < 0) inner = 0;
            return ((1.0-ac) + std::sqrt(inner)) / 2.0;
        };
        auto fn_A = [](double ac, double kp, double k) -> double {
            double inner = (1.0-ac)*(1.0-ac)
                + 2.0/(1.0-kp) * (2.0*(k-ac)*k*(1.0-kp) - (1.0-ac)*kp);
            if (inner < 0) inner = 0;
            return ((1.0-ac) - std::sqrt(inner)) / 2.0;
        };
        t.push_back({"accuracy","kappa","tp",
            c0, c1, c0, c1, l3_ak, u3_ak,
            [](double,double,double k){ return k; },
            fp_A, fn_A
        });
    }

    // 20-B: accuracy, kappa — key=tp (branch B: fp uses - sqrt, fn uses + sqrt)
    {
        auto l3_ak = [](double ac, double kp){
            double disc = ac*ac - 2.0*(1.0-ac)*kp/(1.0-kp);
            return (disc >= 0) ? (ac - std::sqrt(disc)) / 2.0 : 0.0;
        };
        auto u3_ak = [](double ac, double kp){
            double disc = ac*ac - 2.0*(1.0-ac)*kp/(1.0-kp);
            return (disc >= 0) ? (ac + std::sqrt(disc)) / 2.0 : ac;
        };
        auto fp_B = [](double ac, double kp, double k) -> double {
            double inner = (1.0-ac)*(1.0-ac)
                + 2.0/(1.0-kp) * (2.0*(k-ac)*k*(1.0-kp) - (1.0-ac)*kp);
            if (inner < 0) inner = 0;
            return ((1.0-ac) - std::sqrt(inner)) / 2.0;
        };
        auto fn_B = [](double ac, double kp, double k) -> double {
            double inner = (1.0-ac)*(1.0-ac)
                + 2.0/(1.0-kp) * (2.0*(k-ac)*k*(1.0-kp) - (1.0-ac)*kp);
            if (inner < 0) inner = 0;
            return ((1.0-ac) + std::sqrt(inner)) / 2.0;
        };
        t.push_back({"accuracy","kappa","tp",
            c0, c1, c0, c1, l3_ak, u3_ak,
            [](double,double,double k){ return k; },
            fp_B, fn_B
        });
    }

    // 21: FOR, kappa — key=tp; u3 = 1 (tp key upper bound is always 1)
    t.push_back({"FOR","kappa","tp",
        c0, c1,
        c0, [](double f, double){ return std::min(1.0, 2.0*(1.0-f)/(2.0-f)); },
        [](double f, double kp){
            double d = 2.0*(1.0-kp)*(1.0-f);
            return (d > 1e-12) ? kp*f/d : 0.0; },
        [](double, double){ return 1.0; },
        [](double f, double kp, double k) -> double {
            double A = 2.0*(1.0-kp)*((k*(1.0-f)+f*(1.0-k))) + (1.0-f)*kp;
            double B = 8.0*(1.0-kp)*f*(2.0*(1.0-kp)*k*(1.0-(k*(1.0-f)+f)) - (1.0-k)*f*kp);
            double d = 4.0*(1.0-kp)*f;
            if (std::abs(d) < 1e-12) return 0.0;
            double disc = A*A - B;
            if (disc < 0) disc = 0;
            return (A - std::sqrt(disc)) / d; },
        [](double f, double kp, double k) -> double {
            // fn = (1-tp)*FOR - fp
            double A = 2.0*(1.0-kp)*((k*(1.0-f)+f*(1.0-k))) + (1.0-f)*kp;
            double B = 8.0*(1.0-kp)*f*(2.0*(1.0-kp)*k*(1.0-(k*(1.0-f)+f)) - (1.0-k)*f*kp);
            double d = 4.0*(1.0-kp)*f;
            if (std::abs(d) < 1e-12) return 0.0;
            double disc = A*A - B;
            if (disc < 0) disc = 0;
            double fp_val = (A - std::sqrt(disc)) / d;
            return (1.0-k)*f - fp_val; }
    });

    return t;
}

// ── Global table (built once) ─────────────────────────────────────────────────

inline const std::vector<FormulaRow>& formula_table() {
    static const auto t = make_formula_table();
    return t;
}

// ── Name normalization ────────────────────────────────────────────────────────

inline std::string normalize_metric_name(const std::string& n) {
    if (n == "baserate")  return "b1";
    if (n == "baserate2") return "b2";
    if (n == "FDR")       return "precision";  // FDR = 1 - precision
    return n;
}

// ── Find a unique formula row for a pair (v1, v2) ────────────────────────────
// Returns nullptr if not found or ambiguous (accuracy+kappa has two rows).
inline const FormulaRow* find_formula(const std::string& v1,
                                       const std::string& v2) {
    const auto& t = formula_table();
    const FormulaRow* found = nullptr;
    int count = 0;
    for (const auto& row : t) {
        if ((row.var1 == v1 && row.var2 == v2) ||
            (row.var1 == v2 && row.var2 == v1)) {
            found = &row;
            ++count;
        }
    }
    if (count != 1) return nullptr;  // 0 = not found; >1 = ambiguous (accuracy+kappa)
    return found;
}

// ── Evaluate tp/fp/fn from a formula row, given v1, v2, key ──────────────────
struct CMResult {
    double tp, fp, fn, tn;
    bool valid = true;
};

inline CMResult eval_formula(const FormulaRow& row,
                              double v1, double v2, double key) {
    // Ensure (var1, var2) are in canonical order
    double a = v1, b = v2;
    if (row.var1 != row.var1) { a = v2; b = v1; }  // no-op placeholder

    double tp = row.tp(a, b, key);
    double fp = row.fp(a, b, key);
    double fn = row.fn(a, b, key);
    // Clamp near-zero numerical noise
    if (tp < 0 && std::abs(tp) < 1e-10) tp = 1e-10;
    if (fp < 0 && std::abs(fp) < 1e-10) fp = 0.0;
    if (fn < 0 && std::abs(fn) < 1e-10) fn = 0.0;
    double tn = 1.0 - tp - fp - fn;
    if (tn < 0 && std::abs(tn) < 1e-10) tn = 1e-10;
    bool ok = (tp >= 0 && tp <= 1 && fp >= 0 && fp <= 1 &&
               fn >= 0 && fn <= 1 && tn >= 0 && tn <= 1);
    return {tp, fp, fn, tn, ok};
}

// ── range() ──────────────────────────────────────────────────────────────────
// Given two metric values, return the [min, max] range of a third metric.
// Mirrors R's range(list(baserate=0.1, precision=0.85), "kappa").

inline MetricRange range(const std::string& name1, double val1,
                          const std::string& name2, double val2,
                          const std::string& target) {
    const std::string n1 = normalize_metric_name(name1);
    const std::string n2 = normalize_metric_name(name2);
    const std::string nt = normalize_metric_name(target);

    // FDR input: convert to precision value
    double v1 = val1, v2 = val2;
    if (name1 == "FDR") v1 = 1.0 - val1;
    if (name2 == "FDR") v2 = 1.0 - val2;

    const FormulaRow* row = find_formula(n1, n2);
    if (!row) throw std::invalid_argument("range: formula not found for pair");

    // Ensure v1/v2 align with row->var1/var2
    double a = v1, b = v2;
    if (row->var1 == n2) { a = v2; b = v1; }

    // Key bounds
    const double l3 = row->l3(a, b) + 1e-10;
    const double u3 = row->u3(a, b) - 1e-10;
    if (l3 >= u3) throw std::domain_error("range: invalid key range");

    auto eval_target = [&](double key) -> double {
        auto cm = eval_formula(*row, a, b, key);
        if (!cm.valid) throw std::domain_error("range: invalid confusion matrix");
        double s = cm.tp + cm.fp + cm.fn + cm.tn;
        Metrics m = metrics_from_cm({cm.tp/s, cm.fp/s, cm.fn/s, cm.tn/s});
        return get_metric(m, nt);
    };

    // 5-point interval narrowing for max
    double max_v = -1e300, min_v = 1e300;
    {
        double left = l3, right = u3;
        int max_pos = 0;
        bool monotonic = false;
        while (right - left > 1e-6) {
            double step = (right - left) / 4.0;
            double best = -1e300;
            for (int i = 0; i <= 4; ++i) {
                double key = left + i * step;
                double v = eval_target(key);
                if (v > best) { best = v; max_pos = i; }
            }
            max_v = best;
            if (max_pos == 0 || max_pos == 4) { monotonic = true; break; }
            left  = left + (max_pos - 1) * step;
            right = left + 2.0 * step;
        }
        (void)monotonic;
    }
    // 5-point interval narrowing for min
    {
        double left = l3, right = u3;
        bool monotonic = false;
        while (right - left > 1e-6) {
            double step = (right - left) / 4.0;
            double best = 1e300;
            int min_pos = 0;
            for (int i = 0; i <= 4; ++i) {
                double key = left + i * step;
                double v = eval_target(key);
                if (v < best) { best = v; min_pos = i; }
            }
            min_v = best;
            if (min_pos == 0 || min_pos == 4) { monotonic = true; break; }
            left  = left + (min_pos - 1) * step;
            right = left + 2.0 * step;
        }
        (void)monotonic;
    }

    return {min_v, max_v};
}

// ── derive() ─────────────────────────────────────────────────────────────────
// Given values for three independent metrics, return all metrics.
// Mirrors R's derive(list(baserate=0.1, FDR=0.15, FOR=0.05)).
//
// Inputs: three (name, value) pairs.  "kappa" must not be one of the first two
// (it is the target variable in the 5-point search).

inline Metrics derive(const std::string& name1, double val1,
                       const std::string& name2, double val2,
                       const std::string& name3, double val3) {
    // Normalize names
    std::string n[3] = {
        normalize_metric_name(name1),
        normalize_metric_name(name2),
        normalize_metric_name(name3)
    };
    double v[3] = {val1, val2, val3};

    // FDR → precision (inverted value)
    if (name1 == "FDR") v[0] = 1.0 - val1;
    if (name2 == "FDR") v[1] = 1.0 - val2;
    if (name3 == "FDR") v[2] = 1.0 - val3;

    // Move kappa to position 2 (it becomes the search target)
    for (int i = 0; i < 2; ++i) {
        if (n[i] == "kappa") {
            std::swap(n[i], n[2]); std::swap(v[i], v[2]);
            break;
        }
    }
    const std::string& target_name = n[2];
    const double        target_val  = v[2];

    // Validate range of the target
    MetricRange rng = range(name1, val1, name2, val2, name3);
    // For normalized names after FDR conversion the range call uses the
    // converted names — call with original names so alias handling fires.
    rng = range(name1, val1, name2, val2, name3);
    if (target_val < rng.min || target_val > rng.max)
        throw std::domain_error("derive: target metric value outside valid range");

    // Get formula for (n[0], n[1])
    const FormulaRow* row = find_formula(n[0], n[1]);
    if (!row) throw std::invalid_argument("derive: formula not found for pair");

    double a = v[0], b = v[1];
    if (row->var1 == n[1]) { a = v[1]; b = v[0]; }

    const double l3 = row->l3(a, b) + 1e-10;
    const double u3 = row->u3(a, b) - 1e-10;

    auto eval_target = [&](double key) -> double {
        auto cm = eval_formula(*row, a, b, key);
        if (!cm.valid) return target_val;  // keep search moving
        double s = cm.tp + cm.fp + cm.fn + cm.tn;
        Metrics m = metrics_from_cm({cm.tp/s, cm.fp/s, cm.fn/s, cm.tn/s});
        return get_metric(m, target_name);
    };

    // 5-point interval narrowing to find the key value
    double left = l3, right = u3;
    while (right - left > 1e-6) {
        double x5[5];
        double step = (right - left) / 4.0;
        for (int i = 0; i < 5; ++i) x5[i] = left + i * step;

        double best_err = 1e300;
        int best_idx = 0;
        for (int i = 0; i < 5; ++i) {
            double err = std::abs(eval_target(x5[i]) - target_val);
            if (err < best_err) { best_err = err; best_idx = i; }
        }
        if (best_idx == 0)      right = x5[1];
        else if (best_idx == 4) left  = x5[3];
        else { left = x5[best_idx-1]; right = x5[best_idx+1]; }
    }
    const double key_final = (left + right) / 2.0;
    auto cm = eval_formula(*row, a, b, key_final);
    double s = cm.tp + cm.fp + cm.fn + cm.tn;
    return metrics_from_cm({cm.tp/s, cm.fp/s, cm.fn/s, cm.tn/s});
}

// Convenience: derive from a closed-form (b1 + FDR + FOR) — fast path used
// by libcodey without the formula search.
inline Metrics derive_from_rates(double b1, double fdr, double for_) {
    const double fp = fdr * b1;
    const double tp = b1 - fp;
    const double fn = for_ * (1.0 - b1);
    const double tn = 1.0 - fp - tp - fn;
    return metrics_from_cm({tp, fp, fn, tn});
}

} // namespace qestats
