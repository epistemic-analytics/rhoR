#pragma once

// Master include for the libqe-stats C++ library.
//
// Public API:
//
//   Metrics            — all classification metrics in one struct
//   ConfusionMatrix    — tp, fp, fn, tn proportions
//   MetricRange        — min/max of a metric over the key interval
//   Interval           — lower/mean/upper from a hypergeometric estimate
//   PsResult           — n, power, cost from perfect sampling
//   BootstrapResult    — original metrics + per-metric SE and CI
//   MetricRangeRow     — one row from metrics_range()
//
// Functions:
//   metrics_from_cm(cm)                   → Metrics
//   calculate_metrics(tp,fp,fn,tn)        → Metrics
//   get_metric(m, name)                   → double
//
//   phyper(q, K, N_minus_K, n)            → double
//   hyper_range(n_pos,n,N,alpha)          → Interval
//   ps_proportion(x0,x1,N,errors,alpha)   → PsResult
//   ps_kappa(kappa0,baserate,...)          → PsResult
//   metrics_range(tp,fp,fn,tn,N,b1,alpha) → vector<MetricRangeRow>
//
//   range(name1,val1, name2,val2, target) → MetricRange
//   derive(name1,val1, name2,val2, name3,val3) → Metrics
//   derive_from_rates(b1, fdr, for_)      → Metrics  (closed-form fast path)
//
//   bootstrap_metrics(cm,n_sample,...)    → BootstrapResult
//   p_diff(cm1,cm2,n_sample,metric,...)   → double
//   n_diff(cm1,cm2,metric,n_set,...)      → int
//   N_Diff2(cm1,cm2,metric,N,d,alpha)     → int
//   range_est(cm,metric,N,alpha)          → MetricRange
//
//   kappa_from_counts(tp,fp,fn,tn)        → double  (rhoR's calcKappa)
//   rho(kappa,baserate,n,opts,rng)        → double  (rhoR's rhoK; Standard / Fast)

#include "metrics.hpp"
#include "inference.hpp"
#include "derivation.hpp"
#include "comparison.hpp"
#include "rho.hpp"
