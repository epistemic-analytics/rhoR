rm(list = ls())

microbenchmark::microbenchmark(
  R = rhoR:::createRandomSet(100, 0.2, 0.4, 0.65, 0.2,0.8, type="ct"),
  C = rhoR:::random_contingency_table(100, 0.2, 0.4, 0.65, 0.2, 0.8),
  times = 1000
)

microbenchmark::microbenchmark(
  R = rhoR:::calcRho(0.67, 0.2, 20, 0.2),
  C = rhoR:::calcRho_c(0.67, 0.2, 20, 0.2),
  times = 10
)

ct = rhoR::random_contingency_table(10000, 0.2, 0.4, 0.7, 0.1, 0.8)
microbenchmark::microbenchmark(
  R = rhoR:::rhoCT(ct),
  # CR = rhoCT_useC(ct),
  C2 = rhoR:::rhoCT_c(ct),
  times = 100
)

microbenchmark::microbenchmark(
  R = rhoR:::generateKPs(numNeeded = 100, baserate = 0.2, kappaMin = 0.4, kappaMax = 0.8, precisionMin = 0.2, precisionMax = 1.0),
  C = rhoR:::generate_kp_list(numNeeded = 100, baserate = 0.2, kappaMin = 0.4, kappaMax = 0.8, precisionMin = 0.2, precisionMax = 1.0),
  times = 100
)

microbenchmark::microbenchmark(
  # getHandSet(ct, 10000, 0.2, F),
  rhoR:::getHandCT(ct, 10000, 0.2, F),
  # getHand_kappa(ct, 1000, 0.2),
  rhoR:::getHand_ct(ct, 1000, 0.2),
  times = 1000
)
microbenchmark::microbenchmark(
  rhoR:::pr.ct(0.8,0.5, 20, 0.2),
  rhoR:::contingency_table(0.8, 0.5, 20, 0.2),
  times = 100
)
rand_set <- rhoR::as.code.set(rhoR:::random_contingency_table(100, 0.2, 0.4, 0.65, 0.2, 0.8))
microbenchmark::microbenchmark(
  rhoR:::rhoSet(rand_set),
  rhoR:::rhoSet(rand_set, method = "c"),
  times = 100
)


# calcRho(0.800115, 0.02, 10000, 0.3, 10000, 10, 0.65, 0.4, 0.6, 1)
# microbenchmark::microbenchmark(
#   # rhoR:::sample_weighted2(ct, 10000),
#   rhoR:::sample_contingency_table(ct, 10000),
#   times = 100
# )
# 
# microbenchmark::microbenchmark(
#   rhoR:::run.rho_c(0.65, 0.2, replicates = 100),
#   # rhoR:::run_rho_in_c(0.65, 0.2, replicates = 100),
#   times = 10
# )
