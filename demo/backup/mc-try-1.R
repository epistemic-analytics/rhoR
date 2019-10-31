remove.packages("rhoR")
rm(list = ls())
# devtools::install_git(url = "https://git.doit.wisc.edu/clmarquart/rhoR/", ref="feature/ct-usage", force = T, quiet = F, build = T, quick = T, dependencies = F, reload = T)
# devtools::load_all(".")
library(rhoR)
library(snow)
library(MonteCarlo)

run.rho_c <- function(
  threshold,  baserate, 
  handset.length = 20,
  handset.baserate = 0.2, 
  replicates = 800
) {
  ct = random_contingency_table(
    setLength = 10000, baserate = baserate,
    kappaMin = 0.4, kappaMax = 1.0,
    minPrecision = 0.6, maxPrecision = 1.0
  )
  handset = getHand_ct(
    ct, 
    handSetLength = handset.length, 
    handSetBaserate = handset.baserate
  )
  res = FALSE
  if(!any(is.na(handset))) {
    cc = rhoCT_c(handset, testSetBaserateInflation = handset.baserate, replicates = replicates)
    # print(cc)
    res = cc
  }
  
  return(list(kappaCondition = res[1]>threshold, rhoCondition = res[2] <= 0.05, kappa = res[1], rho = res[2]))
}

run.mc <- function(
  handsetLength, # = c(90),
  inflations, # = c(0.5), #, 0.2, 0.3, 0.5),
  thresholds = c(0.65), #, 0.70), 
  replicates = 800,
  nrep = 1,
  ncpus = 1
) {
  param_list = list(handset.length = handsetLength, baserate = inflations, threshold = thresholds, replicates = replicates) 
  
  erg <- MonteCarlo(
    func=run.rho_c,
    param_list=param_list,
    nrep=nrep,
    ncpus=ncpus,
    max_grid = 1000,
    raw = TRUE,
    export_also=list("functions"=c())
  )
  return(erg)
}

duration = system.time({
  res = run.mc(
    handsetLength = 90,
    inflations = 0.5,
    ncpus = future::availableCores() - 1, 
    nrep = 1000
  )
});


res.df = MakeFrame(res)
res.dt = data.table::data.table(res.df)
data.table::setorderv(res.dt, c("baserate","handset.length"))

# res.summed = res.dt[,lapply(.SD, sum) ,by = c("handset.length", "infl"), .SDcols = c("decision")]
# res.summed$percent = res.summed$decision / res$meta$nrep
# res.mat = matrix(res.summed$percent)
# dim(res.mat) = c(length(unique(res.summed$handset.length)), length(unique(res.summed$infl)))
# colnames(res.mat) = unique(res.summed$infl)
# rownames(res.mat) = unique(res.summed$handset.length)
# t(res.mat)
