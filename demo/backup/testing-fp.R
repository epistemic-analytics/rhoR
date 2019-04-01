
compareP = function(precision, TP) {
  FP = TP * (1 - precision) / precision
  
  # clm
  FP2 = (TP - (precision * TP)) / precision

  return(c("brad" = FP, "cody" = FP2))
}


round(runif(10, 1, 20),0)
runif(10, 0, 1)

runIt = function(p = runif(10,0,1), tp = round(runif(1, 1, 20),0)) {
  compareP(precision = p, TP = tp)  
}

data.frame(t(sapply(runif(10, 0, 1), runIt)))

data.frame(t(sapply(rep(.8, 10), runIt, tp = 8)))
