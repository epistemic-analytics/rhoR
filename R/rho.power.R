rho.power = function(baserate){
  alpha = 0.05
  rhoVal = 1
  handSetLength = 0
  inc = 10
  while(rhoVal > alpha){
    handSetLength = handSetLength + inc
    rhoVal = rho(1, baserate, handSetLength)
    cat(handSetLength, rhoVal,"\n")
  }
  return(handSetLength)
}