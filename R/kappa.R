kappa = function(data){
  if(nrow(data) != 2){
    return(kappaSet(data))
  }else{
    return(kappaCT(data))
  }
}