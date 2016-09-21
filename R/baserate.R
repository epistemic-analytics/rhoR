baserate = function(data){
  if(nrow(data) != 2){
    return(baserateSet(data))
  }else{
    return(baserateCT(data))
  }
}