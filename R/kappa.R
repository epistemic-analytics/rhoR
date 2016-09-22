###
#' @include rhoCT.R
#' @title Calculate kappa
#' 
#' @description 
#' This function calculates Cohen's kappa on a \code{\link{contingencyTable}} or a \code{\link{codeSet}}. 
#' 
#' @export
#' @param data A \code{\link{contingencyTable}} or a \code{\link{codeSet}}
#' 
#' @keywords kappa
#' 
#' @seealso \code{\link{kappaSet}} and \code{\link{kappaCT}}
#' 
#' @examples
#' #Given a code set
#' kappa(data = codeSet)
#' 
#' #Given a contingency Table
#' kappa(data = contingencyTable)
#' 
#' @return 
#' The kappa of the \code{\link{contingencyTable}} or \code{\link{codeSet}}
###

kappa = function(data){
  if(nrow(data) != 2){
    return(kappaSet(data))
  }else{
    return(kappaCT(data))
  }
}