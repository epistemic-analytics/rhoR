any_equal <- function(target, current, tolerance = sqrt(.Machine$double.eps)) {
  any(sapply(target, function(targ) abs(current - targ) < tolerance ))
}
all_equal <- function(target, current, tolerance = sqrt(.Machine$double.eps)) {
  all(sapply(target, function(targ) abs(current - targ) < tolerance ))
}


#' Convert a codeset to a contingency table
#'
#' @param x codeset 
#'
#' @return contingency table as a 2x2 matrix
#' 
#' @export
as.contingency.table <- function(x) {
  tp = length(which(x[,1] == 1 & x[,2] == 1))
  tn = length(which(x[,1] == 0 & x[,2] == 0))
  fp = length(which(x[,1] == 0 & x[,2] == 1))
  fn = length(which(x[,1] == 1 & x[,2] == 0))
  
  return(matrix(c(tp,fp,fn,tn), ncol = 2, nrow = 2))
}

#' Convert codeset to contingency table
#'
#' @param x matrix contingency table (2x2)
#'
#' @return 2-column matrix representation of the contingency table
#' 
#' @export
as.codeset <- function(x) {
  return(matrix(c(rep(1, x[1,1]*2), rep(1:0, x[1,2]), rep(0:1, x[2,1]), rep(0, x[2,2]*2)), byrow=TRUE, ncol=2))
}
