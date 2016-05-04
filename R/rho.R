#' @title Calculate rho (and kappa)
#' 
#' @description 
#' This function will calculate rho given the parameters of the initial set,
#' and return rho and the calculated kappa.
#' 
#' @export
#' @param set set or contigency table
#' @param isSet Boolean
#' @param fullSetBaserate Double
#' @param handSetBaserateInflation Double
#' @param fullSetLength Integer
#' @param runs Integer
#' @param kappaThreshold Double
#' @param kappaMin Double
#' @param precisionMin Double
#' @param precisionMax Double
#' @param handSetFunction Function
#' @keywords rho
#' 
#' @examples
#' # Create a contingency table:
#' # ----------------
#' # __|__P___|__N___|
#' #  P| 1433 |  567 |
#' # --|------|------|
#' #  N|  355 | 7645 |
#' # ----------------
#' #
#' myTable = as.data.frame(matrix(c(c(1433,355), c(567,7645)), ncol =2 ))
#' 
#' # Using a contingency table
#' rho(myTable)
#' 
#' # Using a set, a baserate is needed
#' rho(set, fullSetBaserate = 0.2)
#' 
#' @return A list containing the calculated rho and kappa values
###
rho = function(
  set,
  isSet = NULL,
  fullSetBaserate = NULL,
  handSetBaserateInflation = 0,
  fullSetLength = 10000,
  runs = 800,
  kappaThreshold = .65,
  kappaMin = .40,
  precisionMin = 0.6,
  precisionMax = 1,
  handSetFunction = getHandSet
){
  if(is.null(isSet)) {
    isSet = (nrow(set) != ncol(set))
  }
  setLength = length(set);
  if(isSet == F) {
    setLength = sum(set);
  }
  kappa = kappa(set, isSet);

  if(is.null(fullSetBaserate)) {
    fullSetBaserate = length(which(set[,1] == 1))/length(set);
    
    if(isSet != TRUE) {
      fullSetBaserate = (sum(set[1,]))/(sum(set));
    }
  }

  return(list(
    rho = rho.power(
      kappa, 
      fullSetBaserate,
      setLength,
      handSetBaserateInflation,
      fullSetLength,
      runs,
      kappaThreshold, kappaMin, precisionMin, precisionMax, handSetFunction
    ), 
    kappa = kappa
  ));
}