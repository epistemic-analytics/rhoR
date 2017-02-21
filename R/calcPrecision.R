###
# Calculate Precision
#
# This function calculates precision given a set
# @param set This is the set to calculate precision from
# @param isSet TRUE if set FALSE if contigency table
# @param precisionThreshold if null then return precision otherwise return list of precision and above or not
# @keywords precision, calculate
# @return This returns the precision of the set given
###
calcPrecision = function(set, isSet = TRUE, precisionThreshold = NULL) {
  contTable = NULL
  if(!isSet){
    contTable = set
    set = contingencyToSet(set[1,1],set[2,1],set[1,2],set[2,2])
  }
  if(length(which(set[,1] == set[,2] & set[,1] == 1)) == nrow(set) | length(which(set[,1] == set[,2] & set[,1] == 0)) == nrow(set)){
    return(1)
  }
  
  
  positives = length(which(set[,1] == set[,2] & set[,1] == 1))
  falsePositives = length(which(set[,1] == 0 & set[,2] == 1))
  precision = positives / (positives + falsePositives)
  if(is.nan(precision)){
    precision = 0
  }
  return(precision)
}
