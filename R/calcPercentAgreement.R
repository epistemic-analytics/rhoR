###
# Calculate Percent Agreement
#
# This function calculates percent agreement given a set
# @param set This is the set to calculate percent agreement from
# @param isSet TRUE if set FALSE if contigency table
# @param PAThreshold if null then return percent agreement otherwise return list of percent agreement and above or not
# @keywords percent agreement, calculate
# @return This returns the percent agreement of the set given
###
calcPercentAgreement = function(set, isSet = TRUE, PAThreshold = NULL) {
  contTable = NULL
  if(!isSet){
    contTable = set
    set = contingencyToSet(set[1,1],set[2,1],set[1,2],set[2,2])
  }
  if(length(which(set[,1] == set[,2] & set[,1] == 1)) == nrow(set) | length(which(set[,1] == set[,2] & set[,1] == 0)) == nrow(set)){
    return(1)
  }
  
  
  return (length(which(set[,1] == set[,2]))/length(set[,1]))
}
