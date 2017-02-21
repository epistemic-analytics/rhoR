###
# Calculate Recall
#
# This function calculates recall given a set
# @param set This is the set to calculate recall from
# @param isSet TRUE if set FALSE if contigency table
# @param recallThreshold if null then return recall otherwise return list of recall and above or not
# @keywords recall, calculate
# @return This returns the recall of the set given
###
calcRecall = function(set, isSet = TRUE, recallThreshold = NULL) {
  contTable = NULL
  if(!isSet){
    contTable = set
    set = contingencyToSet(set[1,1],set[2,1],set[1,2],set[2,2])
  }
  if(length(which(set[,1] == set[,2] & set[,1] == 1)) == nrow(set) | length(which(set[,1] == set[,2] & set[,1] == 0)) == nrow(set)){
    return(1)
  }
  
  
  positives = length(which(set[,1] == set[,2] & set[,1] == 1))
  falseNegatives = length(which(set[,1] == 1 & set[,2] == 0))
  recall = positives / (positives + falseNegatives)
  if(is.nan(recall)){
    recall = 0
  }
  return(recall)
}
