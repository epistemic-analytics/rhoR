###
# Calculate Jaccard's J
#
# This function calculates jaccard's J given a set
# @param set This is the set to calculate jaccard's J from
# @param isSet TRUE if set FALSE if contigency table
# @param jackardJThreshold if null then return jaccard's J otherwise return list of jaccard's J and above or not
# @keywords jaccard's J, calculate
# @return This returns the jaccard's J of the set given
###
calcJaccardJ = function(set, isSet = TRUE, jackardJThreshold = NULL) {
  contTable = NULL
  if(!isSet){
    contTable = set
    set = contingencyToSet(set[1,1],set[2,1],set[1,2],set[2,2])
  }
  if(length(which(set[,1] == set[,2] & set[,1] == 1)) == nrow(set) | length(which(set[,1] == set[,2] & set[,1] == 0)) == nrow(set)){
    return(1)
  }
  
  positives = length(which(set[,1] == set[,2] & set[,1] == 1))
  negatives = length(which(set[,1] == set[,2] & set[,1] == 0))
  total = nrow(set)
  return(positives / (total - negatives));
}
