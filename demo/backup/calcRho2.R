# ###
# # Calculate Rho
# #
# # This function calculates rho given the parameters of the initial set and the kappa of the observed handset
# #
# # @param observedKappa The kappa of the handset that rho is calculated on
# # @param fullSetBaserate The baserate that the parent set is at
# # @param handSetLength The length of the handset to be taken
# # @param handSetBaserateInflation The handset base rate inflation
# # @param fullSetLength The length of the parent set
# # @param runs The number of sets to add to the distribution
# # @param kappaThreshold The maximum kappa in the distribution
# # @param kappaMin The minimum kappa in the distribution
# # @param precisionMin The minimum precision
# # @param precisionMax The maximum precision
# # @param handSetFunction A function to call
# # 
# # @return a P value is returned and if this value is less than 0.05, it is said that the handset does generalize to the entire set
# ###
# calcRho2 = function(
#   observedKappa, 
#   fullSetBaserate, 
#   handSetLength, 
#   handSetBaserateInflation = 0,
#   fullSetLength = 10000, 
#   runs = 800, 
#   kappaThreshold = .65, 
#   kappaMin = .40, 
#   precisionMin = 0.6, 
#   precisionMax = 1
# ) {
#   #creates distribution from random kappa sets
#   KPs = generateKPs(runs, fullSetBaserate, kappaMin, kappaThreshold, precisionMin, precisionMax)
#   savedKappas = sapply(KPs, function(KP) {
#     kappa = KP$kappa
#     precision = KP$precision
#     recall = getR(kappa, fullSetBaserate, precision)
#     fullCT = pr.ct(precision = precision, recall = recall, length = fullSetLength, baserate = fullSetBaserate)
#     # browser()
#     getHandCT(full.ct = fullCT, handSetLength = handSetLength, handSetBaserate = handSetBaserateInflation);
#   })
#   
#   # compare observedKappa to distribution
#   return(getBootPvalue(savedKappas, observedKappa));
# }
# 
