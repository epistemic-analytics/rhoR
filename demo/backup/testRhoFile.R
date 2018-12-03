fileName = "data/codeSet-multiple.csv"
rho.file(fileName, x = "rater4", y = "rater3")
rho.file(fileName, 3, 4)
csFile = read.csv(fileName)

newCols = createSimulatedCodeSet(length = 40, 0.2, 0.8, 0.90, 0.4, 1)
csFile$rater3 = newCols[,1]
csFile$rater4 = newCols[,2]

# csFile = csFile[,c(2:5)]
colnames(csFile)
write.csv(csFile, file=fileName, row.names = F)
