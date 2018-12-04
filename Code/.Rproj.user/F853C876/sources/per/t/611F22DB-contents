# Initialisation
libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

options(stringsAsFactors = FALSE)

featureCategoryMatrix <- function(tdmMatrix, tdmTarget){
  actualClasses <- unique(tdmTarget)
  featureMatrix <- matrix(c(0:0), nrow = length(actualClasses), ncol= length(colnames(tdmMatrix)), byrow = TRUE, dimnames = list(actualClasses, colnames(tdmMatrix)))
  featureMatrix <- data.frame(featureMatrix)
  for(i in seq(1: length(actualClasses))){
    indexList <- which(tdmTarget %in% actualClasses[i]) 
    featureMatrix[i,] <- colSums(tdmMatrix[indexList,])
  }
  return (featureMatrix)
}