libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

options(stringsAsFactors = FALSE)

confusionMatrix <- function(predictedLabels, actualLabels){
  rowcolnames <- unique(c(predictedLabels, actualLabels))
  confusionMatrix <- matrix(c(0:0), nrow = length(rowcolnames), ncol = length(rowcolnames), byrow = TRUE, dimnames = list(rowcolnames, rowcolnames))
  
  for( i in seq(1: length(predictedLabels))){
    confusionMatrix[predictedLabels[i], actualLabels[i]] = confusionMatrix[predictedLabels[i], actualLabels[i]] + 1 
  }
  
  return (confusionMatrix)
}