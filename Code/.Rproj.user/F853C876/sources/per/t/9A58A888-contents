# Initialisation
libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

options(stringsAsFactors = FALSE)
generate.tfidf.matrix <- function(tdm){
  tfidfMatrix = data.frame(matrix(ncol= ncol(tdm),nrow = nrow(tdm)),stringsAsFactors = FALSE)
  colnames(tfidfMatrix) <- colnames(tdm)
  
  inverseTermSum = colSums(tdm!=0)
  
  print("Wait for a while to complete tfidf Generating process")
  for(i in seq(1:nrow(tdm))){
    for(j in seq(1:ncol(tdm))){
      if(inverseTermSum[j]==0){
        tfidfMatrix[i,j] <- 0
      }
      else {
        tfidfMatrix[i,j] <- tdm[i,j] * log10(nrow(tdm)/inverseTermSum[j])
      }
    }
  }
  tfidfMatrix
}