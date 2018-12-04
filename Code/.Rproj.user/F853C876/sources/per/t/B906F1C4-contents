# Initialisation

wd <- getwd()
source(paste(wd,"GenerateTF-IDF.R",sep="/"))

libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

options(stringsAsFactors = FALSE)

euclidean.dist <- function(x1,x2){
  return (sqrt(sum((as.numeric(x1)-as.numeric(x2))^2)))
}

knnConstance <- list("with-tfidf"=TRUE)

kNearestNeighbour <- function(trainTdm,testTdm,tdmTarget,k=10,with_tfidf=TRUE){
  knnConstance$"with-tfidf" <- with_tfidf
  knnTrainingMatrix <- trainTdm
  if(knnConstance$"with-tfidf"){
    inverseTermSum = colSums(trainTdm!=0)
    knnTrainingMatrix <- generate.tfidf.matrix(trainTdm)
  }
  prediction.list <- c()
  for(testIndex in seq(1:nrow(testTdm))){
    testDocToBeClassified <- testTdm[testIndex,]
    euclidean.dist.matrix <- data.frame(matrix(ncol = 2,nrow = nrow(knnTrainingMatrix)))
    
    if(knnConstance$"with-tfidf"){
      testDocToBeClassified.tfidf <- rep(0,ncol(knnTrainingMatrix))
      for(colIndex in seq(1:(ncol(knnTrainingMatrix)))){
        termFreq = testDocToBeClassified[colIndex]
        if(inverseTermSum[colIndex] == 0){
          testDocToBeClassified.tfidf[colIndex] <- 0
        }else{
          testDocToBeClassified.tfidf[colIndex] <- termFreq * log((nrow(knnTrainingMatrix)/inverseTermSum[colIndex]),10)
        }
      }
      testDocToBeClassified <- testDocToBeClassified.tfidf
    }
    
    euclidean.dist.matrix[,1] <- sqrt(rowSums((sweep(knnTrainingMatrix,2,unlist(testDocToBeClassified)))^2))
    euclidean.dist.matrix[,2] <- tdmTarget
    
    sorted.euclidean.dist.matrix <- euclidean.dist.matrix[order(euclidean.dist.matrix[,1],decreasing =FALSE),]
    prediction.list[testIndex] <-names(which.max(table(sorted.euclidean.dist.matrix[1:k,2])))
    
  }
  prediction.list
}