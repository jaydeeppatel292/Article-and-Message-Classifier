# Initialisation
libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

#Assumption if the posterior probability comes out to be equal, classifier will classify in the first class
options(stringsAsFactors = FALSE)

nbConstance <- list("with-tfidf"=TRUE)

naiveBayes <- function(tdmMatrix,tdmTarget, documentTobeClassified, classFrequency,with_tfidf=TRUE){
  nbConstance$"with-tfidf" <- with_tfidf
  featureMatrix <- tdmMatrix
  if(nbConstance$"with-tfidf"){
    featureMatrix <- generate.tfidf.matrix(tdmMatrix)
  }
  tdmMatrix <- NULL
  featureMatrix <- featureCategoryMatrix(featureMatrix,tdmTarget)
  
  targetLabelsList <- c()
  sumOfMatrix <- sum(featureMatrix)
  
  for(c in seq(1: nrow(documentTobeClassified))){
    max <- 0
    classLabel <- ""
    for(i in seq(1: nrow(featureMatrix))){
      sumOfWords <- sum(featureMatrix[i,])
      #P(Class) priori
      prioriProbability <- sumOfWords / sumOfMatrix
      #P(Doc/Class) likelihood
      likelihood <- 1
      for(j in seq(ncol(documentTobeClassified))){
        if(documentTobeClassified[c,j]!= 0 && featureMatrix[i,j] != 0){
          countOfWordInClass = featureMatrix[i, j]
          likelihood <- likelihood * countOfWordInClass * sumOfMatrix / classFrequency[[i]]
        }
      }
      
      posteriorProbability <- prioriProbability * likelihood
      
      if(max < posteriorProbability){
        max <- posteriorProbability
        classLabel <- rownames(featureMatrix)[i]
        
      }
    }
    targetLabelsList[c] <- classLabel
  }
  
  return (targetLabelsList)
}

