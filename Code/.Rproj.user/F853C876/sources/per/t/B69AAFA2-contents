# Initialization
list.of.packages <- c("tm", "plyr", "class","quadprog")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


wd <- getwd()

source(paste(wd,"GenerateTDM.R",sep="/"))
source(paste(wd,"GenerateTF-IDF.R",sep="/"))
source(paste(wd,"FeatureCategoryMatrix.R",sep="/"))
source(paste(wd,"Naive_Bayes.R",sep="/"))
source(paste(wd,"ConfusionMatrix.R",sep="/"))
source(paste(wd,"K-NearestNeighbours.R",sep="/"))
source(paste(wd,"analysis.R",sep="/"))
source(paste(wd,"decision_tree.R",sep="/"))
source(paste(wd,"svm.R",sep="/"))

libs <- c("tm", "plyr", "class")

is.nan.data.frame <- function(x){
  do.call(cbind,lapply(x,is.nan))
}

lapply(libs, require, character.only = TRUE)

options(stringsAsFactors = FALSE)

#topics <- c("ITD.csv", "OAA.csv", "OSL.csv")
topics <- c("business", "entertainment", "politics", "sport", "tech")

#Replace your path to data directory here
#pathname <- paste(wd,"/../Dataset/University_Dataset")
pathname <- paste(wd,"/../Dataset/Article_Data")

# Clean DataSets

removeSparseTermsFromTdm <- function(tdm,sparsity){
  tdm[,!colSums(tdm!=0)<(1-sparsity)*nrow(tdm)]
}

cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, tolower)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
  corpus.tmp <- tm_map(corpus.tmp,removeWords,c("ad","us","2003","2005","mr","2004","uk","go","next","im"))
  corpus.tmp <- tm_map(corpus.tmp, stemDocument) 
  return (corpus.tmp)
}

# Generate Term Document Matrix
generateTermDocumentMatrix <- function(topic, pathname){
  directory <- sprintf("%s\\%s", pathname, topic)
  
  #dataframe <- read.csv(directory,stringsAsFactors=F)
  
  corpus <- Corpus(DirSource(directory = directory, encoding ="UTF-8"))
  #corpus <- Corpus(DataframeSource(dataframe))
  
  corpus_clean <- cleanCorpus(corpus)

  tdm <- generate.tdm(corpus_clean)
  tdm <- removeSparseTermsFromTdm(tdm,0.8)
  
  result <- list(topic = topic, tdm = tdm)
}

tdm <- lapply(topics, generateTermDocumentMatrix, pathname = pathname)

# Bind Topic to Term Document Matrix
bindTopicToTermDocumentMatrix <- function(tdm){
  dataframe <-  (tdm[["tdm"]])
  dataframe <- cbind(dataframe, rep(tdm[["topic"]], nrow(dataframe)))
  colnames(dataframe)[ncol(dataframe)] <- "targetTopic"
  return (dataframe)
}

topicTDM <- lapply(tdm, bindTopicToTermDocumentMatrix)

# Stack all Datasets
stackTDM <- do.call(rbind.fill, topicTDM)
stackTDM[is.na(stackTDM)] <- 0  

# Remove extra columns
stackTDM <- stackTDM[,!colnames(stackTDM) %in% c("ad","us","2003","2005","mr","2004","uk","go","next","im")]

# Split target topic and TDM
tdmTopic <- stackTDM[,"targetTopic"]
stackTdmNl  <- stackTDM[,!colnames(stackTDM) %in% "targetTopic"]

# Split training and test data
trainData <- sample(nrow(stackTDM),ceiling(nrow(stackTDM)*0.8))
testData <- (1:nrow(stackTDM))[- trainData]



print("--------------------------------------------------------")
print("-------------------  NaiveBayes  -----------------------")

predictedLabelsNaiveBayes <- naiveBayes(stackTdmNl[trainData,],tdmTopic[trainData], documentTobeClassified = stackTdmNl[testData,], table(unlist(tdmTopic[trainData])),with_tfidf = FALSE)
NBConfusionMatrix <- confusionMatrix(predictedLabelsNaiveBayes, tdmTopic[testData])
nbAnalysis <- getAnalytics(NBConfusionMatrix)
print(NBConfusionMatrix)
print(nbAnalysis)

print("-----------------  End NaiveBayes  ---------------------")



print("--------------------------------------------------------")
print("-------------------  KNN Start  -----------------------")

k <- ceiling(sqrt(nrow(stackTdmNl[trainData,])))
knnPred <- kNearestNeighbour(stackTdmNl[trainData,], stackTdmNl[testData,], tdmTopic[trainData],k = k  ,with_tfidf = FALSE)
KNNConfusionMatrix <- confusionMatrix(knnPred, tdmTopic[testData])
knnAnalysis <- getAnalytics(KNNConfusionMatrix)
print(KNNConfusionMatrix)
print(knnAnalysis)

print("-------------------  END KNN  -----------------------")



print("--------------------------------------------------------")
print("-------------------  Decision Tree  -----------------------")

decisionTree_result <- decisionTree(stackTdmNl[trainData,], tdmTopic[trainData], stackTdmNl[testData,],with_tfidf = FALSE)
decisionTreeConfusionMatrix <- confusionMatrix(unlist(decisionTree_result),tdmTopic[testData])
decionTreeAnalysis <- getAnalytics(decisionTreeConfusionMatrix)
print(decisionTreeConfusionMatrix)
print(decionTreeAnalysis)

print("------------------- END Decision Tree  -----------------------")



print("--------------------------------------------------------")
print("-------------------  SVM Start  -----------------------")

y_pred <- svm.train(stackTdmNl[trainData,], stackTdmNl[testData,], tdmTopic[trainData],kernal = rbf.kernal) # specify diffrent kernal from {linear.kernal,gaussian.kernal,polynomial.kernal,rbf.kernal} 
svmConfusionMatrix <- confusionMatrix(y_pred, tdmTopic[testData])
svmAnalysis <- getAnalytics(svmConfusionMatrix)
print(svmConfusionMatrix)
print(svmAnalysis)
print("-------------------  END SVM  -----------------------")

