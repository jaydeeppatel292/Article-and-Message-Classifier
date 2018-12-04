wd <- getwd()
source(paste(wd,"GenerateTF-IDF.R",sep="/"))

constants <- list("min_info_gain"=1e-3,"minimum_leaf_size"=3,"with-tfidf"=TRUE)

get_gini_impurity <- function(feature_data){
  
  if(length(feature_data) == 0) 
    return(0)
  
  return(1- sum((table(feature_data)/length(feature_data))^2)) 
}


info_gain <- function(y,x,splitValue){
  
  leftSplitedTarget <- y[x<splitValue]
  rightSplitedTarget <- y[x>=splitValue]
  current_uncertainty <- get_gini_impurity(y)
  gini_left_subtree <- get_gini_impurity(leftSplitedTarget)
  gini_right_subtree <- get_gini_impurity(rightSplitedTarget)
  
  leftSubTreeSize = length(leftSplitedTarget)
  rightSubTreeSize = length(rightSplitedTarget)
  totalSize <- length(y)
  
  if ( leftSubTreeSize == 0 || rightSubTreeSize == 0) 
    return(0)
  
  return (current_uncertainty - (leftSubTreeSize/totalSize)*gini_left_subtree - (rightSubTreeSize/totalSize)*gini_right_subtree)
}

max_information_gain_split <- function(y,x){
  best_info_gain = NA
  split_value = NA
  for( val in sort(unique(x))){
    information_gain <- info_gain(y,x,val) 
    if(is.na(best_info_gain) || information_gain > best_info_gain){
      best_info_gain = information_gain
      split_value = val
    }
  }
  return(list("best_change"=best_info_gain,
              "split_value"=split_value))
}


best_feature_split <- function(X,y){
  results <- sapply(X,function(x) max_information_gain_split(y,x))
  best_name <- names(which.max(results['best_change',]))
  best_result <- results[,best_name]
  best_result[["name"]] <- best_name
  best_result
}

get_best_mask <- function(X,best_feature_list){
  best_mask <- X[,best_feature_list$name] < best_feature_list$split_value
  return(best_mask)
}

generateDecisionTree <- function(decisionTreeNode){

  best_split <- best_feature_split(decisionTreeNode$dataset,decisionTreeNode$target)
  if(best_split$best_change == 0){
    return (decisionTreeNode)
  }

  best_mask <- get_best_mask(decisionTreeNode$dataset,best_split)
  decisionTreeNode$best_split <- best_split
  
  leftDf = decisionTreeNode$dataset[best_mask,]
  rightDf = decisionTreeNode$dataset[!best_mask,]
  
  if(nrow(leftDf) < constants[["minimum_leaf_size"]] || nrow(rightDf) < constants[["minimum_leaf_size"]]  ){
    return(decisionTreeNode)
  }

  leftDecisionTreeNode <-  list(dataset="data.frame",target=decisionTreeNode$target,best_split=NULL,branches=list("left"=NULL,"right"=NULL))
  leftDecisionTreeNode$dataset <- leftDf
  leftDecisionTreeNode$target <- decisionTreeNode$target[best_mask]
  leftDecisionTreeNode$level <- decisionTreeNode$level +1 
  
  rightDecisionTreeNode <-  list(dataset="data.frame",target=decisionTreeNode$target,best_split=NULL,branches=list("left"=NULL,"right"=NULL))
  rightDecisionTreeNode$dataset <- rightDf
  rightDecisionTreeNode$target <- decisionTreeNode$target[!best_mask]
  rightDecisionTreeNode$level <- decisionTreeNode$level +1
  
  decisionTreeNode$branches$left <- leftDecisionTreeNode
  decisionTreeNode$branches$left <- generateDecisionTree(decisionTreeNode$branches$left)
  decisionTreeNode$branches$right <- rightDecisionTreeNode
  decisionTreeNode$branches$right <- generateDecisionTree(decisionTreeNode$branches$right)
  
  
  decisionTreeNode
}
 

printDecisionTree <- function(decisionTreeNode){
  if(!(is.null(decisionTreeNode))){
    if(!(is.null(decisionTreeNode$branches$left))){
      noOfSpaces <- paste0("%",(decisionTreeNode$level)*5,"s"," < %s")
      print(sprintf(noOfSpaces,decisionTreeNode$best_split$name,formatNumber(decisionTreeNode$best_split$split_value,2)))
      printDecisionTree(decisionTreeNode$branches$left)
    }
    if(!(is.null(decisionTreeNode$branches$right))){
      noOfSpaces <- paste0("%",(decisionTreeNode$level)*5,"s"," > %s")
      print(sprintf(noOfSpaces,decisionTreeNode$best_split$name,formatNumber(decisionTreeNode$best_split$split_value,2)))
      printDecisionTree(decisionTreeNode$branches$right)
    }
    if(is.null(decisionTreeNode$branches$left) && is.null(decisionTreeNode$branches$right)){
      noOfSpaces <- paste0("%",(decisionTreeNode$level)*6,"s","%s")
      print(sprintf(noOfSpaces,"Decision ----->",names(which.max(table(decisionTreeNode$target)))))
    }
  }
}




predict_data = function(row,decisioTreeNode){
  if(is.null(decisioTreeNode$branches$left)){
    predict_value <-  names(which.max(table(decisioTreeNode$target)))
  } else {
    
    if(row[decisioTreeNode$best_split$name] < decisioTreeNode$best_split$split_value){
      predict_value = predict_data(row,decisioTreeNode$branches$left)
    } else {
      predict_value = predict_data(row,decisioTreeNode$branches$right)
    }
  }
  return(predict_value)
}

predict_test_dataset <- function(testDataset,decisionTreeRootNode){
  predict_list <- list()
  
  inverseTermSum = NULL
  if(constants[["with-tfidf"]]){
    inverseTermSum = colSums(testDataset!=0)
  }
  
  for(testIndex in 1:nrow(testDataset)){
    testData <- testDataset[testIndex,]
    
    if(constants[["with-tfidf"]]){
      testData <- rep(0,ncol(decisionTreeRootNode$dataset))
      
      for(colIndex in (1:(ncol(decisionTreeRootNode$dataset)))){
        termFreq = testDataset[testIndex,colIndex]
        if(inverseTermSum[colIndex] == 0){
          testData[colIndex] <- 0
        }else{
          testData[colIndex] <- termFreq * log((nrow(decisionTreeRootNode$dataset)/inverseTermSum[colIndex]),10)
        }
      }
    }
    predict_list <- c(predict_list,predict_data(testDataset[testIndex,],decisionTreeRootNode))
  }
  predict_list
}

decisionTree <- function(trainDataset,trainTarget,testDataset,with_tfidf=TRUE){
  constants[["with-tfidf"]] <- with_tfidf
  
  if(constants[["with-tfidf"]]){
    trainDataset <-  generate.tfidf.matrix(trainDataset)
  }
  
  DecisionTreeRootNode <- list(dataset="data.frame",target=c(),best_split=NULL,branches=list("left"=NULL,"right"=NULL),level=2)
  DecisionTreeRootNode$dataset <- trainDataset
  DecisionTreeRootNode$target <- trainTarget
  
  DecisionTreeRootNode <- generateDecisionTree(DecisionTreeRootNode)
  printDecisionTree(DecisionTreeRootNode)
  
  predict_test_dataset(testDataset,DecisionTreeRootNode)
}

formatNumber <- function(number,decimalPoints){
  format(round(number, decimalPoints), nsmall = decimalPoints)
}

