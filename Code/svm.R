libs <- c("quadprog")
lapply(libs, require, character.only = TRUE)

dot <- function(x1,x2){
  (x1)%*%(x2)
}

linear.kernal <- function(x1,x2){
  dot(x1,x2)
}
rbf.kernal <- function(x1, x2){
  gamma = 0.7
  (exp(-gamma * norm(x1 - x2,"2") ** 2))
}
polynomial.kernal <- function(x1,x2,p=3){
  ((1+ dot(x1,x2))**p)
}
gaussian.kernal <- function(x1,x2,sigma=5.0){
  (exp(-norm(x1-x2,"2")**2/(2*(sigma**2))))
}

center = function(z) (z-mean(z))/sd(z)


svm.fit = function(X, y, FUN=linear.kernal, C=NULL) {
  n.samples = nrow(X)
  n.features = ncol(X)
  K = matrix(rep(0, n.samples*n.samples), nrow=n.samples)
  for (i in 1:n.samples){
    for (j in 1:n.samples){
      K[i,j] = FUN(X[i,], X[j,])
    }
  }
  Dmat = outer(y,y) * K
  Dmat = as.matrix(Matrix::nearPD(Dmat)$mat)
  dvec = rep(1, n.samples)
  Amat = rbind(y, diag(n.samples), -1*diag(n.samples))
  bvec = c(0, rep(0, n.samples), rep(-C, n.samples))
  res = solve.QP(Dmat,dvec,t(Amat),bvec=bvec, meq=1)
  a = res$unconstrained 
  bomega = apply(a*y*X,2,sum)
  return(bomega)
}


svm.train <- function(trainTdm,testTdm,tdmTarget,kernal=linear.kernal){
  svm.multiclass = c()
  class_list <- unique(tdmTarget)
  rootSVM <- list("bomega"=NULL,"class"=NULL,"child"=NULL)
  
  trainTDMData <- as.matrix(trainTdm)
  colnames(trainTDMData) <- NULL
  testTDMData <- as.matrix(testTdm)
  colnames(testTDMData) <- NULL
  
  svm.matrix <- list()
  matrix.index=1
  for(i in 1:(length(class_list)-1)){
    print("Wait for a while to complete svm fitting process")
    for(j in (i+1):length(class_list)){
      class1 <- class_list[i]
      class2 <- class_list[j]
      topic <- tdmTarget[tdmTarget%in%c(class1,class2)]
      topic[topic==class1] = 1
      topic[topic!=1] = -1
      dataset.svm <- trainTDMData[tdmTarget%in%c(class1,class2),] 
      bomega <- svm.fit(cbind(1,dataset.svm),as.numeric(topic),kernal,0.5)
      svm.matrix[[matrix.index]] <- list("from","to","bomega")
      svm.matrix[[matrix.index]]$from <- class1
      svm.matrix[[matrix.index]]$to <- class2
      svm.matrix[[matrix.index]]$bomega <- bomega
      matrix.index = matrix.index+1
    }
  }
  
  y_pred <- c()
  
  for(testDataIndex in 1:nrow(testTDMData)){
    test.Data <- testTDMData[testDataIndex,]
    predicted <- c()
    for(svmclassindex in 1:length(svm.matrix)){
      from <- svm.matrix[[svmclassindex]]$from 
      to <- svm.matrix[[svmclassindex]]$to 
      bomega <- svm.matrix[[svmclassindex]]$bomega
      if((2*((c(1,test.Data) %*% bomega)>0)-1)==1){
        predict <- from
      }else{
        predict <- to
      }
      if(is.null(predicted[predict])|| is.na(predicted[predict])){
        predicted[predict] <-0
        
      }
      predicted[predict]<- as.numeric(predicted[predict])+1 
    }
    y_pred[testDataIndex] =names(predicted[predicted==max(predicted)])[1]
  }
  return(y_pred)
}