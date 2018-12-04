# Initialisation
libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

options(stringsAsFactors = FALSE)

generate.tdm <- function(documentList){
  noOfDocs = length(documentList)
  tdm = data.frame(matrix(ncol = 0,nrow = noOfDocs),stringsAsFactors = FALSE)
  colnames(tdm) <- c()
  print("Wait for a while to complete tdm Generating process")
  for(i in seq(1:noOfDocs)){
    doc.words <- strsplit(documentList[[i]]$content," ")
    for(j in seq(1:length(doc.words[[1]]))){
      word = doc.words[[1]][j]
      if(!(word %in% colnames(tdm))){
        tdm = cbind(tdm, rep(0,noOfDocs))
        colnames(tdm)[ncol(tdm)] =word 
      }
      tdm[i,word] <- tdm[i,word] + 1
    }
  }
  tdm
}