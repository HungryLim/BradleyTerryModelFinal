dataReorganizer<-function(x, lambdaDF, hitDF){ #this formats data to be taken into posteriorlambda
  rownames(lambdaDF)<-lambdaDF$DocId 
  colnames(lambdaDF)<-c("DocId", 'Lambda')
  lambdax<-lambdaDF[paste0(x),"Lambda"]
  thisChoos<-hitDF[which(hitDF$DocIDi==x),"Choose"]
  thisLambda<-lambdaDF[paste0(hitDF[which(hitDF$DocIDi==x),"DocIDj"]),]
  newData<-cbind(thisChoos, thisLambda)
  colnames(newData)<-c("Choose", "DocId", "Lambda")
  newData
}