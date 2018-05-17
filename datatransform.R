datatransform<-function(HIT){
  vec<-rep(1:2, 1500)
  HIT<-as.data.frame(cbind(HIT,vec))
  colnames(HIT)<-c("comparison_id" ,"document_id", "result","num")
  DocIDi<-NULL
  DocIDj<-NULL
  Choose<-NULL
  for (i in 1:nrow(HIT)){
    if (HIT$num[i]==2){
      DocIDj<-c(DocIDj,HIT$document_id[i])
    }
    else{
      DocIDi<-c(DocIDi,HIT$document_id[i])
      Choose<-c(Choose,HIT$result[i])
    }
  }
  newHIT<-as.data.frame(cbind(DocIDi,DocIDj,Choose))
  DocIDi<-NULL
  DocIDj<-NULL
  Choose<-NULL
  for (i in 1:nrow(HIT)){
    if (HIT$num[i]==1){
      DocIDj<-c(DocIDj,HIT$document_id[i])
    }
    else{
      DocIDi<-c(DocIDi,HIT$document_id[i])
      Choose<-c(Choose,HIT$result[i])
    }
  }
  newHIT2<-as.data.frame(cbind(DocIDi,DocIDj,Choose))
  metaHIT<-rbind(newHIT,newHIT2)
  return(metaHIT)
}
