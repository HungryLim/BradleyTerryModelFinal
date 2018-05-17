tolTest<- function(hits, lambdas, DocIds, iterations){
  lambda1<-lambdas$Lambda #allows tolerance check on line 8
  hits$Lambda <- lambdas$Lambda[match(hits$DocIDj, lambdas$DocId)]
  for( i in 1:iterations){ 
    lambda1<-lambdaLoop2(hits=hits, DocIds = DocId, Hit3=hits, extractLambda=lambda1)
    lambdas$newLambda<-lambda1<-lambda1[-1] #correct indexing error; see line 30 of lambdaLoop2.cpp
    hits$Lambda <- lambdas$newLambda[match(hits$DocIDj, lambdas$DocId)] # save output to be used in the next iteration of the loop
    if (all(abs(lambda1-lambdas$Lambda)<1e-15)){ #test tolerance
      break
    }
    else{
      lambdas$Lambda<-lambda1
    }
  }
  return(lambda1)
  
}

