rm(list=ls())
#==============================================================================
# Read in and Format Data
#==============================================================================

#Read in CombinedOutputExperiment2.csv
dat<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModelFinal/CombinedOutputExperiment2.csv", header = T)
HIT<-dat[,3:5] #subset relevant columns

#Format the data: source datatransform.R
setwd('C:/Users/zoeja/OneDrive/Documents/Spring2018/BradleyTerryModelFinal') #set the wd now; there is much to source
source("datatransform.R") #use only once; this function duplicates "HIT," 
                          #which compares document i to document j, and 
                          #switches the document i and j column and appends the duplication
                          #to allow comaprison of document j to document i
HIT2<-datatransform(HIT) 

#Generate random lambda values, which will ultimately converge to the true lambda values
#The random lambda values will be assigned to a document ID. There are 50 values because that's what we were given from the STAN version of this project
# set.seed(42) # for replication
DocId<-unique(HIT$document_id)
DocId<-sort(DocId)
Lambda<-runif(50) 
lambda<-as.data.frame(cbind(DocId,Lambda)) #This will be an argument in future functions

#Create a single dataframe that merges HIT2 and the random lambda values 
HIT2$Lambda <- lambda$Lambda[match(HIT2$DocIDj, lambda$DocId)] #This will be an argument in future functions


#==============================================================================
# Compare the Rcpp (dependent) functions/outputs and STAN outputs
#==============================================================================

#Rcpp (dependent) function
#Source the .cpp file
Rcpp::sourceCpp("lambdaLoop2.cpp") #This function updates the lambda of all documents once
source("tolTest.R") #This function will update all of the lambdas to a tolerance of 1e-15
firstYearsOutput<-tolTest(HIT2, lambda, DocId,2000) #completes 2000 iterations to make the lambdas converge

#STAN outputs: Read in the outputs
apiTest<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/BradelyTerryModelFinal/apiTest.csv")
library(rstan)
load("fitExperiment2.7")
post.lambda = summary(fitExperiment2.7)$summary[paste0('a[',1:50,']'),'mean'] #These are the outputs from STAN
fifthYearOutput<-as.data.frame(cbind(apiTest$id,post.lambda))
colnames(fifthYearOutput)<-c("DocId", "Lambda")

#Compare the outputs of the two methods
cor(log(firstYearsOutput),fifthYearOutput$Lambda) #take the log of the outputs from the Rcpp functions because the original Bradley Terry model was exponentiated
plot(log(firstYearsOutput),fifthYearOutput$Lambda) #Visual representation of the correlation


#=============================================================================================
# Bonus feature: Compare the Rcpp (dependent) functions/outputs and orginal (slow) R functions
#=============================================================================================

#The Rcpp functions were based on the R functions. 
#Therefore, if there is something wrong with the Rcpp functions when you continue with the package,
#the comparison of the Rcpp function to the original R function may be illuminating.

#Compare the functions that update the lambda for one document one time
source("bradleyTerryRFunctions.R")
bradleyterry(a=1,b=1,id=4969,lambda,HIT2) #update Document 4969
source("dataOrganizer.R")
testDF<-dataReorganizer(4969, lambdaDF = lambda, hitDF = HIT2) #organize data in order to run posterior lambda 
posteriorlambda(testDF, lambda$Lambda[1], 1,1) #update Document 4969, which corresponds with lambda$Lambda[1]

#Compare the functions that update the lambda for all documents once
btOneUpdate<-bradleyterry.multid(1,1, id=DocId, lambda, HIT2)
cppOneUpdate<-lambdaLoop2(HIT2, DocId, HIT2, lambda$Lambda)
btOneUpdate$Lambda-cppOneUpdate[-1] #The outputs of the two versions are not exactly the same,
                                    # but the differences are negligible

