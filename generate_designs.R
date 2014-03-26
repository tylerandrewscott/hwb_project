rm(list=ls())
setwd('C:/Users/tscott1/Documents/GitHub/hwb_project')
source('scott_sampling_functions.R')


sd<-read.csv("bakerclass_pilot.csv")
sd<-sd[-26,]
levels(sd[,2])<-c("16-21","22-29","30-39","40-49","50-59")
levels(sd[,3])<-c("tac","pierce","balance","far")

#remove extra columns
sd<-sd[,4:18]

#convert letters to numbers
for(i in 1:ncol(sd)){
  sd[,i]<-as.numeric(sd[,i])
}

#code numbers as strings
for(i in 1:ncol(sd)){
  sd[,i]<-as.character(sd[,i])
}

#code levels for variables

#Q1: How long have you lived in PS
levels(sd[,1])<-c("1","1-3","4-10",">10")
#Q2: I am attached to Puget Sound region
levels(sd[,2])<-c(2,1,0,-1,-2)
#Q3: I identify with Puget Sound region
levels(sd[,3])<-c(2,1,0,-1,-2)
#Q4 How satisfied with life as whole
levels(sd[,4])<-c(2,1,0,-1,-2)
#Q5 How frequently feel inspired in nature
levels(sd[,5])<-c(1,.75,.5,.25,0)
#Q6 How frequently nature reduces stress
levels(sd[,6])<-c(1,.75,.5,.25,0)
#Q7: How often outdoor in Winter
levels(sd[,7])<-c(5,2,1,.25,0)
#Q8: How often outdoor in summer
levels(sd[,8])<-c(5,2,1,.25,0)
#Q9: How often gather resources
levels(sd[,9])<-c(0,1,2,3,4)
#Q10: able to harvest enough resources
levels(sd[,10])<-c(4,3,2,1)
#Q11: participate in cultural activties
levels(sd[,11])<-c(0,1,3,12,52)
#Q12: participate in environmental stewardship
levels(sd[,12])<-c(0,1,3,12,52)
#Q13: work with others in community
levels(sd[,13])<-c(0,1,3,12,52)
#Q14: trust local policy makers
levels(sd[,14])<-c(1,.66,.5,.33,0)
#Q15: trust experts
levels(sd[,15])<-c(1,.66,.5,.33,0)

out<-sd
for(i in 1:ncol(out)){
  out[,i]<-as.double(out[,i])
}

out<-out
for(i in 1:ncol(out)){
  out[,i]<-ordered(out[,i])
}

sd<-out
colnames(sd)<-paste('Q',seq(1,15,1),sep='')


require(snow)
require(mi)
require(lme4)
require(plyr)
#BUILD LIST OF FAKE DATA
#generate 2000 designs
tempA<-rlply(.n=2000,
      .expr=makedesign(nblocks=12,qsperblock=6,screeners=c(4,1),numqs=15,maxoccurence=4,blocksim=4))

#generate 16 fake datasets based upon 16 designs
temp1<-lapply(1:length(tempA), function(x) fakesample(design=tempA[[x]],data=sd,numinblock=400))



temp2 <- llply(.data=temp1, .fun= mi.info) # .parallel=TRUE, .paropts= list(.packages=c('mi')))



cl<-makeCluster(16)
registerDoParallel(cl)
stopCluster(cl)

mi.info(temp1[[1]])


mi.info(temp1[[8]])
#run multiple imputation on each fake dataset
require(doParallel)


multimputesA<-foreach(i =1:length(temp1),
                      .packages=c('mi')) %dopar% mi(object=temp1[[i]],n.iter=20,
                                              max.minutes=8,check.coef.convergence=TRUE,
                                              add.noise=FALSE)
stopCluster(cl)
save.image('searchfordesign.sim3.RData')
temp1[[1]]
mi(object=temp1[[1]],n.iter=20,
   max.minutes=8,check.coef.convergence=TRUE,
   add.noise=FALSE)
?mi
#run imputation on each fake dataset
da<-lapply(1:length(multimputes),function(x) mi.completed(multimputes[[x]]))

#make each imputed dataset a data frame, select one of three imputed sets randomly for each imputation run
da1<-lapply(1:length(da),function(x) as.data.frame(da[[x]][1]))
#[sample(1:3,1)
#make values numeric
da2<-lapply(1:length(da1),function(x) apply(da1[[x]],2,as.numeric))

numdat<-apply(sd,2,as.numeric)


fakecor<-lapply(1:length(da2),function(x) cor(da2[[x]], use='pairwise.complete.obs',method='spearman'))

obscor<-cor(numdat,use='pairwise.complete.obs',method='spearman')

#compar faked to observed (toss out values where NA for one question)
cordiff.score<-lapply(1:length(fakecor),function(x) sum(abs(obscor-fakecor[[x]])))


which.max(unlist(cordiff.score))



save.image('searchfordesign.sim3.RData')

