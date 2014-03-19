#setwd('//Users/TScott/hwb_project')
setwd('C:/Users/tscott1/Documents/GitHub/hwb_project')
rm(list=ls())

sd<-read.csv("bakerclass_pilot.csv")


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

#Q3: How long have you lived in PS
levels(sd[,1])<-c("1","1-3","4-10",">10")
#Q4: I am attached to Puget Sound region
levels(sd[,2])<-c(2,1,0,-1,-2)
#Q5: I identify with Puget Sound region
levels(sd[,3])<-c(2,1,0,-1,-2)
#Q6 How satisfied with life as whole
levels(sd[,4])<-c(2,1,0,-1,-2)
#Q7 How frequently feel inspired in nature
levels(sd[,5])<-c(1,.75,.5,.25,0)
#Q8 How frequently nature reduces stress
levels(sd[,6])<-c(1,.75,.5,.25,0)
#Q9: How often outdoor in Winter
levels(sd[,7])<-c(5,2,1,.25,0)
#Q10: How often outdoor in summer
levels(sd[,8])<-c(5,2,1,.25,0)
#Q11: How often gather resources
levels(sd[,9])<-c(0,1,2,3,4)
#Q12: able to harvest enough resources
levels(sd[,10])<-c(4,3,2,1)
#Q13: participate in cultural activties
levels(sd[,11])<-c(0,1,3,12,52)
#Q14: participate in environmental stewardship
levels(sd[,12])<-c(0,1,3,12,52)
#Q15: work with others in community
levels(sd[,13])<-c(0,1,3,12,52)
#Q16: trust local policy makers
levels(sd[,14])<-c(1,.66,.5,.33,0)
#Q17: trust experts
levels(sd[,15])<-c(1,.66,.5,.33,0)

out<-sd
for(i in 1:ncol(out)){
	out[,i]<-as.double(out[,i])
}

out<-sd
for(i in 1:ncol(out)){
out[,i]<-ordered(out[,i])
}



#SIMULATE A BLOCK DESIGN
makedesign <- function(nblocks=12,qsperblock=5,screeners=c(1,4),numqs=15,maxoccurence=8)
{design<-matrix(0,ncol=qsperblock,nrow=nblocks)
  for(p in 1:length(screeners)){design[,p]<-screeners[p]}
#generate sample list
for(i in 1:nblocks){ 
  for(q in (length(screeners)+1):ncol(design)){
    possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<maxoccurence]
    s1<-possibleqs[possibleqs %in% design[i,]==FALSE]
    s<-sample(s1,1)
    design[i,q]<-s}}
  return(design)}


#GENERATE MULTIPLE BLOCK DESIGNS
multdesigns <- function(ndesigns=2,nblocks=12,qsperblock=5,screeners=c(1,4),numqs=15,maxoccurence=8)
{
  designs <- list()
  for (i in 1:ndesigns)
  {
    designs[[i]]<-as.data.frame(makedesign(nblocks,qsperblock,screeners,numqs,maxoccurence))
  }
  return(designs)
}

#Build Fake Data for 1 design
fakesample<-function(design,data=sd,nblocks=12,qsperblock=5,numinblock=100)
{
total<-numinblock*nblocks
allblocks<-data.frame(matrix(NA,nrow=total,ncol=ncol(data)))
for(i in 1:nblocks)
{
  beg<-1+(numinblock*(i-1))
  end<-numinblock+(numinblock*(i-1))
  #sample from observed complete individuals
  peopletodraw<-c(sample(1:nrow(data),numinblock,replace=TRUE))
  #which questions to draw
  mm<- c(as.numeric(design[i,]))
  #pull from observed data, questions in mm
  df<-data[peopletodraw,mm]
  allblocks[beg:end,mm]<-df
}
return(allblocks)
}

library(snow)
require(mi)
library(lme4)

#BUILD LIST OF FAKE DATA
#generate 16 designs
temp<-multdesigns(ndesigns=500)
#generate 16 fake datasets based upon 16 designs
temp1<-lapply(1:length(temp), function(x) fakesample(design=temp[[x]]))

#run multiple imputation on each fake dataset
require(doParallel)
cl<-makeCluster(16)
registerDoParallel(cl)
multimputes<-foreach(i =1:length(temp1),.packages=c('mi')) %dopar% mi(object=temp1[[i]],n.iter=30)
stopCluster(cl)


#run imputation on each fake dataset
da<-lapply(1:length(multimputes),function(x) mi.completed(multimputes[[x]]))

#make each imputed dataset a data frame, select one of three imputed sets randomly for each imputation run
da1<-lapply(1:length(da),function(x) as.data.frame(da[[x]][sample(1:3,1)]))

#make values numeric
da2<-lapply(1:length(da1),function(x) apply(da1[[x]],2,as.numeric))

numdat<-apply(sd,2,as.numeric)


fakecor<-lapply(1:length(da2),function(x) cor(da2[[x]], use='pairwise.complete.obs',method='spearman'))

obscor<-cor(numdat,use='pairwise.complete.obs',method='spearman')

#compar faked to observed (toss out values where NA for one question)
cordiff.score<-lapply(1:length(fakecor),function(x) sum(abs(obscor-fakecor[[x]])))
save.image('searchfordesign.RData')


