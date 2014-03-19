setwd('//Users/TScott/hwb_project')
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

moderun<-runmincordif(out,4,2,12,5,possibleqs,setvalues=c(1,4))

screeners <- c(1,4)
numqs<- 15


#GENERATE POTENTIAL QUESTION BLOCK MATRICES
possibleqs<-seq(1,numqs,1)[-c(screeners)]

possibleqs

runmincordif(data=sd,howmanyobs=10,howmanytries=1,nblock=5,qsperblock=5,screeners=c(1,4),numqs=15)

nblocks=12;qsperblock=5;screeners=c(1,4);numqs=15;maxoccurence=3

#SIMULATE A BLOCK DESIGN
makedesign <- function(nblocks=12,qsperblock=5,screeners=c(1,4),numqs=15,maxoccurence=8)
{design<-matrix(0,ncol=qsperblock,nrow=nblocks)
  for(p in 1:length(screeners)){design[,p]<-screeners[p]}
#generate sample list
sample.list<-rep(possibleqs,each=maxoccurence)
for(i in 1:nblocks){ 
  for(q in (length(screeners)+1):ncol(design)){
    possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<maxoccurence]
    s1<-possibleqs[possibleqs %in% design[i,]==FALSE]
    s<-sample(s1,1)
    design[i,q]<-s}}
  return(design)}

#GENERATE MULTIPLE BLOCK DESIGNS
multdesigns <- function(ndesigns=2,nblocks=12,qsperblock=5,screeners=c(1,4))
{
  designs <- list()
  for (i in 1:ndesigns)
  {
    designs[[i]]<-as.data.frame(makedesign(nblocks,qsperblock,screeners))
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


#BUILD LIST OF FAKE DATA
temp<-multdesigns()

temp1<-lapply(1:length(temp), function(x) fakesample(design=temp[[x]]))

temp2<-mclapply(1:length(temp1), function(x) mi(object=temp1[[x]],n.iter=5),)



detach(library='parallel')
library(parallel)
require(multicore)
library(dplyr)

temp<-fakesample(data=sd,design=test)


lapply(fakelist,mi,n.iter=10,n.imp=3,rand.imp.method='bootstrap',add.noise=noice.control(method='reshuffling',K=1))
length(fakelist)


da<-mi.completed(dfdata)
da<-da[1]
da<-data.frame(da)
for(i in 1:ncol(da)){
  da[,i]<-as.numeric(da[,i])
}
for(i in 1:ncol(data)){
  data[,i]<-as.numeric(data[,i])
}
made<-cor(da, use="pairwise.complete.obs", method="spearman")
made<-made[1:numqs,1:numqs]
test<-cor(data, use="pairwise.complete.obs", method="spearman")
test<-test[1:numqs,1:numqs]
result<-sum(abs(test-made))
results[k]<-result


