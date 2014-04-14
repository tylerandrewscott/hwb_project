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

qlabels<-c('Q1:Howlong','Q2:PSattach','Q3:PSident','Q4:Howsatisfied','Q5:natinspire','Q6:natstress','Q7:wintrec',
  'Q8:sumrec','Q9:resgather','Q10:resable','Q11:cultureact','Q12:stewardact','Q13:workothers','Q14:trustpolicy',
  'Q15:trustexperts')

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

#attach('runmiresults.RData')


require(snow)
require(mi)
require(lme4)
require(plyr)
#BUILD LIST OF FAKE DATA
#generate 2000 designs

cl<-makeCluster(8)
registerDoParallel(cl)
tempA<-foreach(i=1:8,.packages='plyr') %dopar%  rlply(.n=100,.expr=makedesign(nblocks=24,qsperblock=6,screeners=c(4,1),
                                              numqs=15,maxoccurence=8,
                                              blocksim=4,minask=7,verbose=FALSE,
                                              obs.all.combins=FALSE))
stopCluster(cl)


save.image('designs_24block.RData')



tempAA<-unlist(tempA,recursive=F)

#generate 16 fake datasets based upon 16 designs
temp1<-lapply(1:length(tempAA), function(x) fakesample(design=tempAA[[x]],data=sd,numinblock=150))

#make mi.info objects for each fake dataset
temp2 <- llply(.data=temp1, .fun= mi.info) # .parallel=TRUE, .paropts= list(.packages=c('mi')))


#change colnames of fake data to 'X1' etc. 
for (i in 1:length(temp1))
{
  colnames(temp1[[i]])<-paste0('X',seq(1,15,1))
}



require(doParallel)
cl<-makeCluster(16)
registerDoParallel(cl)
multimputesA<-
  llply(.data=temp1, .fun=mi,
        n.iter=20,check.coef.convergence=TRUE,add.noise=FALSE,max.minutes=8,
        .parallel=TRUE,.paropts = list(.packages='mi',.verbose=TRUE),.progress='tk')

stopCluster(cl)
da<-llply(.data=multimputesA, .fun=mi.completed,.progress='text')
#make each imputed dataset a data frame
da1<-lapply(1:length(da),function(x) as.data.frame(da[[x]][1]))
#make values numeric
da2<-lapply(1:length(da1),function(x) apply(da1[[x]],2,as.numeric))
numdat<-apply(sd,2,as.numeric)
fakecor<-llply(.data=da2,.fun=cor,use='pairwise.complete.obs',method='spearman',.progress='text')
obscor<-cor(numdat,use='pairwise.complete.obs',method='spearman')
#compar faked to observed (toss out values where NA for one question)
cordiff.score<-lapply(1:length(fakecor),function(x) sum(abs(obscor-fakecor[[x]])))
save.image('searchfordesign.24blocks.Sim3.RData')


cor(numdat,use='pairwise.complete.obs',method='spearman')

min(unlist(cordiff.score))

sum(unlist(cordiff.score)<16.5)



best <- which.min(unlist(cordiff.score))
small3 <- which(unlist(cordiff.score)<16.5)

#best
apply(tempAA[[best]],1,sort)
#9 (gather resources) and 10 (able to harvest) #2
#3 (identity) and 5 (inspiration) #2
#2 (attached) and 5 (inspiration) #
#2 (attached) and 3 (identity) #2
#14 (trust policymakers) and 15 (trust experts) #2
#12 (environmental stewardship) and/or 14, 15  #12/14 = 3, #12/15 = 1

emp<-as.vector(NULL)
pp<-t(apply(bd,1,sort))
for (u in 1:nrow(pp))
{for (v in 1:ncol(pp)){if (v != ncol(pp))
{emp<-append(emp,(paste(pp[u,v],pp[u,(v+1):ncol(pp)])))}}}
sort(table(emp))


2 5  3 12  3 14  7 10  8 10  8 15



for (i in 1:nrow(bd))
{
  for( j in 1:ncol(bd))
  {
    (tc[bd[i,j],bd[i,]])
  }
}



cor(numdat,use='pairwise.complete.obs',method='spearman')

tc<-cor(numdat,use='pairwise.complete.obs',method='spearman')
all(
  paste(t(combn(15,2))[,1],t(combn(15,2))[,2])
  
  %in% emp)

tabulate(tempAA[[best]])
testswitch<-tempAA[[best]]
testswitch[1,5]<-5

cordiff.score[[small3[2]]]

apply(tempAA[[small3[2]]],1,sort)
#9 (gather resources) and 10 (able to harvest) #9/10 = 2
#3 (identity) and 5 (inspiration) #3/5 = 2
#2 (attached) and 5 (inspiration) #2/5 = 4
#2 (attached) and 3 (identity) #2/3 = 3
#14 (trust policymakers) and 15 (trust experts) #14/15 = 3
#12 (environmental stewardship) and/or 14, 15  #12/14 = 1, #12/15 = 1, #14/15 = 3


apply(tempAA[[small3[1]]],1,sort)
#9 (gather resources) and 10 (able to harvest) #9/10 = 2
#3 (identity) and 5 (inspiration) #3/5 = 1
#2 (attached) and 5 (inspiration) #2/5 = 2
#2 (attached) and 3 (identity) #2/3 = 2
#14 (trust policymakers) and 15 (trust experts) #14/15 = 2
#12 (environmental stewardship) and/or 14, 15  #12/14 = 1, #12/15 = 2, #14/15 = 2

mm<-apply(sd,2,as.numeric)
nn<-as.matrix(mm)
nn
cor(nn,na.rm=TRUE)
?cor

cormatrix(sd)
as.numeric(sd)
cor(as.numeric(sd))


tabulate(tempAA[best]])
bd<-tempAA[[small3[2]]]
qlabels
bdq<-bd
for (i in 1:nrow(bd))
{
  for (q in 1:ncol(bd))
  {
    
    bdq[i,q]<-qlabels[bd[i,q]]
  }
}

write.csv(as.data.frame(bdq),'bestdesign.csv')
#2nd place
secbest<-tempA[[small3[2]]]
#3rd place
thirdbest<-tempA[[small3[3]]]
tabulate(thirdbest)
design.vector<-tempA
save.image('searchfordesign.36blocks.Sim3.RData')

