#setwd('//Users/TScott/hwb_project')
setwd('C:/Users/tscott1/Documents/GitHub/hwb_project')
rm(list=ls())

#SIMULATE A BLOCK DESIGN
makedesign <- function(nblocks,qsperblock,screeners,numqs,maxoccurence,blocksim,minask,
                       verbose=FALSE,obs.all.combins=FALSE)
{
if(obs.all.combins==TRUE)
{  
  #norig<-nblocks
  #maxorig<-maxoccurence 
repeat{
  design<-matrix(0,ncol=qsperblock,nrow=nblocks)
  #if(fixed==0){nblocks = nblocks + 1
  #nblocks<-ifelse(nblocks>48,norig,nblocks)
  #maxoccurence <- nblocks / 3
  #maxoccurence<-ifelse(nblocks>48,maxorig,maxoccurence)}
  
if (length(screeners)>0)
{ for(p in 1:length(screeners)){design[,p]<-screeners[p]}}
 #generate sample list
 for (i in 1:1)
 {for(q in (length(screeners)+1):ncol(design)){ 
 possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<maxoccurence]

 #is question already in design?
 s1<-c(possibleqs[possibleqs %in% design[i,]==FALSE])
 s<-ifelse(length(s1)>1,sample(s1,1),s1) 
 design[i,q]<-s}}
 for (i in 2:nblocks)
 {
    repeat{
 for (q in (length(screeners)+1):ncol(design)){
   possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<min(maxoccurence,i)]
   #is question already in block?
   s1<-c(possibleqs[possibleqs %in% design[i,]==FALSE])
   s<-ifelse(length(s1)>1,sample(s1,1),s1) 
   design[i,q]<-s}
 if(all(colSums(sapply(1:(i-1),function(x) design[x,(length(screeners)+1):qsperblock] %in% 
                         design[i,(length(screeners)+1):qsperblock]))<=blocksim))
{break}}
 }
if(verbose==TRUE){print(design)}
emp<-as.vector(NULL)
pp<-t(apply(design,1,sort))
for (u in 1:nrow(pp))
{for (v in 1:ncol(pp)){if (v != ncol(pp))
    {emp<-append(emp,(paste(pp[u,v],pp[u,(v+1):ncol(pp)])))}}}

if(all(is.na(design)==FALSE)& 
     all((sapply(1:nblocks, function(x) all(tabulate(design[x,])<2)))==TRUE)&
     all(tabulate(design,nbins=numqs)>=minask) &
   all(paste(t(combn(numqs,2))[,1],t(combn(numqs,2))[,2])%in% emp))
#   & 
#     mean(paste(t(combn(numqs,2))[,1],t(combn(numqs,2))[,2])%in% emp)>per.comb.occur)       
{break}}
}

else 
{ 
  #norig<-nblocks
  #maxorig<-maxoccurence
  repeat{
    design<-matrix(0,ncol=qsperblock,nrow=nblocks)
    #if(fixed==0){nblocks = nblocks + 1
    #nblocks<-ifelse(nblocks>48,norig,nblocks)
    #maxoccurence <- nblocks / 3
    #maxoccurence<-ifelse(nblocks>48,maxorig,maxoccurence)}
    
    if (length(screeners)>0)
    { for(p in 1:length(screeners)){design[,p]<-screeners[p]}}
    #generate sample list
    for (i in 1:1)
    {for(q in (length(screeners)+1):ncol(design)){ 
      possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<maxoccurence]
      
      #is question already in design?
      s1<-c(possibleqs[possibleqs %in% design[i,]==FALSE])
      s<-ifelse(length(s1)>1,sample(s1,1),s1) 
      design[i,q]<-s}}
    for (i in 2:nblocks)
    {
      repeat{
        for (q in (length(screeners)+1):ncol(design)){
          possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<min(maxoccurence,i)]
          #is question already in block?
          s1<-c(possibleqs[possibleqs %in% design[i,]==FALSE])
          s<-ifelse(length(s1)>1,sample(s1,1),s1) 
          design[i,q]<-s}
        if(all(colSums(sapply(1:(i-1),function(x) design[x,(length(screeners)+1):qsperblock] %in% 
                                design[i,(length(screeners)+1):qsperblock]))<=blocksim))
        {break}}
    }
    if(verbose==TRUE){print(design)}
    emp<-as.vector(NULL)
    pp<-t(apply(design,1,sort))
    for (u in 1:nrow(pp))
    {for (v in 1:ncol(pp)){if (v != ncol(pp))
    {emp<-append(emp,(paste(pp[u,v],pp[u,(v+1):ncol(pp)])))}}}
    
    if(all(is.na(design)==FALSE)& 
         all((sapply(1:nblocks, function(x) all(tabulate(design[x,])<2)))==TRUE)&
         all(tabulate(design,nbins=numqs)>=minask))
      #   & 
      #     mean(paste(t(combn(numqs,2))[,1],t(combn(numqs,2))[,2])%in% emp)>per.comb.occur)       
    {break}}
}
  return(design)}    






#GENERATE MULTIPLE BLOCK DESIGNS
multdesigns <- function(ndesigns,nblocks,qsperblock,screeners,numqs,maxoccurence,blocksim)
{
  designs <- list()
  for (i in 1:ndesigns)
  {
    designs[[i]]<-as.data.frame(makedesign(nblocks,qsperblock,screeners,numqs,maxoccurence,blocksim))
  }
  return(designs)
}

#Build Fake Data for 1 design

fakesample<-function(design,data,numinblock)
{
  nb <- nrow(design)
  total<-numinblock*nb
  
  allblocks<-data.frame(matrix(NA,nrow=total,ncol=ncol(data)))
  
  for(i in 1:nb)
  {
    beg<-1+(numinblock*(i-1))
    end<-numinblock+(numinblock*(i-1))
    #sample from observed complete individuals
    whichpeople<-as.vector(sample(1:nrow(data),numinblock,replace=TRUE))
    #which questions to draw
    whichqs<- c(unlist(design[i,]))
    #pull from observed data, questions in mm
    df<-(data[whichpeople,whichqs])#;colnames(df)<-seq(1,ncol(design),1)
    allblocks[beg:end,whichqs]<-df
    rownames(allblocks)<-seq(1,nrow(allblocks),1);colnames(allblocks)<-seq(1,ncol(allblocks),1)
  }
  return(allblocks)
}



