#setwd('//Users/TScott/hwb_project')
setwd('C:/Users/tscott1/Documents/GitHub/hwb_project')
rm(list=ls())

#SIMULATE A BLOCK DESIGN
makedesign <- function(nblocks,qsperblock,screeners,numqs,maxoccurence,blocksim)
{
repeat{
design<-matrix(0,ncol=qsperblock,nrow=nblocks)
 for(p in 1:length(screeners)){design[,p]<-screeners[p]}
 #generate sample list
 for (i in 1:1)
 {for(q in (length(screeners)+1):ncol(design)){ 
 possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<maxoccurence]
 #is question already in design?
 s1<-c(possibleqs[possibleqs %in% design[1,]==FALSE])
 s<-ifelse(length(s1)>1,sample(s1,1),s1) 
 design[i,q]<-s}}
 for (i in 2:nblocks)
 {for (q in (length(screeners)+1):ncol(design)){
    repeat{
   possibleqs<-seq(1,numqs,1)[tabulate(design,nbins=numqs)<maxoccurence]
   #is question already in design?
   s1<-c(possibleqs[possibleqs %in% design[i,]==FALSE])
   s<-ifelse(length(s1)>1,sample(s1,1),s1) 
   s<-ifelse(is.na(s)==TRUE,which.min(tabulate(design))[1],s)
   design[i,q]<-s
   if(all((colSums(sapply(1:(i-1),function(x) design[x,(length(screeners)+1):qsperblock] %in% 
            design[i,(length(screeners)+1):qsperblock])))<=blocksim))
   {break}
    }}
 }
if(all(is.na(design)==FALSE)& 
     all((sapply(1:nblocks, function(x) all(tabulate(design[x,])<2)))==TRUE)&
     all(tabulate(design,nbins=numqs)>0))
{break}}
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



