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


runmincordif<-function(data,howmanyobs,howmanytries,nblocks,qsperblock,screeners=c(1,4),numqs=15,setvalues){
  #data is a data frame containing some sample data
  #howmanyobs is a numeric digit describing how many observations from the sample you would like to simulate for each block
  #howmanytries is a numeric digit to specify how many different combinations you would like the program to try
  #nblocks is the number of blocks you would like to run
  #qsperblock is how many questions in each block
  #possiblqs is a vector of the column numbers for your data frame that contain questions that need to be simulated. MUST be any column not including setvalues.
  #setvalues is a numeric vector for the column numbers for questions that will be asked in every survey.
  results<-vector(length=howmanytries)
  possibleqs<-seq(1,numqs,1)[-c(screeners)]
  require(mi)
  matrixes<-list()
  num<-howmanyobs

  qmat<-matrix(nrow=nblocks,ncol=qsperblock)
  for(p in 1:length(setvalues)){
    qmat[,p]<-setvalues[p]
  }

  qmat[,(length(setvalues)+1)]<-sample(possibleqs,replace=T)
    
  for(k in 1:howmanytries){
    for(i in 1:nblocks){ 
      for(q in (length(setvalues)+2):qsperblock){
        s1<-possibleqs[possibleqs %in% qmat[i,q-1]==FALSE]
        qmat[i,q]<-sample(s1,1)
      }
    }

    len<-num*nblocks
    matrixes[[k]]<-qmat
    block1<-data[1:len,]
    block1[1:len,1:ncol(block1)]<-NA
    m<-qmat[1,]
    draw<-c(sample(1:nrow(data),num, replace=TRUE))
    
    df<-data[draw,m]
    rownames(df)<-c(1:num)
    block1[1:num,m]<-df
    for(i in 2:nblocks){
      beg<-1+(num*(i-1))
      end<-num+(num*(i-1))
      draw<-c(sample(1:nrow(data),num, replace=TRUE))
      m<-qmat[i,]
      df<-data[draw,m]
      block1[c(beg:end),m]<-df
    }

    dfdata<-mi(block1,n.iter=10, n.imp=3, rand.imp.method="bootstrap", add.noise=noise.control(method="reshuffling", K=1))
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
  }
  print(min(results))
  print(matrixes[which(results==min(results))])
}
