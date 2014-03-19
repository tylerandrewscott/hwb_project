sd<-read.csv("Google Drive/Puget Sound/bakerclass_pilot.csv")
head(sd)
sd[,2]

cor(sd, use="everything",method=c("spearman"))


levels(sd[,2])<-c("16-21","22-29","30-39","40-49","50-59")
levels(sd[,3])<-c("tac","pierce","balance","far")
sd<-sd[,c(4:18)]
for(i in 1:ncol(sd)){
	sd[,i]<-as.numeric(sd[,i])
}
for(i in 1:ncol(sd)){
	sd[,i]<-as.character(sd[,i])
}
head(sd)
levels(sd[,1])<-c("1","1-3","4-10",">10")
levels(sd[,2])<-c(2,1,0,-1,-2)
levels(sd[,3])<-c(2,1,0,-1,-2)
levels(sd[,4])<-c(2,1,0,-1,-2)
levels(sd[,5])<-c(1,.75,.5,.25,0)
levels(sd[,6])<-c(1,.75,.5,.25,0)
levels(sd[,7])<-c(5,2,1,.25,0)
levels(sd[,8])<-c(5,2,1,.25,0)
levels(sd[,9])<-c(0,1,2,3,4)
levels(sd[,10])<-c(4,3,2,1)
levels(sd[,11])<-c(0,1,3,12,52)
levels(sd[,12])<-c(0,1,3,12,52)
levels(sd[,13])<-c(0,1,3,12,52)
levels(sd[,14])<-c(1,.66,.5,.33,0)
levels(sd[,15])<-c(1,.66,.5,.33,0)

out<-sd
for(i in 1:ncol(out)){
	out[,i]<-as.double(out[,i])
}
head(out)


out<-sd
for(i in 1:ncol(out)){
out[,i]<-ordered(out[,i])
}



matmaker<-function(a,b,c,d,e,f,g,h,i,j,k,l,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2
){
matrix(c(rep(1,12),rep(4,12),a,b,c,d,e,f,g,h,i,j,k,l,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2),nrow=12,ncol=5)
}
matmaker(1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12)

mexam<-matrix(nrow)
mexam
possibleqs<-c(2,3,5,6,7,8,9,10,11,12,13,14)
moderun<-runmincordif(out,4,2,12,5,possibleqs,setvalues=c(1,4))


runmincordif<-function(data,howmanyobs,howmanytries,nblocks,qsperblock,possibleqs,setvalues){
	#data is a data frame containing some sample data
	#howmanyobs is a numeric digit describing how many observations from the sample you would like to simulate for each block
	#howmanytries is a numeric digit to specify how many different combinations you would like the program to try
	#nblocks is the number of blocks you would like to run
	#qsperblock is how many questions in each block
	#possiblqs is a vector of the column numbers for your data frame that contain questions that need to be simulated. MUST be any column not including setvalues.
	#setvalues is a numeric vector for the column numbers for questions that will be asked in every survey.
out<-data
results<-vector(length=howmanytries)
require(mi)
matrixes<-list()
num<-howmanyobs
possibleqs
matrix<-matrix(nrow=nblocks,ncol=qsperblock)
for(p in 1:length(setvalues)){
matrix[,p]<-setvalues[p]
}
start<-length(setvalues)+1
matrix[,start]<-possibleqs
for(k in 1:howmanytries){
for(i in 1:nblocks){ 
	for(q in c(start+1):qsperblock){
s1<-subset(possibleqs, possibleqs!=matrix[i,q-1])
s1<-subset(s1, s1!=matrix[i,q-2])
matrix[i,q]<-sample(s1,1,replace=TRUE)
}
}
len<-num*nblocks
matrixes[[k]]<-matrix
block1<-out[1:len,]
block1[1:len,1:ncol(block1)]<-NA
m<-matrix[1,]
draw<-c(sample(1:nrow(out),num, replace=TRUE))
df<-out[draw,m]
rownames(df)<-c(1:num)
block1[1:num,m]<-df
head(block1);head(df)
for(i in 2:12){
	beg<-1+(num*(i-1))
	end<-num+(num*(i-1))
	draw<-c(sample(1:nrow(out),num, replace=TRUE))
	m<-matrix[i,]
	df<-out[draw,m]
	df
	head(df)
	block1[c(beg:end),m]<-df
}
dfout<-mi(block1,n.iter=10, n.imp=3, rand.imp.method="bootstrap", add.noise=noise.control(method="reshuffling", K=1))
da<-mi.completed(dfout)
da<-da[1]
da<-data.frame(da)
for(i in 1:ncol(da)){
	da[,i]<-as.numeric(da[,i])
}
for(i in 1:ncol(out)){
	out[,i]<-as.numeric(out[,i])
}
made<-cor(da, use="pairwise.complete.obs", method="spearman")
made<-made[1:14,1:14]
test<-cor(out, use="pairwise.complete.obs", method="spearman")
test<-test[1:14,1:14]
result<-sum(abs(test-made))
results[k]<-result
}
print(min(results))
print(matrixes[which(results==min(results))])
}
