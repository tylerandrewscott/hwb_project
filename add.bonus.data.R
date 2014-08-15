rm(list=ls())
require(xlsx)
require(dplyr)
require(plyr)
require(ggmap)
require(doParallel)
require(reshape2)
setwd('H:/gcs_data')

first10 = read.xlsx('BONUS_DATA_FIRST10.xls',sheetIndex=3)
last5 = read.xlsx('BONUS_DATA_LAST5.xls',sheetIndex=3)
#remove extra geography tag
last5$Geography = paste(gsub('US-WEST-WA-','',last5$Geography),'Washington',sep=', ')
first10$Geography = paste(gsub('US-WEST-WA-','',first10$Geography),'Washington',sep=', ')
last5$Block = 'BONUS5'
first10$Block = 'BONUS10'


design = read.csv('H:/gcs_data/bestdesign.csv',row.names=1)

first10.design = data.frame(matrix(0,ncol=10,nrow=1))
colnames(first10.design) = paste('S',1:10,sep='')
first10.design[1,]=
c('Q1:Howlong','Q4:Howsatisfied','Q2:PSattach','Q3:PSident','Q5:natinspire',
'Q6:natstress','Q7:wintrec','Q8:sumrec','Q9:resgather','Q10:resaccess')

last5.design = data.frame(matrix(0,ncol=7,nrow=1))
colnames(last5.design) = paste('S',1:7,sep='')
last5.design[1,] = c('Q1:Howlong','Q4:Howsatisfied','Q11:cultureact','Q12:stewardact','Q13:familyoutside',
  'Q14:trustpolicy','Q15:trustexperts')


last5.design$Block = 'BONUS5'
first10.design$Block = 'BONUS10'


#Issues with questions
last51 = join(last5,last5.design,type='full')
first101 = join(first10,first10.design,type='full')

first101$S1 = gsub(':','',first101$S1)
first101$S2 = gsub(':','',first101$S2)
first101$S3 = gsub(':','',first101$S3)
first101$S4 = gsub(':','',first101$S4)
first101$S5 = gsub(':','',first101$S5)
first101$S6 = gsub(':','',first101$S6)
first101$S7 = gsub(':','',first101$S7)
first101$S8 = gsub(':','',first101$S8)
first101$S9 = gsub(':','',first101$S9)
first101$S10 = gsub(':','',first101$S10)

last51$S1 = gsub(':','',last51$S1)
last51$S2 = gsub(':','',last51$S2)
last51$S3 = gsub(':','',last51$S3)
last51$S4 = gsub(':','',last51$S4)
last51$S5 = gsub(':','',last51$S5)
last51$S6 = gsub(':','',last51$S6)
last51$S7 = gsub(':','',last51$S7)

colnames(first101)[10:19] = paste('Answer.',1:10,sep='')
colnames(last51)[10:16] = paste('Answer.',1:7,sep='')


require(plyr)
require(rgdal)
require(reshape2)
require(maptools)
require(lubridate)
require(sp)
require(gridExtra)
require(lattice)
require(ggplot2)

write.csv(first101,'H:/gcs_data/data.block.b1.csv')
write.csv(last51,'H:/gcs_data/data.block.b2.csv')

#######
#rm(list=ls())
constant.vec = c(colnames(first10)[1:9],'Block')
first10.long = melt(first101,id.vars=constant.vec)
last5.long = melt(last51,id.vars=constant.vec)

temp = first10.long
temp.resp = temp[grep('Response.Time',temp$variable),]
temp.resp = arrange(temp.resp,User.ID,variable)
temp.resp$uq = paste(temp.resp$User.ID, gsub('Response.Time..','',temp.resp$variable) ,sep='_')
colnames(temp.resp)[(ncol(temp.resp)-2):(ncol(temp.resp)-1)] = c('drop','Response.Time')
temp.resp = (temp.resp[,-(ncol(temp.resp)-2)])

temp.ans = temp[grep('Answer',temp$variable),]
temp.ans = arrange(temp.ans,User.ID,variable)
temp.ans$uq = paste(temp.ans$User.ID, gsub('Answer.','',temp.ans$variable) ,sep='_')
colnames(temp.ans)[(ncol(temp.ans)-2):(ncol(temp.ans)-1)]  = c('drop','Answer')
temp.ans = temp.ans[,-(ncol(temp.ans)-2)]

temp.quest = temp[grep('S',temp$variable),]
temp.quest = arrange(temp.quest, User.ID, variable)
temp.quest$uq = paste(temp.quest$User.ID, gsub('S','',temp.quest$variable) ,sep='_')
colnames(temp.quest)[(ncol(temp.quest)-2):(ncol(temp.quest)-1)] = c("Slot" ,'Question')
df.temp1 = join_all(dfs=list(temp.ans,temp.resp,temp.quest))
write.csv(df.temp1,'H:/gcs_data/b1_long_data.csv')

####
temp = last5.long
temp.resp = temp[grep('Response.Time',temp$variable),]
temp.resp = arrange(temp.resp,User.ID,variable)
temp.resp$uq = paste(temp.resp$User.ID, gsub('Response.Time..','',temp.resp$variable) ,sep='_')
colnames(temp.resp)[(ncol(temp.resp)-2):(ncol(temp.resp)-1)] = c('drop','Response.Time')
temp.resp = (temp.resp[,-(ncol(temp.resp)-2)])

temp.ans = temp[grep('Answer',temp$variable),]
temp.ans = arrange(temp.ans,User.ID,variable)
temp.ans$uq = paste(temp.ans$User.ID, gsub('Answer.','',temp.ans$variable) ,sep='_')
colnames(temp.ans)[(ncol(temp.ans)-2):(ncol(temp.ans)-1)]  = c('drop','Answer')
temp.ans = temp.ans[,-(ncol(temp.ans)-2)]

temp.quest = temp[grep('S',temp$variable),]
temp.quest = arrange(temp.quest, User.ID, variable)
temp.quest$uq = paste(temp.quest$User.ID, gsub('S','',temp.quest$variable) ,sep='_')
colnames(temp.quest)[(ncol(temp.quest)-2):(ncol(temp.quest)-1)] = c("Slot" ,'Question')
df.temp2 = join_all(dfs=list(temp.ans,temp.resp,temp.quest))
write.csv(df.temp2,'H:/gcs_data/b2_long_data.csv')

t1 = read.csv('long_data.csv')
t2 = read.csv('b1_long_data.csv')
t3 = read.csv('b2_long_data.csv')
t1$EXTRA = 0
t2$EXTRA = 1
t3$EXTRA = 1
all.wide = join_all(dfs = list(t1,t2,t3),type='full')
write.csv(all.wide,'long_data.2.csv')

#####################
#rm(list=ls())

original.mat = read.csv('Response.Matrix.csv',row.names=1)
r = nrow(original.mat)+length(unique(df.temp2$User.ID))+length(unique(df.temp1$User.ID))

tmat = as.data.frame(matrix(NA,nrow=r,ncol=15))
rownames(tmat)= c(as.character(rownames(original.mat)),
                 as.character(unique(df.temp2$User.ID)),
                 as.character(unique(df.temp1$User.ID)))
colnames(tmat) = colnames(original.mat)

for (i in 1:nrow(original.mat))
{
  for(j in 1:ncol(original.mat))
  {
    tmat[i,j] = as.character(original.mat[i,j])
  }
}


for (i in 1:nrow(df.temp1))
{
 tmat[match(df.temp1$User.ID[i],rownames(tmat)),
  match(df.temp1$Question[i],colnames(tmat))] = as.character(df.temp1$Answer[i])
}


for (i in 1:nrow(df.temp2))
{
  tmat[match(df.temp2$User.ID[i],rownames(tmat)),
       match(df.temp2$Question[i],colnames(tmat))] = as.character(df.temp2$Answer[i])
}


tmat = as.data.frame(tmat)


#########

q2levels = c("Strongly disagree","Disagree",
             "Neither agree nor disagree",
             "Agree" ,"Strongly agree"
             tmat$Q2PSattach = ordered(tmat$Q2PSattach,levels=q2levels)
             
             q3levels = c("Strongly disagree","Disagree",
                          "Neither agree nor disagree",
                          "Agree" ,"Strongly agree"
                          tmat$Q3PSident = ordered(tmat$Q3PSident,levels=q3levels)
                          
                          q4levels = c("Extremely dissatisfied","Somewhat dissatisfied",
                                       "Neither satisfied nor dissatisfied",
                                       "Somewhat satisfied",
                                       "Extremely satisfied")           
                          tmat$Q4Howsatisfied = ordered(tmat$Q4Howsatisfied,levels=q4levels)            
                          
                          
                          q5levels = c("Almost never or never"  ,"Some of the time (about a third)" ,
                                       "About half of the time"    ,
                                       "Most of the time (about two-thirds)",
                                       "Almost always or always")
                          tmat$Q5natinspire = ordered(tmat$Q5natinspire,levels=q5levels)
                          
                          
                          q6levels = c("Almost never or never"  ,"Some of the time (about a third)" ,
                                       "About half of the time"    ,
                                       "Most of the time (about two-thirds)",
                                       "Almost always or always")
                          tmat$Q6natstress = ordered(tmat$Q6natstress,levels=q6levels)
                          
                          
q7levels = c( "Rarely or never (less than 1 time a month)",
                          "About 1-3 times a month" ,
                          "About 1 time a week",
                          "Several times a week (about 3 times a week)",
                          "Almost every day (at least 5 times a week)")
                          tmat$Q7wintrec = ordered(tmat$Q7wintrec,levels=q7levels)
                          
q8levels =  c("Rarely or never (less than 1 time a month)",
 "About 1-3 times a month" ,
 "About 1 time a week",
 "Several times a week (about 3 times a week)",
 "Almost every day (at least 5 times a week)")
 tmat$Q8sumrec = ordered(tmat$Q8sumrec,levels=q8levels)
                          
                          q9levels = c( 
                            "Never"                                      , 
                            "Rarely (once or twice during the season)" ,
                            "Occasionally (several times a season)" ,
                            "Regularly (most of the season)",
                            "Constantly (almost every day in the season)")
                          tmat$Q9resgather = ordered(tmat$Q9resgather,q9levels)
                          
                          q12levels =  c("Never", "Rarely (at least once or twice)",
                                         "Occasionally (at least three or four times)",
                                         "Regularly (at least once a month)",
                                         "Constantly (at least once a week)" )
                          tmat$Q12stewardact = ordered(tmat$Q12stewardact,levels=q12levels)   
                          
                          q10levels = c("I don't like to gather or hunt"   ,         
                                        "Rarely (access less than 30% of the time)"  ,     
                                        "Sometimes have access" ,                    
                                        "Usually (access more than 70% of the time)")
                          tmat$Q10resaccess = ordered(tmat$Q10resaccess,levels=q10levels) 
                          
                          q11levels = c("Never"   ,"Rarely (at least once or twice)" ,                                   
                                        "Occasionally (at least three or four times)",         
                                        "Regularly (at least once a month)",
                                        "Constantly (at least once a week)")
                          tmat$Q11cultureact = ordered(tmat$Q11cultureact,levels=q11levels)
                          
                          q13levels = c(
                            "Rarely or never (less than 1 time a month)",
                            "About 1-3 times a month" ,                   
                            "About 1 time a week" ,
                            "Several times a week (about 3 times a week)",
                            "Almost every day (at least 5 times a week)" )
                          tmat$Q13familyoutside = ordered(tmat$Q13familyoutside,levels=q13levels)
                          
                          
                          q14levels = c("Almost never or never"  ,            
                                        "Some of the time (about a third)",
                                        "About half of the time" ,
                                        "Most of the time (about two-thirds)",
                                        "Almost always or always")
                          tmat$Q14trustpolicy = ordered(tmat$Q14trustpolicy,levels=q14levels)
                          
q15levels = c("Almost never or never"  ,            
    "Some of the time (about a third)",
    "About half of the time" ,
  "Most of the time (about two-thirds)",
    "Almost always or always")
tmat$Q15trustexperts = ordered(tmat$Q15trustexperts,levels=q15levels) 
#Code responses as ordinal factors
#####################

write.csv(tmat,'Response.Matrix.2.csv')


