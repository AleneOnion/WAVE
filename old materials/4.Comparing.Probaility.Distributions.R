# set the working directory 
setwd("C:/Rscripts/WAVE/output")

#read the data
test = read.csv("BAPunder5.Prob.Estimates")

test[,"probability"]=round(test[,"probability"]/.05)*.05

hi = test$bap >=7.5
high <- test[hi,]

nk = test$bap >=5
Slight <- test[nk,]
nk = test$bap <7.5
Slight<- test[nk,]

lo = test$bap <5
Low <- test[lo,]

#library for the count function
library(plyr)
HiC <- count(high,c("probability"))
names(HiC)[names(HiC)=="freq"]<-"High"

SlightC <- count(Slight,c("probability"))
names(SlightC)[names(SlightC)=="freq"]<-"Slight"

LoC <- count(Low,c("probability"))
names(LoC)[names(LoC)=="freq"]<-"Low"

#merge together
table <- merge(HiC,SlightC,by=c("probability"), all=TRUE)
table <- merge(table,LoC,by=c("probability"), all=TRUE)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(table,file="Low.distribution.csv",sep=",",row.names=FALSE)

#######################################################################################

#read the data
High = read.csv("BAPover7.5.Prob.Estimates.csv")
Slight = read.csv("BAP(over5.under7.5).Prob.Estimates.csv")
Low = read.csv("BAPunder5.Prob.Estimates.csv")

#change the column names
names(High)[names(High)=="probability"]<-"pHigh"
names(Slight)[names(Slight)=="probability"]<-"pSlight"
names(Low)[names(Low)=="probability"]<-"pLow"

#merge together
Combined <- merge(High,Slight,by=c("sample_id","bap"), all=TRUE)
Combined <- merge(Combined,Low,by=c("sample_id","bap"), all=TRUE)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(Combined,file="Combined.probs.csv",sep=",",row.names=FALSE)


#tidy up. This removes everything stored in short term memory
rm(list=ls())

#######################################################################################

#read the data
pairs = read.csv("WAVE.FamilyPair.Frequencies.csv")
