#10/2017 Alene Onion
# This script takes a file of all SBU kick samples since 1990 
# and renames the genspecies field to the broader WAVE Family
# It then calculates the #samples containing each family, 
# #samples with bap<5 containing each family, 
# #samples with bap>5 containing each family, 
# and #samples with bap>7.5 containing each family
# These frequencies will be used in the next script to calculate probabilities


###############################################
#YOU MUST RUN script 20.Creating.user.subsets FIRST
###############################################


#set libraries
#library for the count function
library(plyr)

# set the working directory 
setwd("C:/Rscripts/WAVE")

#Pull bug names from the table
WAVE = read.csv("All.Families.csv")
#convert the names to characters
WAVE$Family <- as.character(WAVE$Family)
WAVE$GENSPECIES <- as.character(WAVE$GENSPECIES)
names(WAVE)[names(WAVE)=="GENSPECIES"]<- "genspecies"

#Create a file with the WAVE Organisms Named
SBU <- merge(SBU,WAVE,by=c("genspecies"))
#remove WAVE column
keep <- c("sample_id","coll_date","bap","genspecies","user","Family")
SBU <- SBU[keep]
rm(keep)
#rename Family column WAVE column
names(SBU)[names(SBU)=="Family"]<-"WAVE"
#Convert the family names to characters
SBU$WAVE <- as.character(SBU$WAVE)
#Produces a SBU file with no duplicate WAVE families per sample. This is necessary because there may be more than one species of a given family in the sample
SBU <- unique(SBU[c("sample_id","coll_date","bap","WAVE","user")])
rm(WAVE)

#merge WAVE and user column
SBU$Family <- do.call(paste,c(SBU[c("WAVE","user")],sep="_"))
#remove user and WAVE columns
keep <- c("sample_id","coll_date","bap","Family")
SBU <- SBU[keep]
rm(keep)
#rename Family column to WAVE
names(SBU)[names(SBU)=="Family"]<- "WAVE"

#Create impact column
SBU$impact <- "0"
SBU$impact <- ifelse(SBU$bap<5,"3.Low",SBU$impact)
SBU$impact <- ifelse(SBU$bap>=7.5,"1.High",SBU$impact)
SBU$impact <- ifelse(SBU$impact==0,"2.Slight",SBU$impact)

#Create WAVE.Family-User Frequencies
#first count the frequency of each combination
library(plyr)
SBUFreq <- count(SBU[c("WAVE","impact")])
#now restructure the table
library(reshape)
SBUFreq <- cast(SBUFreq,WAVE~impact)

#Get total values for each impact
junk <- unique(SBU[c("sample_id","coll_date","impact")])
total <- count(junk,c("impact"))
rm(junk)
high <- total$freq[1]
slight <- total$freq[2]
low <- total$freq[3]
rm(total)

#Create probability tables
SBUProb <- SBUFreq
SBUProb$`1.High` <- SBUFreq$`1.High`/high
SBUProb$`2.Slight` <- SBUFreq$`2.Slight`/slight
SBUProb$`3.Low` <- SBUFreq$`3.Low`/low
#convert NA values to 0
SBUProb[is.na(SBUProb)]<- 0
rm(high)
rm(low)
rm(slight)

#merge the SBU and SBUProb tables
SBU <- merge(SBU,SBUProb,by=c("WAVE"))
rm(SBUProb)
rm(SBUFreq)

#Confirm that there's only one sample per site
junk1 <- unique(SBU[c("sample_id")])
junk2 <- unique(SBU[c("sample_id","coll_date")])
print "these values should be the same. If not, then there are more than one sample per site"
nrow(junk1)
nrow(junk2)
rm(junk1)
rm(junk2)

#calculate the prob of high, slight, low for each sample 
#find out how many samples I have to run
samples = unique(SBU$sample_id)
nsamples <- length(samples)

#for the first sample
sample = SBU$sample_id==samples[1]
SBUsample <- SBU[sample,]
SBUsample$`1.High`<- prod(SBUsample$`1.High`)
SBUsample$`2.Slight`<- prod(SBUsample$`2.Slight`)
SBUsample$`3.Low`<- prod(SBUsample$`3.Low`)
SBUsample <- unique(SBUsample[c("sample_id","coll_date","bap","impact","1.High","2.Slight","3.Low")])
highT <- SBUsample$`1.High`/(SBUsample$`1.High`+SBUsample$`2.Slight`+SBUsample$`3.Low`)
slightT <- SBUsample$`2.Slight`/(SBUsample$`1.High`+SBUsample$`2.Slight`+SBUsample$`3.Low`)
lowT <- SBUsample$`3.Low`/(SBUsample$`1.High`+SBUsample$`2.Slight`+SBUsample$`3.Low`)
SBUsample$`1.High`<- highT
SBUsample$`2.Slight`<- slightT
SBUsample$`3.Low`<- lowT
ProbSamp <- SBUsample
rm(SBUsample)
rm(sample)
rm(highT)
rm(slightT)
rm(lowT)

#and now for all subsequent samples
for(i in 2:nsamples){
  sample = SBU$sample_id==samples[i]
  SBUsample <- SBU[sample,]
  SBUsample$`1.High`<- prod(SBUsample$`1.High`)
  SBUsample$`2.Slight`<- prod(SBUsample$`2.Slight`)
  SBUsample$`3.Low`<- prod(SBUsample$`3.Low`)
  SBUsample <- unique(SBUsample[c("sample_id","coll_date","bap","impact","1.High","2.Slight","3.Low")])
  highT <- SBUsample$`1.High`/(SBUsample$`1.High`+SBUsample$`2.Slight`+SBUsample$`3.Low`)
  slightT <- SBUsample$`2.Slight`/(SBUsample$`1.High`+SBUsample$`2.Slight`+SBUsample$`3.Low`)
  lowT <- SBUsample$`3.Low`/(SBUsample$`1.High`+SBUsample$`2.Slight`+SBUsample$`3.Low`)
  SBUsample$`1.High`<- highT
  SBUsample$`2.Slight`<- slightT
  SBUsample$`3.Low`<- lowT
  ProbSamp <- merge(ProbSamp,SBUsample,all=TRUE)
  rm(SBUsample)
  rm(sample)
  rm(highT)
  rm(slightT)
  rm(lowT)
}
rm(i)
rm(nsamples)
rm(samples)

###########################################################################################################################################
#Summarize data
###########################################################################################################################################

#plot these values
plot(ProbSamp$bap,ProbSamp$`1.High`,type="p",lwd=4,col="deeppink1",main="All Families + User Data",xlab="BAP Score",ylab="Probability")
#lines(ProbSamp$bap,ProbSamp$`2.Slight`,lwd=4,col="darkmagenta",type="p")
lines(ProbSamp$bap,ProbSamp$`3.Low`,lwd=4,col="blue3",type="p")
abline(v=7.5)
abline(v=5)
abline(v=2.5)

summary <- ProbSamp
summary$prob <- 0
summary$prob <- ifelse(summary$`1.High`>0.95,"1.pH",summary$prob)
summary$prob<-ifelse(summary$`2.Slight`>0.95,"2.pS",summary$prob)
summary$prob<-ifelse(summary$`3.Low`>0.95,"3.pL",summary$prob)

#count the records
library(plyr)
summary <- count(summary,c("impact","prob"))

#reshape the table
library(reshape)
summary <- cast(summary,impact~prob)
summary[is.na(summary)]<-0
#create total column
total <- count(ProbSamp$impact)
names(total)[names(total)=="freq"]<-"total"
names(total)[names(total)=="x"]<-"impact"
#merge total with summary
summary <- merge(total,summary,by=c("impact"))
rm(total)
#remove the 0 column
keep <- c("impact","total","1.pH","2.pS","3.pL")
summary <- summary[keep]
rm(keep)
#calculate percentage
summary$perc <- summary$`1.pH`/summary$total
#print to view
summary



# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(ProbSamp,file="Prob.Estimates.csv",sep=",",row.names=FALSE)
write.table(summary,file="Prob.Estimates.summary.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())