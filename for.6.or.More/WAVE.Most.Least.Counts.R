# 6/22/2017 Alene Onion
# This script takes a file of all SBU kick samples since 1990 and identifies the number of WAVE Least and Most Wanteds in each sample
# and produces files to run the probability analyses with

#set libraries
#library for the count function
library(plyr)
#library for the Confidence Interval Function
library(rcompanion)

# set the working directory 
setwd("C:/Rscripts/WAVE")

SBU = read.csv("all_samples.csv")

#Pull bug names from the table
Least = read.csv("Least.csv")
Most = read.csv("Most.csv")
#convert the names to characters
Least$Family <- as.character(Least$Family)
Least$GENSPECIES <- as.character(Least$GENSPECIES)
Most$Family <- as.character(Most$Family)
Most$GENSPECIES <- as.character(Most$GENSPECIES)

#Create a file with the Least Wanted Organisms Named
j=NULL
m=0
SBUSubset1 = NULL 
bugs1 = NULL
nbugs1 = length(Least$GENSPECIES)

for (j in 1:nbugs1){
  #Create selection criteria specific to the given parameter
  bugs1 = SBU$genspecies == Least$GENSPECIES[j]
  #Pare data to specific parameter
  SBUm1 <- SBU[bugs1,]
  #Capture the name WAVE bug
  name1 = Least$Family[j]
  #add the name to the WAVE column
  SBUm1$WAVE <- rep(name1,nrow(SBUm1))
  #merge this with previous pulls
  if (m>0){
    SBUSubset1 <- merge(SBUSubset1,SBUm1, all=TRUE)
  }
  else {
    SBUSubset1 <- SBUm1
    m = 1
  }
}

#Create a file with the Most Wanted Organisms Named
k=NULL
n=0
SBUSubset2 = NULL 
bugs2 = NULL
nbugs2 = length(Most$GENSPECIES)

for (k in 1:nbugs2){
  #Create selection criteria specific to the given parameter
  bugs2 = SBU$genspecies == Most$GENSPECIES[k]
  #Pare data to specific parameter
  SBUm2 <- SBU[bugs2,]
  #Capture the name WAVE bug
  name2 = Most$Family[k]
  #add the name to the WAVE column
  SBUm2$WAVE <- rep(name2,nrow(SBUm2))
  #merge this with previous pulls
  if (n>0){
    SBUSubset2 <- merge(SBUSubset2,SBUm2, all=TRUE)
  }
  else {
    SBUSubset2 <- SBUm2
    n = 1
  }
}

#Produces a file with no duplicate WAVE families per sample. This is necessary because there may be more than one species of a given family in the sample
LeastSubset <- unique(SBUSubset1[c("sample_id","coll_date","bap","WAVE")])
MostSubset <- unique(SBUSubset2[c("sample_id","coll_date","bap","WAVE")])
#Produces a file with a count of WAVE least wanted families per sample
LeastCount <- count(LeastSubset,c("sample_id","coll_date","bap"))
MostCount <- count(MostSubset,c("sample_id","coll_date","bap"))

#Create a file with only samples with 6 or more most wanteds
Most6 <- subset(MostCount,freq>=5)
MostSBU <- merge(SBU,Most6,by=c("sample_id","coll_date","bap"),all=FALSE)

#Create a file with only samples with 4 or more least wanteds
Least4 <- subset(LeastCount,freq<=4)
LeastSBU <- merge(SBU,Least4,by=c("sample_id","coll_date","bap"),all=FALSE)

# set the working directory
setwd("C:/Rscripts/WAVE/for.6.or.More")

#Write the data files
write.table(MostSBU,file="MostSBU.csv",sep=",")
write.table(LeastSBU,file="LeastSBU.csv",sep=",")

#tidy up. This removes everything stored in short term memory
rm(list=ls())