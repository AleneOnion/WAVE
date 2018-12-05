# 6/22/2017 Alene Onion
# This script takes a file of all SBU kick samples since 1990 and identifies the number of WAVE Leadt Wanteds and WAVE Most Wanteds in each sample
# It then plots all the BAP scores as well as those for each family level subset 

#set libraries
#library for the count function
library(plyr)
#library for the Confidence Interval Function
library(rcompanion)

# set the working directory 
setwd("C:/Rscripts/WAVE")

#read the SBU data - all kick samples since 1990
SBU = read.csv("all_samplesL.csv")

#Create SBU file with only sample ID, date, and BAP -> meaning only one record per sample
SBUprint <- count(SBU,c("sample_id","coll_date","bap"))
#remove the freq from this file
keep1 <- c("sample_id","coll_date","bap")
SBUprint <- SBUprint[keep1]


#Pull bug names from the table
Most = read.csv("Most.over30P.csv")
#convert the names to characters
Most$Family <- as.character(Most$Family)
Most$GENSPECIES <- as.character(Most$GENSPECIES)

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

#Produces a WAVE file with no duplicate WAVE families per sample. This is necessary because there may be more than one species of a given family in the sample
MostSubset <- count(SBUSubset2,c("sample_id","coll_date","bap","WAVE"))
keep2 <- c("sample_id","coll_date","bap","WAVE")
MostSubset <- MostSubset[keep2]

#Truncate WAVE and SBU file to BAP>=7.5
highMost = MostSubset$bap >= 7.5
MostH <- MostSubset[highMost,]
highSBU = SBUprint$bap >= 7.5
SBUH <- SBUprint[highSBU,]

#Truncate WAVE and SBU file to BAP>=5
medMost = MostSubset$bap >= 5
MostNKI <- MostSubset[medMost,]
medSBU = SBUprint$bap >= 5
SBUNKI <- SBUprint[medSBU,]

#Count # samples in each subset
FreqAll <- count(MostSubset,"WAVE")
FreqHigh <- count(MostH,"WAVE")
FreqNKI <- count(MostNKI,"WAVE")

#Get total possible records in the SBU files for column names
allSBU <- nrow(SBUprint)
HSBU <- nrow(SBUH)
MSBU <- nrow(SBUNKI)

#Changing Column Names
names(FreqAll)[names(FreqAll)=="freq"]<- paste("FreqAll out of ", allSBU,sep="")
names(FreqHigh)[names(FreqHigh)=="freq"]<-paste("FreqHigh out of ",HSBU,sep="")
names(FreqNKI)[names(FreqNKI)=="freq"]<-paste("FreqNKI out of ", MSBU,sep="")

#Combine them together
first <- merge(FreqAll,FreqHigh,by=c("WAVE"), all=TRUE)
Combined <- merge(first,FreqNKI,by=c("WAVE"),all=TRUE)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(Combined,file="WAVE.Family.Frequencies.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())