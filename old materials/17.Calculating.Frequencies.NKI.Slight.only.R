#10/2017 Alene Onion
# This script takes a file of all SBU kick samples since 1990 
# and renames the genspecies field to the broader WAVE Family
# It then calculates the #samples containing each family, 
# #samples with bap<5 containing each family, 
# #samples with bap>5 containing each family, 
# and #samples with bap>7.5 containing each family
# These frequencies will be used in the next script to calculate probabilities


#set libraries
#library for the count function
library(plyr)

# set the working directory 
setwd("C:/Rscripts/WAVE")

#read the SBU data - all kick samples since 1990
SBU = read.csv("all_samples.csv")

#Create SBU file with only sample ID, date, and BAP -> meaning only one record per sample
SBUshort <- count(SBU,c("sample_id","coll_date","bap"))
#remove the freq from this file
keep1 <- c("sample_id","coll_date","bap")
SBUshort <- SBUshort[keep1]


#Pull bug names from the table
WAVE = read.csv("Most.double.csv")
#convert the names to characters
WAVE$Family <- as.character(WAVE$Family)
WAVE$GENSPECIES <- as.character(WAVE$GENSPECIES)

#Create a file with the WAVE Organisms Named
k=NULL
n=0
SBUrenamed = NULL 
bugs = NULL
nbugs = length(WAVE$GENSPECIES)

for (k in 1:nbugs){
  #Create selection criteria specific to the given parameter
  bugs = SBU$genspecies == WAVE$GENSPECIES[k]
  #Pare data to specific parameter
  SBUm <- SBU[bugs,]
  #Capture the name WAVE bug
  name = WAVE$Family[k]
  #add the name to the WAVE column
  SBUm$WAVE <- rep(name,nrow(SBUm))
  #merge this with previous pulls
  if (n>0){
    SBUrenamed <- merge(SBUrenamed,SBUm, all=TRUE)
  }
  else {
    SBUrenamed <- SBUm
    n = 1
  }
  bugs = NULL
}

#Convert the family names to characters
SBUrenamed$WAVE <- as.character(SBUrenamed$WAVE)


#Produces a SBUrenamed file with no duplicate WAVE families per sample. This is necessary because there may be more than one species of a given family in the sample
SBUrenamed <- count(SBUrenamed,c("sample_id","coll_date","bap","WAVE"))
keep2 <- c("sample_id","coll_date","bap","WAVE")
SBUrenamed <- SBUrenamed[keep2]

#Truncate WAVE and SBU file to BAP>=7.5
WAVEh = SBUrenamed$bap >= 7.5
SBUrenamedNKI <- SBUrenamed[WAVEh,]
highSBU = SBUshort$bap >= 7.5
SBUH <- SBUshort[highSBU,]

#Truncate WAVE and SBU file to BAP<5
lowWAVE = SBUrenamed$bap < 7.5
SBUrenamedLow <- SBUrenamed[lowWAVE,]
lowSBU = SBUshort$bap < 7.5
SBUlow <- SBUshort[lowSBU,]

#Count # samples in each subset
FreqAll <- count(SBUrenamed,"WAVE")
FreqNKI <- count(SBUrenamedNKI,"WAVE")
FreqLow <- count(SBUrenamedLow,"WAVE")

#Changing Column Names
names(FreqAll)[names(FreqAll)=="freq"]<- "Total"
names(FreqLow)[names(FreqLow)=="freq"]<-"Low"
names(FreqNKI)[names(FreqNKI)=="freq"]<-"NKI"

#Combine them together
Combined <- merge(FreqAll,FreqLow,by=c("WAVE"), all=TRUE)
Combined <- merge(Combined,FreqNKI,by=c("WAVE"),all=TRUE)

#Get total possible records for the Total row
allSBU <- nrow(SBUshort)
LSBU <- nrow(SBUlow)
HSBU <- nrow(SBUH)

#Add a total row
TOTAL = data.frame(WAVE="Total",Total=allSBU,Low=LSBU,NKI=HSBU)
Combined2 <- merge(TOTAL,Combined,all=TRUE)

#Convert NAs to 0
Combined2[is.na(Combined2)] <- 0


# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(Combined2,file="WAVE.Family.Frequencies.csv",sep=",",row.names=FALSE)
write.table(SBUrenamed,file="SBU.Data.renamed.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())