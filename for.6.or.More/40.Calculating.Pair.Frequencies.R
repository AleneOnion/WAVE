#10/2017 Alene Onion
# This script takes a file of all SBU kick samples since 1990 
# and renames the genspecies field to the broader WAVE Family
# It then creates a list of all possible family pairs
# and calculates the frequencies for each category
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
WAVE = read.csv("All.WAVE.Families.csv")
#convert the names to characters
WAVE$Family <- as.character(WAVE$Family)
WAVE$GENSPECIES <- as.character(WAVE$GENSPECIES)
#sort the file by Family then GenSpecies
WAVE <- WAVE[order(WAVE$Family,WAVE$GENSPECIES),]

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

#Create a file frequencies for each wave family combo
n=NULL
m=NULL
p=NULL
b2 = NULL
SBU2 = NULL
b3 = NULL
SBU3 = NULL
SBU4 = NULL
h1 = NULL
SBUHigh = NULL
S1 = NULL
SBUSlight = NULL
L1 = NULL
SBULow = NULL
FreqAll = NULL
FreqLow = NULL
FreqSli = NULL
FreqHigh = NULL
temp = NULL
b1 = NULL
SBU1 = NULL
bugs <- unique(WAVE$Family)
nbugs = length(bugs)
nbugstop = nbugs-1

#create table to put these into
table = data.frame(Family1=NA,Family2=NA,Total=NA,Low=NA,Slight=NA,High=NA)

for (m in 1:nbugstop){
  #Create selection criteria specific to the given parameter
  b1 = SBUrenamed$WAVE == bugs[m]
  #Pare data to specific parameter
  SBU1 <- SBUrenamed[b1,]
  n=m+1
  
  for (p in n:nbugs){
    #Create selection criteria specific to the given parameter
    b2 = SBUrenamed$WAVE == bugs[p]
    #Pare data to specific parameter
    SBU2 <- SBUrenamed[b2,]
    #merge these together and only keep overlapping samples
    SBU4 <- merge(SBU1,SBU2,by=c("sample_id","coll_date","bap"), all=FALSE)
    #truncate this file into High,Slith,Low, and NKI
    h1 = SBU4$bap >= 7.5
    SBUHigh <- SBU4[h1,]
    S1 = SBU4$bap >= 5
    SBUSlight <- SBU4[S1,]
    S1 = SBUSlight$bap < 7.5
    SBUSlight <- SBUSlight[S1,]
    L1 = SBU4$bap <5
    SBULow <- SBU4[L1,]
    #count the frequencies
    FreqAll <- nrow(SBU4)
    FreqLow <- nrow(SBULow)
    FreqSli <- nrow(SBUSlight)
    FreqHigh <- nrow(SBUHigh)
    temp = data.frame(Family1=(bugs[m]),Family2=(bugs[p]),Total=FreqAll,Low=FreqLow,Slight=FreqSli,High=FreqHigh)
    table <- merge(table,temp,all=TRUE)
    b2 = NULL
    SBU2 = NULL
    SBU4 = NULL
    h1 = NULL
    SBUHigh = NULL
    S1 = NULL
    SBUSlight = NULL
    L1 = NULL
    SBULow = NULL
    FreqAll = NULL
    FreqLow = NULL
    FreqSli = NULL
    FreqHigh = NULL
    temp = NULL
  }
  b1 = NULL
  SBU1 = NULL
  }

#Truncate SBU file to BAP>=7.5
highSBU = SBUshort$bap >= 7.5
SBUH <- SBUshort[highSBU,]

#Truncate SBU file to (>7.5 BAP >=5)
sSBU = SBUshort$bap >= 5
SBUSl <- SBUshort[sSBU,]
sSBU = SBUSl$bap < 7.5
SBUSl <- SBUSl[sSBU,]

#Truncate WAVE and SBU file to BAP<5
lowSBU = SBUshort$bap < 5
SBUlow <- SBUshort[lowSBU,]

#Truncate WAVE and SBU file to BAP>=5
medSBU = SBUshort$bap >= 5
SBUNKI <- SBUshort[medSBU,]

#Get total possible records for the Total row
allSBU <- nrow(SBUshort)
LSBU <- nrow(SBUlow)
SSBU <- nrow(SBUSl)
HSBU <- nrow(SBUH)

#Add a total row
TOTAL = data.frame(Family1="total",Family2="total",Total=allSBU,Low=LSBU,Slight=SSBU,High=HSBU)
Combined <- merge(TOTAL,table,all=TRUE)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(Combined,file="FamilyPair.Frequencies.csv",sep=",",row.names=FALSE)
write.table(SBUrenamed,file="SBU.Data.renamed.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())