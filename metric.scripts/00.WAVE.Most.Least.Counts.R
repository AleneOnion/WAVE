# 6/22/2017 Alene Onion
# This script takes a file of all SBU kick samples since 1990 and identifies the number of WAVE Leadt Wanteds and WAVE Most Wanteds in each sample
# It then plots all the BAP scores as well as those with more than 5,6,7,8,9,and10 most wanteds as well as those with more than 3,4,and 5 Least Wanteds 

#set libraries
#library for the count function
library(plyr)
#library for the Confidence Interval Function
library(rcompanion)

# set the working directory 
setwd("C:/Rscripts/WAVE")

SBU = read.csv("all_samplesL.csv")

#Pull bug names from the table
Least = read.csv("Least.csv")
Most = read.csv("Most.csv")
#convert the names to characters
Least$Family <- as.character(Least$Family)
Least$GENSPECIES <- as.character(Least$GENSPECIES)
Most$Family <- as.character(Most$Family)
Most$GENSPECIES <- as.character(Most$GENSPECIES)

#Create a file with the Least Wanted Organisms Named
nbugs1 = length(Least$GENSPECIES)
m=0
n=0

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



##################################################################################################
##################################################################################################
#Produce one file with all the sites, the least count, and the most count and plot these data

#Create file with only sample ID, date, and BAP -> meaning only one record per sample
SBUprint <- count(SBU,c("sample_id","coll_date","bap"))
#remove the freq from this file
keep1 <- c("sample_id","coll_date","bap")
SBUprint <- SBUprint[keep1]

#Rename the freq column in the LeastCount and MostCount tables to LeastNumber and MostNumber
LeastCount <- rename(LeastCount,c("freq"="LeastNumber"))
MostCount <- rename(MostCount,c("freq"="MostNumber"))

#Merge the datasets without deleting non-overlapping data (using all=TRUE)
Combined <- merge(SBUprint,LeastCount,by=c("sample_id","coll_date","bap"), all=TRUE)
Combined2 <- merge(Combined,MostCount,by=c("sample_id","coll_date","bap"), all=TRUE)

#round the BAP scores to the nearest 0.5 while keeping the original file to print later
Rounded <- Combined2
Rounded[,"bap"]= round(Rounded[,"bap"]/.5)*.5

#create data sets where MostNumber is >5,>6,>7,>8,>9,>10 OR the LeastNumber is >3,>4,>5
Most5 <- subset(Rounded,MostNumber>=5)
Most6 <- subset(Rounded,MostNumber>=6)
Most7 <- subset(Rounded,MostNumber>=7)
Most8 <- subset(Rounded,MostNumber>=8)
Most9 <- subset(Rounded,MostNumber>=9)
Most10 <- subset(Rounded,MostNumber>=10)
Least3 <- subset(Rounded,LeastNumber>=3)
Least4 <- subset(Rounded,LeastNumber>=4)
Least5 <- subset(Rounded,LeastNumber>=5)

#Create bap score counts for each subset
AllPlot <- count(Rounded,c("bap"))
Most5Plot <- count(Most5,c("bap"))
Most6Plot <- count(Most6,c("bap"))
Most7Plot <- count(Most7,c("bap"))
Most8Plot <- count(Most8,c("bap"))
Most9Plot <- count(Most9,c("bap"))
Most10Plot <- count(Most10,c("bap"))
Least3Plot <- count(Least3,c("bap"))
Least4Plot <- count(Least4,c("bap"))
Least5Plot <- count(Least5,c("bap"))

#Calculate mean and sd for each subset (not rounded)
#first create subsets with BAP scores that are not rounded to the nearest .5
Most5CI <- subset(Combined2,MostNumber>=5)
Most6CI <- subset(Combined2,MostNumber>=6)
Most7CI <- subset(Combined2,MostNumber>=7)
Most8CI <- subset(Combined2,MostNumber>=8)
Most9CI <- subset(Combined2,MostNumber>=9)
Most10CI <- subset(Combined2,MostNumber>=10)

#create one plot to rule them all with vertical lines for the impact categories
plot(AllPlot$bap,AllPlot$freq,type="l",lwd=4,xlab="BAP Score",ylab="Number of Sites")
lines(Most6Plot$bap,Most6Plot$freq,lwd=4,col="dodgerblue1",type="l")
lines(Most7Plot$bap,Most7Plot$freq,lwd=4,col="dodgerblue2",type="l")
lines(Most8Plot$bap,Most8Plot$freq,lwd=4,col="dodgerblue3",type="l")
lines(Most9Plot$bap,Most9Plot$freq,lwd=4,col="dodgerblue4",type="l")
lines(Most10Plot$bap,Most10Plot$freq,lwd=4,col="deepskyblue4",type="l")
lines(Least3Plot$bap,Least3Plot$freq,lwd=4,col="firebrick3",type="l")
lines(Least4Plot$bap,Least4Plot$freq,lwd=4,col="firebrick2",type="l")
lines(Least5Plot$bap,Least5Plot$freq,lwd=4,col="firebrick1",type="l")
abline(v=7.5)
abline(v=5)
abline(v=2.5)
legend("topleft",lty=1,legend=c("All Sites",">5 to >10 Most",">3 to >5 Least"),lwd=c(2,2,2),col=c("black","dodgerblue","firebrick1"))


#############################################################################################
#Create Count table
#############################################################################################

#creat impact column
Combined2$impact <- "2.slight"
Combined2$impact <- ifelse(Combined2$bap<5,"3.low",Combined2$impact)
Combined2$impact <- ifelse(Combined2$bap>=7.5,"1.high",Combined2$impact)

#count records in each category
library(plyr)
testc <- count(Combined2,c("impact","LeastNumber"))

#create table
library(reshape)
testc <- cast(testc,impact~LeastNumber)
#replace NA values with 0
testc[is.na(testc)] <- 0
#create a total column
testc$total <- rowSums(testc)

#count records in each category
library(plyr)
testd <- count(Combined2,c("impact","MostNumber"))

#create table
library(reshape)
testd <- cast(testd,impact~MostNumber)
#replace NA values with 0
testd[is.na(testd)] <- 0
#create a total column
testd$total <- rowSums(testd)
testd

# set the working directory
setwd("C:/Rscripts/WAVE/output")

#Write the summary.table
write.csv(testc,file="family.count.summary.least.csv",row.names=FALSE)
write.csv(testd,file="family.count.summary.most.csv",row.names=FALSE)

#Write the combined data file
write.table(Combined2,file="Output.csv",sep=",")

#tidy up. This removes everything stored in short term memory
rm(list=ls())