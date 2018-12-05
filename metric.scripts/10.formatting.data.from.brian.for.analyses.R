# Program:  Preparing file for WAVE analysis
# Programmer: Alene Onion 
# Date: 11/13/2017

#The purpose of this script is to prepare a file that meets these criteria
#- Only 1 sample per site
#- Only the most recent assessment
#- Only kick sampling method
#- only collected between July and September

# set the working directory
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data")

SBU1 <- read.csv("95-99_SpeciesBAP.csv")
SBU2 <- read.csv("00-08_SpeciesBAP.csv")
SBU3 <- read.csv("09-15_SpeciesBAP.csv")

SBU <- merge(SBU1,SBU2,all=TRUE)
SBU <- merge(SBU,SBU3,all=TRUE)

rm(SBU1)
rm(SBU2)
rm(SBU3)



############################################################################################
#Remove samples collected outside the sampling period
############################################################################################

#Convert dates to date values
SBU$coll_date <- as.Date(SBU$coll_date,"%d-%b-%y")
#Create a new column for sampling month
SBU$month <- format(SBU$coll_date,"%m")

#pull records where month is between 7-9
mon = SBU$month == "07" | SBU$month == "08" | SBU$month == "09"
SBU <- SBU[mon,]
rm(mon)

#pull records where collect = kick sampling (1)
kick = SBU$collect == "1"
SBU <- SBU[kick,]
rm(kick)

#merge the latlon
SBU$latlon <- do.call(paste,c(SBU[c("LATITUDE","LONGITUDE")],sep="_"))

#Remove uneccesary columns
keep <- c("latlon","coll_date","replicate","genspecies","bap")
SBU <- SBU[keep]
rm(keep)

#Remove NA values
SBU <- SBU[!is.na(SBU$coll_date),]

############################################################################################
#keep the max replicate, and most recent sample
############################################################################################

#pull max date for each record
library(plyr)
library(reshape2)
SBU1 <- melt(SBU,id=c("latlon","coll_date"),na.rm=TRUE)
SBU1 <- ddply(SBU1,.(latlon),summarise,coll_date=max(coll_date))
#remove NA values
SBU1 <- SBU1[!is.na(SBU1$coll_date),]

#merge the files so only the most recent date is retained
SBU <- merge(SBU1,SBU,all=FALSE)
rm(SBU1)

#pull max replicate for each sample
library(plyr)
library(reshape2)
SBU2 <- melt(SBU,id=c("latlon","replicate"),na.rm=TRUE)
SBU2 <- ddply(SBU2,.(latlon),summarise,replicate=max(replicate))
#remove NA values
SBU2 <- SBU2[!is.na(SBU2$replicate),]

#merge the files so only
SBU <- merge(SBU2,SBU,all=FALSE)
rm(SBU2)

############################################################################################
#add superfluous columns 
############################################################################################

#rename latlon to sample_id
names(SBU)[names(SBU)=="latlon"]<-"sample_id"

#remove replicate column
keep <- c("sample_id","coll_date","genspecies","bap")
SBU <- SBU[keep]
rm(keep)

#add WAVE column
SBU$WAVE <- NA

# write the table
setwd("C:/Rscripts/WAVE")
write.table(SBU,file="all_samplesL.csv",sep=",",row.names=FALSE)


#tidy up. This removes everything stored in short term memory
rm(list=ls())