#March 2018 Alene Onion
# Identifying possibly impaired sites using BAES prob calcualtions

#tidy up. This removes everything stored in short term memory
rm(list=ls())

setwd("L:/DOW/StreamDatabase/SBU")
species<-read.csv("Species.csv")
sites<-read.csv("Sites.csv")
metrics<-read.csv("metrics.csv")

############################################################################################
#Restrict to multiplate/kick samples, collected in sampling period, and only one result per site
############################################################################################

#Convert dates to date values
metrics$DATE <- as.Date(metrics$DATE,"%m/%d/%Y")
#Create a new column for sampling month
metrics$month <- format(metrics$DATE,"%m")

#pull records where month is between 7-9
mon = metrics$month == "07" | metrics$month == "08" | metrics$month == "09"
metrics <- metrics[mon,]
rm(mon)

#restricting to only multiplate and kick samples
sample = metrics$COLLECT=="1"|metrics$COLLECT=="2"|metrics$COLLECT=="5"|metrics$COLLECT=="3"
metrics<-metrics[sample,]
rm(sample)

#restricting to only one replicate
metrics<-metrics[metrics$REPLICATE=="1",]

#add stream name and lat/lon to the metrics table
sites$SITE_ID <- paste(sites$BASIN,"-",sites$LOCATION,"-",sites$RIVMILE,sep="")
sites<-unique(sites[c("SITE_ID","NAME","LATITUDE","LONGITUDE")])

metrics$SITE_ID<-paste(metrics$BASIN,"-",metrics$LOCATION,"-",metrics$RIVMILE,sep="")
data<-merge(metrics,sites,all=FALSE)
rm(list=c("metrics","sites"))

#Remove NA values
data <- data[!is.na(data$DATE),]
data<-unique(data[c("NAME","SITE_ID","LATITUDE","LONGITUDE","DATE","REPLICATE","COLLECT","FINAL_SCORE")])

#rename final score to BAP
names(data)[names(data)=="FINAL_SCORE"]<-"bap"

#pull only the most recent date
data1<-aggregate(data$DATE,by=list(data$SITE_ID),max)
names(data1)[names(data1)=="Group.1"]<-"SITE_ID"
names(data1)[names(data1)=="x"]<-"DATE"
data<-merge(data1,data,by=c("SITE_ID","DATE"),all=FALSE)
rm(data1)

#modify species table for merge
species$SITE_ID<-paste(species$BASIN,"-",species$LOCATION,"-",species$RIVMILE,sep="")
species$DATE <- as.Date(species$COLL_DATE,"%m/%d/%Y")
species<-unique(species[c("SITE_ID","DATE","COLLECT","REPLICATE","MACRO_GENSPECIES")])

#merge the tables
data<-merge(data,species,by=c("SITE_ID","DATE","COLLECT","REPLICATE"),all=FALSE)
rm(species)
data<-unique(data[c("SITE_ID","DATE","MACRO_GENSPECIES","bap")])
data$WAVE<-NA

#rename column headers to match expected
names(data)[names(data)=="SITE_ID"]<-"sample_id"
names(data)[names(data)=="DATE"]<-"coll_date"
names(data)[names(data)=="MACRO_GENSPECIES"]<-"genspecies"

SBU<-data
rm(data)

##############################################################################################################
#Create probability tables for each WAVE family
##############################################################################################################

#set libraries
#library for the count function
library(plyr)

# set the working directory 
setwd("C:/Rscripts/WAVE")

#Pull bug names from the table
WAVE = read.csv("All.WAVE.Families.csv")
#convert the names to characters
WAVE$Family <- as.character(WAVE$Family)
WAVE$GENSPECIES <- as.character(WAVE$GENSPECIES)
names(WAVE)[names(WAVE)=="GENSPECIES"]<- "genspecies"
WAVE$genspecies<-toupper(WAVE$genspecies)


#Create a file with the WAVE Organisms Named
#this will remove organisms that aren't on the WAVE Families list
SBU <- merge(SBU,WAVE,by=c("genspecies"))
#remove WAVE column
keep <- c("sample_id","coll_date","bap","genspecies","Family")
SBU <- SBU[keep]
rm(keep)
#rename Family column WAVE column
names(SBU)[names(SBU)=="Family"]<-"WAVE"
#Convert the family names to characters
SBU$WAVE <- as.character(SBU$WAVE)
#Produces a SBU file with no duplicate WAVE families per sample. This is necessary because there may be more than one species of a given family in the sample
SBU <- unique(SBU[c("sample_id","coll_date","bap","WAVE")])
rm(WAVE)

#Create impact column
SBU$impact <- "0"
SBU$impact <- ifelse(SBU$bap<5,"3.Low",SBU$impact)
SBU$impact <- ifelse(SBU$bap>=7.5,"1.High",SBU$impact)
SBU$impact <- ifelse(SBU$impact==0,"2.Slight",SBU$impact)

#Create WAVE.Family Frequencies
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
rm(SBUFreq)
rm(SBU)

##############################################################################################################
#Pull in sample data
##############################################################################################################


# set the working directory
setwd("C:/Rscripts/WAVE/2017")

#Read data tables
Sample<-read.csv("Sample.final.csv")

#remove (Order) out of the macro column
Sample$Macroinvertebrate.Family<-gsub("\\(.+\\)","",Sample$Macroinvertebrate.Family)
Sample$Macroinvertebrate.Family<-gsub("\\s","",Sample$Macroinvertebrate.Family)

#rename matching columns
names(Sample)[names(Sample)=="Macroinvertebrate.Family"]<-"WAVE"

#create sample ID
Sample$sample_id <- paste(Sample$Latitude,",",Sample$Longitude,sep="")


#merge Sample and Prob tables
data<-merge(SBUProb,Sample,by=c("WAVE"),all=FALSE)
rm(list=c("Sample","SBUProb"))

##############################################################################################################
#Calculate prob for each sample
##############################################################################################################


#calculate the prob of high, slight, low for each sample 
#find out how many samples I have to run
samples = unique(data$sample_id)
nsamples <- length(samples)

#for the first sample
sample = data$sample_id==samples[1]
WAVEsample <- data[sample,]
WAVEsample$`1.High`<- prod(WAVEsample$`1.High`)
WAVEsample$`2.Slight`<- prod(WAVEsample$`2.Slight`)
WAVEsample$`3.Low`<- prod(WAVEsample$`3.Low`)
WAVEsample <- unique(WAVEsample[c("sample_id","Collection.Date","1.High","2.Slight","3.Low")])
highT <- WAVEsample$`1.High`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
slightT <- WAVEsample$`2.Slight`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
lowT <- WAVEsample$`3.Low`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
WAVEsample$`1.High`<- highT
WAVEsample$`2.Slight`<- slightT
WAVEsample$`3.Low`<- lowT
ProbSamp <- WAVEsample
rm(list=c("WAVEsample","sample","slightT","lowT","highT"))


#and now for all subsequent samples
for(i in 2:nsamples){
  sample = data$sample_id==samples[i]
  WAVEsample <- data[sample,]
  WAVEsample$`1.High`<- prod(WAVEsample$`1.High`)
  WAVEsample$`2.Slight`<- prod(WAVEsample$`2.Slight`)
  WAVEsample$`3.Low`<- prod(WAVEsample$`3.Low`)
  WAVEsample <- unique(WAVEsample[c("sample_id","Collection.Date","1.High","2.Slight","3.Low")])
  highT <- WAVEsample$`1.High`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
  slightT <- WAVEsample$`2.Slight`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
  lowT <- WAVEsample$`3.Low`/(WAVEsample$`1.High`+WAVEsample$`2.Slight`+WAVEsample$`3.Low`)
  WAVEsample$`1.High`<- highT
  WAVEsample$`2.Slight`<- slightT
  WAVEsample$`3.Low`<- lowT
  ProbSamp <- merge(ProbSamp,WAVEsample,all=TRUE)
  rm(list=c("WAVEsample","sample","slightT","lowT","highT"))
}
rm(list=c("i","nsamples","samples"))

#restrict it to only those sites where Prob Low is >50%
ProbSamp<-unique(ProbSamp[c("sample_id","Collection.Date","3.Low")])
ProbSamp<-ProbSamp[ProbSamp$`3.Low`>.5,]
ProbSamp$PossiblyImpaired <-"yes"

# set the working directory 
setwd("C:/Rscripts/WAVE/2018")
write.csv(ProbSamp,file="Recommended.WAVE.Sites.csv",row.names=FALSE)


   #tidy up. This removes everything stored in short term memory
rm(list=ls())