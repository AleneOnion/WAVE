#2017.11.13
#Alene Onion
#calculating the frequency in each category

#read the SBU data file with genspecies renamed to WAVE families
SBU = read.csv("SBU.Data.renamed.csv")

#read the prob calculations
Probs = read.csv("Prob.Estimates.csv")


##################################################################################################
#Calculate the accuracy
##################################################################################################

#################################
#for SBU data
#pull out severe moderate, slight, and nonimpacted SBU samples
SBU <- unique(SBU[c("sample_id","coll_date","bap")])
low <- subset(SBU,SBU$bap<5)
slight <- subset(SBU,SBU$bap<7.5)
slight <- subset(slight,slight$bap>=5)
nonimpacted <- subset(SBU,SBU$bap>=7.5)

#count these records
#remove N values
low <- low[!is.na(low$bap),]
slight <- slight[!is.na(slight$bap),]
nonimpacted <- nonimpacted[!is.na(nonimpacted$bap),]
#convert to counts
low <- nrow(low)
slight <- nrow(slight)
nonimpacted <- nrow(nonimpacted)

#create table
TOTAL = data.frame(Group="TOTAL",High=nonimpacted,Slight=slight,Low=low)
rm(low)
rm(slight)
rm(nonimpacted)
rm(SBU)

#################################
#for Probs High data
#pull out probabilities greater than 0.95
Probs1 <- subset(Probs,Probs$probHigh>=0.95)
#pull out severe moderate, slight, and nonimpacted SBU samples
low <- subset(Probs1,Probs1$bap<5)
slight <- subset(Probs1,Probs1$bap<7.5)
slight <- subset(slight,slight$bap>=5)
nonimpacted <- subset(Probs1,Probs1$bap>=7.5)

#count these records
#remove N values
low <- low[!is.na(low$bap),]
slight <- slight[!is.na(slight$bap),]
nonimpacted <- nonimpacted[!is.na(nonimpacted$bap),]
#convert to counts
low <- nrow(low)
slight <- nrow(slight)
nonimpacted <- nrow(nonimpacted)

#create table
High = data.frame(Group="PN",High=nonimpacted,Slight=slight,Low=low)
rm(low)
rm(slight)
rm(nonimpacted)
rm(Probs1)

#################################
#for Probs Slight data
#pull out probabilities greater than 0.95
Probs1 <- subset(Probs,Probs$probSlight>=0.95)
#pull out severe moderate, slight, and nonimpacted SBU samples
low <- subset(Probs1,Probs1$bap<5)
slight <- subset(Probs1,Probs1$bap<7.5)
slight <- subset(slight,slight$bap>=5)
nonimpacted <- subset(Probs1,Probs1$bap>=7.5)

#count these records
#remove N values
low <- low[!is.na(low$bap),]
slight <- slight[!is.na(slight$bap),]
nonimpacted <- nonimpacted[!is.na(nonimpacted$bap),]
#convert to counts
low <- nrow(low)
slight <- nrow(slight)
nonimpacted <- nrow(nonimpacted)

#create table
Slight = data.frame(Group="PS",High=nonimpacted,Slight=slight,Low=low)
rm(low)
rm(slight)
rm(nonimpacted)
rm(Probs1)

#################################
#for Probs Low data
#pull out probabilities greater than 0.95
Probs1 <- subset(Probs,Probs$probLow>=0.95)
#pull out severe moderate, slight, and nonimpacted SBU samples
low <- subset(Probs1,Probs1$bap<5)
slight <- subset(Probs1,Probs1$bap<7.5)
slight <- subset(slight,slight$bap>=5)
nonimpacted <- subset(Probs1,Probs1$bap>=7.5)

#count these records
#remove N values
low <- low[!is.na(low$bap),]
slight <- slight[!is.na(slight$bap),]
nonimpacted <- nonimpacted[!is.na(nonimpacted$bap),]
#convert to counts
low <- nrow(low)
slight <- nrow(slight)
nonimpacted <- nrow(nonimpacted)

#create table
Low = data.frame(Group="PL",High=nonimpacted,Slight=slight,Low=low)
rm(low)
rm(slight)
rm(nonimpacted)
rm(Probs1)

#################################
#merge tables
Table <- merge(TOTAL,High,all=TRUE)
Table <- merge(Table,Slight,all=TRUE)
Table <- merge(Table,Low,all=TRUE)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(Table,file="table.csv",sep=",",row.names=FALSE)


#tidy up. This removes everything stored in short term memory
rm(list=ls())