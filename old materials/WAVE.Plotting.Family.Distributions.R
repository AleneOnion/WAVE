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

SBU = read.csv("all_samples.csv")

#Pull bug names from the table
Most = read.csv("Most.csv")
#convert the names to characters
Most$Most <- as.character(Most$Most)
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
  name2 = Most$Most[k]
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
MostSubset <- count(SBUSubset2,c("sample_id","coll_date","bap","WAVE"))
keep2 <- c("sample_id","coll_date","bap","WAVE")
MostSubset <- MostSubset[keep2]

#round the BAP scores to the nearest 0.2 while keeping the original file to print later
Rounded <- MostSubset
Rounded[,"bap"]= round(Rounded[,"bap"]/.2)*.2

#Create file with only sample ID, date, and BAP -> meaning only one record per sample
SBUprint <- count(SBU,c("sample_id","coll_date","bap"))
#remove the freq from this file
keep1 <- c("sample_id","coll_date","bap")
SBUprint <- SBUprint[keep1]
#round these values
roundSBU <- SBUprint
roundSBU[,"bap"]= round(roundSBU[,"bap"]/.2)*.2
#Create a BAP score count
AllPlot <- count(roundSBU,c("bap"))

#######################################################################################
#Create plots
#######################################################################################

#open the frequencies table to add to the plot legend
setwd("C:/Rscripts/WAVE/output")
Freq = read.csv("WAVE.Family.Frequencies.csv")

families = c('Athericidae','Baetiscidae','Brachycentridae','Caenidae','Capniidae','Chloroperlidae','Corydalidae','Ephemerellidae','Ephemeridae','Glossosomatidae','Gomphidae','Helicopsychidae','Heptageniidae','Hydroptilidae','Isonychiidae','Lepidostomatidae','Leptohyphidae','Leptophlebiidae','Leuctridae','Nemouridae','Odontoceridae','Peltoperlidae','Perlidae','Perlodidae','Philopotamidae','Polycentropodidae','Polymitarcyidae','Potamanthidae','Psephenidae','Pteronarcidae','Rhyacophilidae','Uenoidae')
nfamilies = length(families)
j=NULL
k=NULL
l=NULL
m=NULL

subFamily = NULL

for (j in 1:nfamilies){
  k = paste(families[j],".plot.jpg",sep="")
  jpeg(k)
  subFamily <- subset(Rounded,WAVE == families[j])
  FamilyPlot <- count(subFamily,c("bap"))
  plot(AllPlot$bap,AllPlot$freq,type="l",lwd=4,xlab="BAP Score",ylab="Number of Sites")
  lines(FamilyPlot$bap,FamilyPlot$freq,lwd=4,col="dodgerblue",type="l")
  abline(v=7.5)
  abline(v=5)
  abline(v=2.5)
#Adding frequencies to the legend
  fam = paste(families[j]," (",Freq[j,2],",",Freq[j,4],",",Freq[j,3],")")
  legend("topleft",lty=1,legend=c("All Sites (951,819,361)",fam),lwd=c(2,2),col=c("black","dodgerblue"))
  dev.off()
}



#tidy up. This removes everything stored in short term memory
rm(list=ls())