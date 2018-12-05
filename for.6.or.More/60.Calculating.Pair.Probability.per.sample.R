# 10/2017 Alene Onion
# This script uses previously calculated probabilities for each WAVE family based on SBU kick samples since 1990
# to calculate P((bap>=7.5)|WAVE families in the sample) 

##################################################################################################################################
# We will be using na¨ive Bayes: the assumption of conditioanl independence
# (even when we know the data aren't conditionally independent)
#
# The formula for this calculation is: P((bap>=7.5)|WAVE families in the sample)
#
#                                       P(bap>=7.5)P(Family_1|(bap>=7.5))P(Family_2|(bap>=7.5))...etc
# = _____________________________________________________________________________________________________________________________
#   P(bap>=7.5)P(Family_1|(bap>=7.5))...etc + P(5<bap<7.5)P(Family_1|(5<bap<7.5))...etc + P(bap<5)P(Family_1|(bap<5))...etc
##################################################################################################################################

#Rewritten using the terms used in this script it's:
#
#                     (PbapH)(PfamH1)(PfamH2)...etc
# = ____________________________________________________________________
#   (PbapH)(PfamH1)(PfamH2)...etc + (Pbap)(Pfam1)(Pfam2)...etc + (PbapL)(PFamL1)(PFamL2)...etc
##################################################################################################################################

# set the working directory 
setwd("C:/Rscripts/WAVE/output")

#read the pre calculated probabilities file
Prev = read.csv("Family.Pair.Probabilities.csv")

#Pull out the P(bap>=7.5), P(5<bap<7.5), and P(bap<5) because these must be used with every calculation 
bap = Prev$Pair == "Pbap"
Pbap <- Prev[bap,]
PbapH = Pbap$PfamH
PbapS = Pbap$Pfam
PbapL = Pbap$PFamL
#Remove the Pbap values from the Prev table
Prev <- Prev[!(rownames(Prev) %in% rownames(Pbap)),]

#read the SBU data file with genspecies renamed to WAVE families
SBU = read.csv("SBU.Data.renamed.csv")
#sort by sample ID, date, bap, family
SBU <- SBU[order(SBU$sample_id,SBU$coll_date,SBU$bap,SBU$WAVE),]



#############################################################################################################
# First lets examine only those samples with BAP>=7.5
#############################################################################################################

#Create a table to enter these values into
test = data.frame(sample_id=NA,bap=NA,probHigh=NA,probSlight=NA,probLow=NA)

#Calculating the probabilities for each sample and for each High, Slight, Low category
samp = unique(SBU$sample_id)
nsamp = length(samp)
temp = NULL
sample = NULL
fam = NULL
nfam = NULL
k=NULL
j=NULL
h=NULL
i=NULL
PfamH = NULL
Pfam = NULL
PfamL = NULL
P1=NULL
P2 = NULL
temp= NULL
sample = NULL
ProbH = NULL
ProbS = NULL
ProbL = NULL
temp2=NULL
pairtemp=NULL

for (k in 1:nsamp){
  temp = SBU$sample_id == samp[k]
  sample <- SBU[temp,]
  fam = unique(sample$WAVE)
  nfam = length(fam)
  nfamstop = nfam-1
  prodH = PbapH
  prodS = PbapS
  prodL = PbapL
  
  if(nfam<2){
    print (paste(samp[k],"has only one family"))
  }else{
  
   for(j in 1:nfamstop){
      h = j+1
      for(i in h:nfam){
        pairtemp = paste(fam[j],fam[i],sep="_")
          P1 = Prev$Pair == pairtemp
          P2 <- Prev[P1,]
          prodH = prodH * (P2$PfamH)
          prodS = prodS * (P2$Pfam)
          prodL = prodL * (P2$PFamL)
          #a test of the script          
              print (paste(samp[k],pairtemp,P2$PfamH,P2$Pfam,P2$PFamL,prodH,prodS,prodL))
          P1=NULL
          P2 = NULL
        }
      pairtemp=NULL
        }
  ProbH = prodH /(prodH+prodS+prodL)
  ProbS = prodS /(prodH+prodS+prodL)
  ProbL = prodL /(prodH+prodS+prodL)
  temp2 = data.frame(sample_id=(sample$sample_id[1]),bap=(sample$bap[1]),probHigh=ProbH,probSlight=ProbS,probLow=ProbL)
  #now merge
  test <- merge(test,temp2,all=TRUE)
  }
}

#sort the data by bap score and probH
test <- test[order(-test$bap,-test$probHigh),]

#plot these values
plot(test$bap,test$probHigh,type="p",lwd=4,col="deeppink1",xlab="BAP Score",ylab="Probability")
lines(test$bap,test$probSlight,lwd=4,col="darkmagenta",type="p")
lines(test$bap,test$probLow,lwd=4,col="blue3",type="p")
abline(v=7.5)
abline(v=5)
abline(v=2.5)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(test,file="Pair.Prob.Estimates.per.sample.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())