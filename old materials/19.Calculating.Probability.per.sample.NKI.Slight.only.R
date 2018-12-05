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
#   P(bap>=7.5)P(Family_1|(bap>=7.5))...etc + P(bap<7.5)P(Family_1|(bap<7.5))...etc +
##################################################################################################################################

#Rewritten using the terms used in this script it's:
#
#                     (PbapH)(PfamH1)(PfamH2)...etc
# = ____________________________________________________________________
#   (PbapH)(PfamH1)(PfamH2)...etc + (PbapL)(PFamL1)(PFamL2)...etc
##################################################################################################################################

# set the working directory 
setwd("C:/Rscripts/WAVE/output")

#read the pre calculated probabilities file
Prev = read.csv("WAVE.Family.Probabilities.csv")


#Pull out the P(bap>=7.5), P(5<bap<7.5), and P(bap<5) because these must be used with every calculation 
bap = Prev$WAVE == "Pbap"
Pbap <- Prev[bap,]
PbapH = Pbap$PfamH
PbapL = Pbap$PFamL

#read the SBU data file with genspecies renamed to WAVE families
SBU = read.csv("SBU.Data.renamed.csv")

#############################################################################################################
# First lets examine only those samples with BAP>=7.5
#############################################################################################################

#Add the Pbap to the factor list for SBU
levels(SBU$WAVE) <- c(levels(SBU$WAVE),'Pbap')


#Create a table to enter these values into
test = data.frame(sample_id=NA,bap=NA,probHigh=NA,probLow=NA)

#Calculating the probabilities for each sample and for each High,  Low category
samp = unique(SBU$sample_id)
nsamp = length(samp)
temp = NULL
sample = NULL
fam = NULL
nfam = NULL
k=NULL
j=NULL
PfamH = NULL
PfamL = NULL
P1=NULL
P2 = NULL
temp= NULL
sample = NULL
prodH = NULL
prodL =NULL
ProbH = NULL
ProbL = NULL
temp2=NULL

for (k in 1:nsamp){
  temp = SBU$sample_id == samp[k]
  sample <- SBU[temp,]
  fam = unique(sample$WAVE)
  nfam = length(fam)
  for(j in 1:nfam){
    if(is.null(prodH)) {
      P1 = Prev$WAVE == sample$WAVE[j]
      P2 <- Prev[P1,]
      PfamH = P2$PfamH
      PfamL = P2$PFamL
      prodH = PbapH*PfamH
      prodL = PbapL*PfamL
      PfamH = NULL
      PfamL = NULL
      P1 = NULL
      P2 = NULL
      }    else {
        P1 = Prev$WAVE == sample$WAVE[j]
        P2 <- Prev[P1,]
        PfamH = P2$PfamH
        PfamL = P2$PFamL
        prodH = prodH*PfamH
        prodL = prodL*PfamL
        PfamH = NULL
        PfamL = NULL
        P1=NULL
        P2 = NULL
      }
  }
  ProbH = prodH /(prodH+prodL)
  ProbL = prodL /(prodH+prodL)
  temp2 = data.frame(sample_id=(sample$sample_id[1]),bap=(sample$bap[1]),probHigh=ProbH,probLow=ProbL)
  test <- merge(test,temp2,all=TRUE)
  temp = NULL
  sample = NULL
  fam = NULL
  nfam = NULL
  prodH = NULL
  prodL =NULL
  temp2=NULL
  ProbH = NULL
  ProbL = NULL
} 

#Convert NAs to 0
test[is.na(test)] <- 0

#sort the data by bap score and probH
test <- test[order(-test$bap,-test$probHigh),]

#plot these values
plot(test$bap,test$probHigh,type="p",lwd=4,col="deeppink1",xlab="BAP Score",ylab="Probability")
lines(test$bap,test$probLow,lwd=4,col="blue3",type="p")
abline(v=7.5)
abline(v=5)
abline(v=2.5)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(test,file="Prob.Estimates.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())