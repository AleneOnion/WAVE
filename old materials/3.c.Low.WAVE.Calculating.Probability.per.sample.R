# 10/2017 Alene Onion
# This script uses previously calculated probabilities for each WAVE family based on SBU kick samples since 1990
# to calculate P((bap<5)|WAVE families in the sample) 

##################################################################################################################################
# We will be using na¨ive Bayes: the assumption of conditioanl independence
# (even when we know the data aren't conditionally independent)
#
# The formula for this calculation is: P((bap<5)|WAVE families in the sample)
#
#                                       P(bap<5)P(Family_1|(bap<5))P(Family_2|(bap<5))...etc
# = _____________________________________________________________________________________________________________________________
#   P(bap<5)P(Family_1|(bap<5))P(Family_2|(bap<5))...etc + P(bap>5)P(Family_1|(bap>5))P(Family_2|(bap>5))...etc
#
#Rewritten using the terms used in this script it's:
#
#                     (PbapL)(PFamL1)(PFamL2)...etc
# = ____________________________________________________________________
#   (PbapL)(PFamL1)(PFamL2)...etc + (PbapL0)(PFamL01)(PFamL02)...etc
##################################################################################################################################

# set the working directory 
setwd("C:/Rscripts/WAVE/output")

#read the pre calculated probabilities file
Prob = read.csv("Low.WAVE.Family.Probabilities.csv")

#Pull out the P(bap<5) and P(bap<7,5) because these must be used with every calculation 
bap = Prob$WAVE == "Pbap"
Pbap <- Prob[bap,]
PbapL = Pbap$PFamL
PbapL0 = Pbap$PFamLo

#read the SBU data file with genspecies renamed to WAVE families
SBU = read.csv("SBU.Data.renamed.csv")

#############################################################################################################
# First lets examine only those samples with BAP<5
#############################################################################################################

#Add the Pbap to the factor list for SBU
levels(SBU$WAVE) <- c(levels(SBU$WAVE),'Pbap')


#Create a table to enter these values into
test = data.frame(sample_id=NA,bap=NA,probability=NA)

#Calculating the P((bap<5)|WAVE families in the sample) for each sample with BAP>7.5
samp = unique(SBU$sample_id)
nsamp = length(samp)
k=NULL
j=NULL
P1=NULL
P2 = NULL
temp= NULL
sample = NULL
prod = NULL
prod0=NULL
P3 = NULL
temp2=NULL

for (k in 1:nsamp){
  temp = SBU$sample_id == samp[k]
  sample <- SBU[temp,]
  fam = unique(sample$WAVE)
  nfam = length(fam)
  for(j in 1:nfam){
    if(is.null(prod)) {
      P1 = Prob$WAVE == sample$WAVE[j]
      P2 <- Prob[P1,]
      PFamL = P2$PFamL
      PFamL0 = P2$PFamLo
      prod = PbapL*PFamL
      prod0 = PbapL0*PFamL0
      P1=NULL
      P2 = NULL
      }    else {
      P1 = Prob$WAVE == sample$WAVE[j]
      P2 <- Prob[P1,]
      PFamL = P2$PFamL
      PFamL0 = P2$PFamLo
      prod = PFamL*prod
      prod0 = PFamL0*prod0
      P1=NULL
      P2 = NULL
      }
  }
  P3 = prod /(prod+prod0)
  temp2 = data.frame(sample_id=(sample$sample_id[1]),bap=(sample$bap[1]),probability=P3)
  test <- merge(test,temp2,all=TRUE)
  temp= NULL
  sample = NULL
  prod = NULL
  prod0=NULL
  P3 = NULL
  temp2=NULL
}

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(test,file="BAPunder5.Prob.Estimates.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())