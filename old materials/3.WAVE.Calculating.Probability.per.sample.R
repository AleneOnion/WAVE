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
#   P(bap>=7.5)P(Family_1|(bap>=7.5))P(Family_2|(bap>=7.5))...etc + P(bap<7.5)P(Family_1|(bap<7.5))P(Family_2|(bap<7.5))...etc
#
#Rewritten using the terms used in this script it's:
#
#                     (PbapH)(PfamH1)(PfamH2)...etc
# = ____________________________________________________________________
#   (PbapH)(PfamH1)(PfamH2)...etc + (PbapH0)(PfamH01)(PfamH02)...etc
##################################################################################################################################

# set the working directory 
setwd("C:/Rscripts/WAVE/output")

#read the pre calculated probabilities file
Prob = read.csv("WAVE.Family.Probabilities.csv")

#Pull out the P(bap>=7.5) and P(bap<7,5) because these must be used with every calculation
bap = Prob$WAVE == "Pbap"
Pbap <- Prob[bap,]
PbapH = Pbap$PfamH
PbapH0 = Pbap$PfamHo

#read the SBU data file with genspecies renamed to WAVE families
SBUrenamed = read.csv("SBU.Data.renamed.csv")

#############################################################################################################
# First lets examine only those samples with BAP>=7.5
#############################################################################################################

#pull out data with BAP greater than 7.5
h1 = SBUrenamed$bap >= 7.5
SBUH <- SBUrenamed[h1,]
#Add the Pbap to the factor list for SBUH
levels(SBUH$WAVE) <- c(levels(SBUH$WAVE),'Pbap')


#Create a table to enter these values into
test = data.frame(sample_id=NA,bap=NA,probability=NA)

#Calculating the P((bap>=7.5)|WAVE families in the sample) for each sample with BAP>7.5
samp = unique(SBUH$sample_id)
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
  temp = SBUH$sample_id == samp[k]
  sample <- SBUH[temp,]
  fam = unique(sample$WAVE)
  nfam = length(fam)
  for(j in 1:nfam){
    if(is.null(prod)) {
      P1 = Prob$WAVE == sample$WAVE[j]
      P2 <- Prob[P1,]
      PfamH = P2$PfamH
      PfamH0 = P2$PfamHo
      prod = PbapH*PfamH0
      prod0 = PbapH0*PfamH0
      P1=NULL
      P2 = NULL
      }    else {
      P1 = Prob$WAVE == sample$WAVE[j]
      P2 <- Prob[P1,]
      PfamH = P2$PfamH
      PfamH0 = P2$PfamHo
      prod = PbapH*PfamH0*prod
      prod0 = PbapH0*PfamH0*prod0
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


#tidy up. This removes everything stored in short term memory
rm(list=ls())