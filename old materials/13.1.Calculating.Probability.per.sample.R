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
Prev = read.csv("WAVE.Family.Probabilities.csv")


#Pull out the P(bap>=7.5), P(5<bap<7.5), and P(bap<5) because these must be used with every calculation 
bap = Prev$WAVE == "Pbap"
Pbap <- Prev[bap,]
PbapH = Pbap$PfamH
PbapS = Pbap$Pfam
PbapL = Pbap$PFamL

#read the SBU data file with genspecies renamed to WAVE families
SBU = read.csv("SBU.Data.renamed.csv")

#############################################################################################################
# First lets examine only those samples with BAP>=7.5
#############################################################################################################

#Add the Pbap to the factor list for SBU
levels(SBU$WAVE) <- c(levels(SBU$WAVE),'Pbap')


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
PfamH = NULL
Pfam = NULL
PfamL = NULL
P1=NULL
P2 = NULL
temp= NULL
sample = NULL
prodH = NULL
prodS =NULL
prodL =NULL
ProbH = NULL
ProbS = NULL
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
      Pfam = P2$Pfam
      PfamL = P2$PFamL
      prodH = PbapH*PfamH
      prodS = PbapS*Pfam
      prodL = PbapL*PfamL
      PfamH = NULL
      Pfam = NULL
      PfamL = NULL
      P1 = NULL
      P2 = NULL
      }    else {
        P1 = Prev$WAVE == sample$WAVE[j]
        P2 <- Prev[P1,]
        PfamH = P2$PfamH
        Pfam = P2$Pfam
        PfamL = P2$PFamL
        prodH = prodH*PfamH
        prodS = prodS*Pfam
        prodL = prodL*PfamL
        PfamH = NULL
        Pfam = NULL
        PfamL = NULL
        P1=NULL
        P2 = NULL
      }
  }
  ProbH = prodH /(prodH+prodS+prodL)
  ProbS = prodS /(prodH+prodS+prodL)
  ProbL = prodL /(prodH+prodS+prodL)
  temp2 = data.frame(sample_id=(sample$sample_id[1]),bap=(sample$bap[1]),probHigh=ProbH,probSlight=ProbS,probLow=ProbL)
  test <- merge(test,temp2,all=TRUE)
  temp = NULL
  sample = NULL
  fam = NULL
  nfam = NULL
  prodH = NULL
  prodS =NULL
  prodL =NULL
  temp2=NULL
  ProbH = NULL
  ProbS = NULL
  ProbL = NULL
} 

#Convert NAs to 0
test[is.na(test)] <- 0

#sort the data by bap score and probH
test <- test[order(-test$bap,-test$probHigh),]

#plot these values
plot(test$bap,test$probHigh,type="p",lwd=4,col="deeppink1",main="All Families",xlab="BAP Score",ylab="Probability")
#lines(test$bap,test$probSlight,lwd=4,col="darkmagenta",type="p")
lines(test$bap,test$probLow,lwd=4,col="blue3",type="p")
abline(v=7.5)
abline(v=5)
abline(v=2.5)

#identify probHigh samples that have prob>0.95
test$pH95 <- test$probHigh
test$pH95 <- ifelse(test$pH95>0.95,1,0)
test$pS95 <- test$probSlight
test$pS95 <- ifelse(test$pS95>0.95,1,0)
test$pL95 <- test$probLow
test$pL95 <- ifelse(test$pL95>0.95,1,0)
test$impact <- test$bap
test$impact <- ifelse(test$bap>=7.5,"1.high",test$impact)
test$impact <- ifelse(test$bap>=5&test$bap<7.5,"2.slight",test$impact)
test$impact <- ifelse(test$bap<5,"3.low",test$impact)

#create box plot
test$prob <- NA
test$prob <- ifelse(test$pH95>0,"1.pH",test$prob)
test$prob <- ifelse(test$pS95>0,"2.pS",test$prob)
test$prob <- ifelse(test$pL95>0,"3.pL",test$prob)
#boxplot(test$bap~test$prob,data=test,main="All Families",xlab="Predicted Impact",ylab="BAP")

#count records in each category
library(plyr)
testc <- count(test,c("impact","prob"))

#create table
library(reshape)
testc <- cast(testc,impact~prob)
#replace NA values with 0
testc[is.na(testc)] <- 0
#create a total column
testc$total <- rowSums(testc)
testc$perc <- testc$`1.pH`/testc$total
#reorganize table
keep <- c("impact","total","1.pH","2.pS","3.pL","perc")
testc <- testc[keep]
rm(keep)
#print them in the output below
testc

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(test,file="Prob.Estimates.csv",sep=",",row.names=FALSE)
write.table(testc,file="Prob.Estimates.summary.csv",sep=",",row.names=FALSE)


#tidy up. This removes everything stored in short term memory
rm(list=ls())