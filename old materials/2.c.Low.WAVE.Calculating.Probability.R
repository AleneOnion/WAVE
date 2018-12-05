# 10/2017 Alene Onion
# This script uses previously calculated frequencies for each WAVE family in all SBU kick samples since 1990
# It then calculates the needed probabilities for the next step 

# set the working directory 
setwd("C:/Rscripts/WAVE/output")

#read the SBU data - all kick samples since 1990
Freq = read.csv("WAVE.Family.Frequencies.csv")

#Pull out the total row
tot = Freq$WAVE == "Total"
TOTAL <- Freq[tot,]


##################################################################################################################################
# We will be using na¨ive Bayes: the assumption of conditioanl independence
# (even when we know the data aren't conditionally independent)
#
# The formula for this calculation is: P((bap<5)|WAVE families in the sample)
#
#                                       P(bap<5)P(Family_1|(bap<5))P(Family_2|(bap<5))...etc
# = _____________________________________________________________________________________________________________________________
#   P(bap<5)P(Family_1|(bap<5))P(Family_2|(bap<5))...etc + P(bap>5)P(Family_1|(bap>5))P(Family_2|(bap>5))...etc
##################################################################################################################################

#Calculating P(Family|(bap<5)) referred to as PFamL
nbugs = length(Freq$WAVE)
k=NULL
Pr=NULL
temp= NULL

for (k in 2:nbugs){
  if(!exists("Prob")){
    Pr = Freq$Low[k]/TOTAL$Low
    Prob = data.frame(WAVE=Freq$WAVE[k],PFamL = Pr)
    Pr=NULL
  }
  if(exists("Prob")){
  Pr = Freq$Low[k]/TOTAL$Low
  temp = data.frame(WAVE=Freq$WAVE[k],PFamL = Pr)
  Prob <- merge(Prob,temp,all=TRUE)
  Pr=NULL
  }
}

#Calculating P(Family|(bap>5)) referred to as PFamLo
  nbugs = length(Freq$WAVE)
  k=NULL
  Pr=NULL
  temp= NULL
  
  for (k in 2:nbugs){
    if(!exists("Prob0")){
      Pr = ((Freq$Total[k])-(Freq$Low[k]))/((TOTAL$Total)-(TOTAL$Low))
      Prob0 = data.frame(WAVE=Freq$WAVE[k],PFamLo = Pr)
      Pr=NULL
  }
    if(exists("Prob0")){
      Pr = ((Freq$Total[k])-(Freq$Low[k]))/((TOTAL$Total)-(TOTAL$Low))
      temp = data.frame(WAVE=Freq$WAVE[k],PFamLo = Pr)
      Prob0 <- merge(Prob0,temp,all=TRUE)
      Pr=NULL
      temp=NULL
    }
  }

#Merging these two tables
Probability <- merge(Prob,Prob0,all=TRUE)

#Calculating P(bap<5) referred to as PbapL
PbapL = TOTAL$Low / TOTAL$Total
#Calculating P(bap>5) referred to as PbapLo
PbapLo = 1-PbapL
#Creating temp file and merging with 
temp = data.frame(WAVE="Pbap",PFamL=PbapL,PFamLo=PbapLo)  
Probability <- merge(temp,Probability,all=TRUE)

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(Probability,file="Low.WAVE.Family.Probabilities.csv",sep=",",row.names=FALSE)

  
#tidy up. This removes everything stored in short term memory
rm(list=ls())