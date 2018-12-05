# 10/2017 Alene Onion
# This script uses previously calculated frequencies for each WAVE family in all SBU kick samples since 1990
# It then calculates the needed probabilities for the next step 

# set the working directory 
setwd("C:/Rscripts/WAVE/for.6.or.More/output")

#read the SBU data - all kick samples since 1990
Freq = read.csv("WAVE.Family.Frequencies.csv")

#Pull out the total row
tot = Freq$WAVE == "Total"
TOTAL <- Freq[tot,]


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

#Calculating P(Family|(bap>=7.5)) referred to as PFamH
nbugs = length(Freq$WAVE)
k=NULL
Pr=NULL
temp= NULL

for (k in 2:nbugs){
  if(!exists("Prob")){
    Pr = Freq$High[k]/TOTAL$High
    Prob = data.frame(WAVE=Freq$WAVE[k],PfamH = Pr)
    Pr=NULL
  }
  if(exists("Prob")){
  Pr = Freq$High[k]/TOTAL$High
  temp = data.frame(WAVE=Freq$WAVE[k],PfamH = Pr)
  Prob <- merge(Prob,temp,all=TRUE)
  Pr=NULL
  }
}

#renaming this table
Probability1 <- Prob
Prob = NULL

#Calculating P(bap>=7.5) referred to as PbapH
PbapH = TOTAL$High / TOTAL$Total
#Creating temp file and merging with 
temp = data.frame(WAVE="Pbap",PfamH=PbapH)  
Probability1 <- merge(temp,Probability1,all=TRUE)



#Calculating P(Family|(5<bap<7.5)) referred to as Pfam
nbugs = length(Freq$WAVE)
k=NULL
Pr=NULL
temp= NULL

for (k in 2:nbugs){
  if(!exists("Prob")){
    Pr = Freq$Slight[k]/TOTAL$Slight
    Prob = data.frame(WAVE=Freq$WAVE[k],Pfam = Pr)
    Pr=NULL
  }
  if(exists("Prob")){
    Pr = Freq$Slight[k]/TOTAL$Slight
    temp = data.frame(WAVE=Freq$WAVE[k],Pfam = Pr)
    Prob <- merge(Prob,temp,all=TRUE)
    Pr=NULL
  }
}

#renaming this table
Probability2 <- Prob
Prob = NULL

#Calculating P(5<bap<7.5) referred to as Pbap
Pbap = TOTAL$Slight / TOTAL$Total
#Creating temp file and merging with 
temp = data.frame(WAVE="Pbap",Pfam=Pbap)  
Probability2 <- merge(temp,Probability2,all=TRUE)


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


#renaming this table
Probability3 <- Prob
Prob = NULL

#Calculating P(bap<5) referred to as PbapL
PbapL = TOTAL$Low / TOTAL$Total
#Creating temp file and merging with 
temp = data.frame(WAVE="Pbap",PFamL=PbapL)  
Probability3 <- merge(temp,Probability3,all=TRUE)

#Combine these tables
Combined <- merge(Probability1,Probability2,by=c("WAVE"), all=TRUE)
Combined <- merge(Combined,Probability3,by=c("WAVE"), all=TRUE)

#Convert NAs to 0
Combined[is.na(Combined)] <- 0

# write the table
setwd("C:/Rscripts/WAVE/for.6.or.More/output")
write.table(Combined,file="WAVE.Family.Probabilities.csv",sep=",",row.names=FALSE)

  
#tidy up. This removes everything stored in short term memory
rm(list=ls())