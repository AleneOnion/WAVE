# 10/2017 Alene Onion
# This script uses previously calculated frequencies for each WAVE family in all SBU kick samples since 1990
# It then calculates the needed probabilities for the next step 

# set the working directory 
setwd("C:/Rscripts/WAVE/output")

#read the SBU data - all kick samples since 1990
Freq = read.csv("FamilyPair.Frequencies.csv")

#Pull out the total row
tot = Freq$Family1 == "total"
TOTAL <- Freq[tot,]
#Remove NA values
TOTAL <- TOTAL[!is.na(TOTAL$High),]


##################################################################################################################################
# We will be using na¨ive Bayes: the assumption of conditioanl independence
# (even when we know the data aren't conditionally independent)
#
# The formula for this calculation is: P((bap>=7.5)|WAVE family pairs in the sample)
#
#                                       P(bap>=7.5)P(FamilyPair1|(bap>=7.5))P(FamilyPair2|(bap>=7.5))...etc
# = _____________________________________________________________________________________________________________________________
#   P(bap>=7.5)P(FamilyPair1|(bap>=7.5))...etc + P(5<bap<7.5)P(FamilyPair1|(5<bap<7.5))...etc + P(bap<5)P(FamilyPair1|(bap<5))...etc
##################################################################################################################################

#Prepare the Freq table
#Remove the total row
Freq <- Freq[!(rownames(Freq) %in% rownames(TOTAL)),]
#sort the Freq data by Family1 then Family2
Freq <- Freq[order(Freq$Family1,Freq$Family2),]
#Create a column labeled Family1_Family2
Freq$Pair <- do.call(paste,c(Freq[c("Family1","Family2")],sep="_"))
#Remove NA values
Freq <- Freq[!is.na(Freq$High),]


#Calculating P(FamilyPair|(bap>=7.5)) referred to as PFamH
nbugs = length(Freq$Pair)

for (k in 1:nbugs){
  if(!exists("Prob")){
    Pr = Freq$High[k]/TOTAL$High
    Prob = data.frame(Pair=Freq$Pair[k],PfamH = Pr)
    rm(Pr)
  }
  if(exists("Prob")){
  Pr = Freq$High[k]/TOTAL$High
  temp = data.frame(Pair=Freq$Pair[k],PfamH = Pr)
  Prob <- merge(Prob,temp,all=TRUE)
  rm(Pr)
  }
}

#renaming this table
Probability1 <- Prob
rm(Prob)

#Calculating P(bap>=7.5) referred to as PbapH
PbapH = TOTAL$High / TOTAL$Total
#Creating temp file and merging with 
temp = data.frame(Pair="Pbap",PfamH=PbapH)  
Probability1 <- merge(temp,Probability1,all=TRUE)



#Calculating P(Family|(5<bap<7.5)) referred to as Pfam
nbugs = length(Freq$Pair)

for (k in 1:nbugs){
  if(!exists("Prob")){
    Pr = Freq$Slight[k]/TOTAL$Slight
    Prob = data.frame(Pair=Freq$Pair[k],Pfam = Pr)
    rm(Pr)
  }
  if(exists("Prob")){
    Pr = Freq$Slight[k]/TOTAL$Slight
    temp = data.frame(Pair=Freq$Pair[k],Pfam = Pr)
    Prob <- merge(Prob,temp,all=TRUE)
    rm(Pr)
  }
}

#renaming this table
Probability2 <- Prob
rm(Prob)

#Calculating P(5<bap<7.5) referred to as Pbap
Pbap = TOTAL$Slight / TOTAL$Total
#Creating temp file and merging with 
temp = data.frame(Pair="Pbap",Pfam=Pbap)  
Probability2 <- merge(temp,Probability2,all=TRUE)


#Calculating P(Family|(bap<5)) referred to as PFamL
nbugs = length(Freq$Pair)

for (k in 1:nbugs){
  if(!exists("Prob")){
    Pr = Freq$Low[k]/TOTAL$Low
    Prob = data.frame(Pair=Freq$Pair[k],PFamL = Pr)
    rm(Pr)
  }
  if(exists("Prob")){
    Pr = Freq$Low[k]/TOTAL$Low
    temp = data.frame(Pair=Freq$Pair[k],PFamL = Pr)
    Prob <- merge(Prob,temp,all=TRUE)
    rm(Pr)
  }
}


#renaming this table
Probability3 <- Prob
rm(Prob)

#Calculating P(bap<5) referred to as PbapL
PbapL = TOTAL$Low / TOTAL$Total
#Creating temp file and merging with 
temp = data.frame(Pair="Pbap",PFamL=PbapL)  
Probability3 <- merge(temp,Probability3,all=TRUE)

#Combine these tables
Combined <- merge(Probability1,Probability2,by=c("Pair"), all=TRUE)
Combined <- merge(Combined,Probability3,by=c("Pair"), all=TRUE)

#Convert NAs to 0
Combined[is.na(Combined)] <- 0

# write the table
setwd("C:/Rscripts/WAVE/output")
write.table(Combined,file="Family.Pair.Probabilities.csv",sep=",",row.names=FALSE)

  
#tidy up. This removes everything stored in short term memory
rm(list=ls())