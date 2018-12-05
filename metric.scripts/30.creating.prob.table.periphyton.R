#working with the matched UserP and SBU data table
#this script 
#2017.12.4
#Alene Onion

#read matched table
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data")
matched <- read.csv("2017.matched.UserP.csv")

#keep only the columns I need
keep <- c("LOCATION","RIVMILLE","coll_date","LATITUDE","LONGITUDE","bap","PRIM_CONTACT","SEC_CONTACT","WATER_CLARITY","SUSP_PHYTOPLANKTON","PERIPHYTON","ODOR","TRASH","DISCHARGE_PIPES")
matched <- matched[keep]
rm(keep)

#remove NA values
junk <- !is.na(matched$WATER_CLARITY)
matched <- matched[junk,]
junk <- !is.na(matched$SUSP_PHYTOPLANKTON)
matched <- matched[junk,]
junk <- !is.na(matched$PERIPHYTON)
matched <- matched[junk,]
junk <- !is.na(matched$ODOR)
matched <- matched[junk,]
junk <- !is.na(matched$TRASH)
matched <- matched[junk,]
junk <- !is.na(matched$DISCHARGE_PIPES)
matched <- matched[junk,]
rm(junk)

#convert param values to numeric
matched$WATER_CLARITY <- as.numeric(matched$WATER_CLARITY)
matched$SUSP_PHYTOPLANKTON <- as.numeric(matched$SUSP_PHYTOPLANKTON)
matched$PERIPHYTON <- as.numeric(matched$PERIPHYTON)
matched$ODOR <- as.numeric(matched$ODOR)
matched$TRASH <- as.numeric(matched$TRASH)
matched$DISCHARGE_PIPES <- as.numeric(matched$DISCHARGE_PIPES)

#create an impact column
matched$impact <- matched$bap
matched$impact <- ifelse(matched$bap>=7.5,"1.high",matched$impact)
matched$impact <- ifelse(matched$bap>=5&matched$bap<7.5,"2.slight",matched$impact)
matched$impact <- ifelse(matched$bap<5,"3.low",matched$impact)

#convert the contact values to upper case
matched$PRIM_CONTACT <- toupper(matched$PRIM_CONTACT)
matched$SEC_CONTACT <- toupper(matched$SEC_CONTACT)

#convert the weird entries to simpler form
matched$PRIM_CONTACT <- gsub("A ","A",matched$PRIM_CONTACT)
matched$PRIM_CONTACT <- gsub("C-F","F",matched$PRIM_CONTACT)
matched$PRIM_CONTACT <- gsub("C/D","D",matched$PRIM_CONTACT)
matched$SEC_CONTACT <- gsub("B/C","C",matched$SEC_CONTACT)
matched$SEC_CONTACT <- gsub("B/D","D",matched$SEC_CONTACT)

#Convert prim and sec contact to ranked values
matched$PRIM_CONTACT <- ifelse(matched$PRIM_CONTACT == "A","a.Beautiful",matched$PRIM_CONTACT)
matched$PRIM_CONTACT <- ifelse(matched$PRIM_CONTACT == "B","b.Minor",matched$PRIM_CONTACT)
matched$PRIM_CONTACT <- ifelse(matched$PRIM_CONTACT == "C","c.Impacted",matched$PRIM_CONTACT)
matched$PRIM_CONTACT <- ifelse(matched$PRIM_CONTACT == "D","d.Reduced",matched$PRIM_CONTACT)
matched$PRIM_CONTACT <- ifelse(matched$PRIM_CONTACT == "E","e.Awful",matched$PRIM_CONTACT)
matched$SEC_CONTACT <- ifelse(matched$SEC_CONTACT == "A","a.Beautiful",matched$SEC_CONTACT)
matched$SEC_CONTACT <- ifelse(matched$SEC_CONTACT == "B","b.Minor",matched$SEC_CONTACT)
matched$SEC_CONTACT <- ifelse(matched$SEC_CONTACT == "C","c.Impacted",matched$SEC_CONTACT)
matched$SEC_CONTACT <- ifelse(matched$SEC_CONTACT == "D","d.Reduced",matched$SEC_CONTACT)
matched$SEC_CONTACT <- ifelse(matched$SEC_CONTACT == "E","e.Awful",matched$SEC_CONTACT)

#create histograms of slight and impaired
junk <- matched$impact == "2.slight"
slight <- matched[junk,]
junk <- matched$impact == "3.low"
impaired <- matched[junk,]
#create histograms
par(mfrow=c(2,1))
hist(slight$WATER_CLARITY)
hist(impaired$WATER_CLARITY,col = "gray")
par(mfrow=c(2,1))
hist(slight$SUSP_PHYTOPLANKTON)
hist(impaired$SUSP_PHYTOPLANKTON,col = "gray")
par(mfrow=c(2,1))
hist(slight$PERIPHYTON)
hist(impaired$PERIPHYTON,col = "gray")
par(mfrow=c(2,1))
hist(slight$ODOR)
hist(impaired$ODOR,col = "gray")
par(mfrow=c(2,1))
hist(slight$TRASH)
hist(impaired$TRASH,col = "gray")
par(mfrow=c(2,1))
hist(slight$DISCHARGE_PIPES)
hist(impaired$DISCHARGE_PIPES,col = "gray")

#create box plots
boxplot(matched$bap~matched$PRIM_CONTACT,data=matched,main="Primary Contact",xlab="Primary Contact",ylab="BAP")
boxplot(matched$bap~matched$SEC_CONTACT,data=matched,main="Secondary Contact",xlab="Secondary Contact",ylab="BAP")
boxplot(matched$bap~matched$WATER_CLARITY,data=matched,main="WATER_CLARITY",xlab="WATER_CLARITY (1-10)",ylab="BAP")
boxplot(matched$bap~matched$SUSP_PHYTOPLANKTON,data=matched,main="SUSP_PHYTOPLANKTON",xlab="SUSP_PHYTOPLANKTON (1-10)",ylab="BAP")
boxplot(matched$bap~matched$PERIPHYTON,data=matched,main="Periphyton",xlab="Periphyton (1-10)",ylab="BAP")
boxplot(matched$bap~matched$ODOR,data=matched,main="ODOR",xlab="ODOR (1-10)",ylab="BAP")
boxplot(matched$bap~matched$TRASH,data=matched,main="TRASH",xlab="TRASH (1-10)",ylab="BAP")
boxplot(matched$bap~matched$DISCHARGE_PIPES,data=matched,main="DISCHARGE_PIPES",xlab="DISCHARGE_PIPES (1-10)",ylab="BAP")

#Reduce the rankings for individual features to binary - 0, 1 (for 1-4), 2 (for 5-10)
#this breakdown is based on an observation in the box plots of the raw data. There seems to be a break at 1 and 5 in all params
matched$WATER_CLARITY <- ifelse(matched$WATER_CLARITY>4,"zigh",matched$WATER_CLARITY)
matched$WATER_CLARITY <- ifelse(matched$WATER_CLARITY>0&matched$WATER_CLARITY<5,"med",matched$WATER_CLARITY)
matched$SUSP_PHYTOPLANKTON <- ifelse(matched$SUSP_PHYTOPLANKTON>4,"zigh",matched$SUSP_PHYTOPLANKTON)
matched$SUSP_PHYTOPLANKTON <- ifelse(matched$SUSP_PHYTOPLANKTON>0&matched$SUSP_PHYTOPLANKTON<5,"med",matched$SUSP_PHYTOPLANKTON)
matched$PERIPHYTON <- ifelse(matched$PERIPHYTON>4,"zigh",matched$PERIPHYTON)
matched$PERIPHYTON <- ifelse(matched$PERIPHYTON>0&matched$PERIPHYTON<5,"med",matched$PERIPHYTON)
matched$ODOR <- ifelse(matched$ODOR>4,"zigh",matched$ODOR)
matched$ODOR <- ifelse(matched$ODOR>0&matched$ODOR<5,"med",matched$ODOR)
matched$TRASH <- ifelse(matched$TRASH>4,"zigh",matched$TRASH)
matched$TRASH <- ifelse(matched$TRASH>0&matched$TRASH<5,"med",matched$TRASH)
matched$DISCHARGE_PIPES <- ifelse(matched$DISCHARGE_PIPES>4,"zigh",matched$DISCHARGE_PIPES)
matched$DISCHARGE_PIPES <- ifelse(matched$DISCHARGE_PIPES>0&matched$DISCHARGE_PIPES<5,"med",matched$DISCHARGE_PIPES)

#convert zigh and med to 2 and 1
matched$WATER_CLARITY <- ifelse(matched$WATER_CLARITY == "zigh","2",matched$WATER_CLARITY)
matched$WATER_CLARITY <- ifelse(matched$WATER_CLARITY == "med","1",matched$WATER_CLARITY)
matched$SUSP_PHYTOPLANKTON <- ifelse(matched$SUSP_PHYTOPLANKTON == "zigh","2",matched$SUSP_PHYTOPLANKTON)
matched$SUSP_PHYTOPLANKTON <- ifelse(matched$SUSP_PHYTOPLANKTON == "med","1",matched$SUSP_PHYTOPLANKTON)
matched$PERIPHYTON <- ifelse(matched$PERIPHYTON == "zigh","2",matched$PERIPHYTON)
matched$PERIPHYTON <- ifelse(matched$PERIPHYTON == "med","1",matched$PERIPHYTON)
matched$ODOR <- ifelse(matched$ODOR == "zigh","2",matched$ODOR)
matched$ODOR <- ifelse(matched$ODOR == "med","1",matched$ODOR)
matched$TRASH <- ifelse(matched$TRASH == "zigh","2",matched$TRASH)
matched$TRASH <- ifelse(matched$TRASH == "med","1",matched$TRASH)
matched$DISCHARGE_PIPES <- ifelse(matched$DISCHARGE_PIPES == "zigh","2",matched$DISCHARGE_PIPES)
matched$DISCHARGE_PIPES <- ifelse(matched$DISCHARGE_PIPES == "med","1",matched$DISCHARGE_PIPES)



#######################################################################################################################################
#calculate a total score
#######################################################################################################################################

#convert scores to integers
#convert param values to numeric
matched$WATER_CLARITY <- as.numeric(matched$WATER_CLARITY)
matched$SUSP_PHYTOPLANKTON <- as.numeric(matched$SUSP_PHYTOPLANKTON)
matched$PERIPHYTON <- as.numeric(matched$PERIPHYTON)
matched$ODOR <- as.numeric(matched$ODOR)
matched$TRASH <- as.numeric(matched$TRASH)
matched$DISCHARGE_PIPES <- as.numeric(matched$DISCHARGE_PIPES)

#total score
matched$score <- matched$WATER_CLARITY+matched$SUSP_PHYTOPLANKTON+matched$PERIPHYTON+matched$ODOR+matched$TRASH+matched$DISCHARGE_PIPES

#plot it
par(mfrow=c(2,1))
hist(slight$score)
hist(impaired$score,col = "gray")



#######################################################################################################################################
#Create frequency table
#######################################################################################################################################

#count occurences for frequency prob calculations and rename freq to name.c
library(plyr)
WATER_CLARITY <- count(matched,c("impact","WATER_CLARITY"))
names(WATER_CLARITY)[names(WATER_CLARITY)=="freq"]<-"WATER_CLARITY.c"
SUSP_PHYTOPLANKTON <- count(matched,c("impact","SUSP_PHYTOPLANKTON"))
names(SUSP_PHYTOPLANKTON)[names(SUSP_PHYTOPLANKTON)=="freq"]<-"SUSP_PHYTOPLANKTON.c"
PERIPHYTON <- count(matched,c("impact","PERIPHYTON"))
names(PERIPHYTON)[names(PERIPHYTON)=="freq"]<-"PERIPHYTON.c"
ODOR <- count(matched,c("impact","ODOR"))
names(ODOR)[names(ODOR)=="freq"]<-"ODOR.c"
TRASH <- count(matched,c("impact","TRASH"))
names(TRASH)[names(TRASH)=="freq"]<-"TRASH.c"
DISCHARGE_PIPES <- count(matched,c("impact","DISCHARGE_PIPES"))
names(DISCHARGE_PIPES)[names(DISCHARGE_PIPES)=="freq"]<-"DISCHARGE_PIPES.c"

#create pivot tables
library(reshape)
WATER_CLARITY <- cast(WATER_CLARITY,impact~WATER_CLARITY)
SUSP_PHYTOPLANKTON <- cast(SUSP_PHYTOPLANKTON,impact~SUSP_PHYTOPLANKTON)
PERIPHYTON <- cast(PERIPHYTON,impact~PERIPHYTON)
ODOR <- cast(ODOR,impact~ODOR)
TRASH <- cast(TRASH,impact~TRASH)
DISCHARGE_PIPES <- cast(DISCHARGE_PIPES,impact~DISCHARGE_PIPES)

#rename 0, 1, zigh colums to ODOR.2, ODOR.1 and ODOR.0
names(WATER_CLARITY)[names(WATER_CLARITY)=="0"]<-"WATER_CLARITY.0"
names(WATER_CLARITY)[names(WATER_CLARITY)=="med"]<-"WATER_CLARITY.1"
names(WATER_CLARITY)[names(WATER_CLARITY)=="zigh"]<-"WATER_CLARITY.2"
names(SUSP_PHYTOPLANKTON)[names(SUSP_PHYTOPLANKTON)=="0"]<-"SUSP_PHYTOPLANKTON.0"
names(SUSP_PHYTOPLANKTON)[names(SUSP_PHYTOPLANKTON)=="med"]<-"SUSP_PHYTOPLANKTON.1"
names(SUSP_PHYTOPLANKTON)[names(SUSP_PHYTOPLANKTON)=="zigh"]<-"SUSP_PHYTOPLANKTON.2"
names(PERIPHYTON)[names(PERIPHYTON)=="0"]<-"PERIPHYTON.0"
names(PERIPHYTON)[names(PERIPHYTON)=="med"]<-"PERIPHYTON.1"
names(PERIPHYTON)[names(PERIPHYTON)=="zigh"]<-"PERIPHYTON.2"
names(ODOR)[names(ODOR)=="0"]<-"ODOR.0"
names(ODOR)[names(ODOR)=="med"]<-"ODOR.1"
names(ODOR)[names(ODOR)=="zigh"]<-"ODOR.2"
names(TRASH)[names(TRASH)=="0"]<-"TRASH.0"
names(TRASH)[names(TRASH)=="med"]<-"TRASH.1"
names(TRASH)[names(TRASH)=="zigh"]<-"TRASH.2"
names(DISCHARGE_PIPES)[names(DISCHARGE_PIPES)=="0"]<-"DISCHARGE_PIPES.0"
names(DISCHARGE_PIPES)[names(DISCHARGE_PIPES)=="med"]<-"DISCHARGE_PIPES.1"
names(DISCHARGE_PIPES)[names(DISCHARGE_PIPES)=="zigh"]<-"DISCHARGE_PIPES.2"

#merge into one table
frequencies <- merge(WATER_CLARITY,SUSP_PHYTOPLANKTON,all=TRUE)
frequencies <- merge(frequencies,PERIPHYTON,all=TRUE)
frequencies <- merge(frequencies,ODOR,all=TRUE)
frequencies <- merge(frequencies,TRASH,all=TRUE)
frequencies <- merge(frequencies,DISCHARGE_PIPES,all=TRUE)

#delete individual param tables
rm(DISCHARGE_PIPES)
rm(ODOR)
rm(PERIPHYTON)
rm(SUSP_PHYTOPLANKTON)
rm(TRASH)
rm(WATER_CLARITY)

#add a total column
impact <- count(matched,c("impact"))
names(impact)[names(impact)=="freq"]<-"Total"
frequencies <- merge(impact,frequencies,all=TRUE)
rm(impact)

#convert to probabilities
frequencies$WATER_CLARITY.0 <- (frequencies$WATER_CLARITY.0/frequencies$Total)
frequencies$WATER_CLARITY.1 <- (frequencies$WATER_CLARITY.1/frequencies$Total)
frequencies$WATER_CLARITY.2 <- (frequencies$WATER_CLARITY.2/frequencies$Total)
frequencies$SUSP_PHYTOPLANKTON.0 <- (frequencies$SUSP_PHYTOPLANKTON.0/frequencies$Total)
frequencies$SUSP_PHYTOPLANKTON.1 <- (frequencies$SUSP_PHYTOPLANKTON.1/frequencies$Total)
frequencies$SUSP_PHYTOPLANKTON.2 <- (frequencies$SUSP_PHYTOPLANKTON.2/frequencies$Total)
frequencies$PERIPHYTON.0 <- (frequencies$PERIPHYTON.0/frequencies$Total)
frequencies$PERIPHYTON.1 <- (frequencies$PERIPHYTON.1/frequencies$Total)
frequencies$PERIPHYTON.2 <- (frequencies$PERIPHYTON.2/frequencies$Total)
frequencies$ODOR.0 <- (frequencies$ODOR.0/frequencies$Total)
frequencies$ODOR.1 <- (frequencies$ODOR.1/frequencies$Total)
frequencies$ODOR.2 <- (frequencies$ODOR.2/frequencies$Total)
frequencies$TRASH.0 <- (frequencies$TRASH.0/frequencies$Total)
frequencies$TRASH.1 <- (frequencies$TRASH.1/frequencies$Total)
frequencies$TRASH.2 <- (frequencies$TRASH.2/frequencies$Total)
frequencies$DISCHARGE_PIPES.0 <- (frequencies$DISCHARGE_PIPES.0/frequencies$Total)
frequencies$DISCHARGE_PIPES.1 <- (frequencies$DISCHARGE_PIPES.1/frequencies$Total)
frequencies$DISCHARGE_PIPES.2 <- (frequencies$DISCHARGE_PIPES.2/frequencies$Total)



#tidy up. This removes everything stored in short term memory
rm(list=ls())