#adding a column to the SBU data indicating whether the User Perception data indicate the stream is "clean" or "dirty"
#Alene Onion
#December 2017


######################################################################################################################
#First fun the first 65 lines of 30.creating...
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

######################################################################################################################
#Next run the first 20 lines of 11.
#set libraries
#library for the count function
library(plyr)

# set the working directory 
setwd("C:/Rscripts/WAVE")

#read the SBU data - all kick samples since 1990
SBU = read.csv("all_samplesL.csv")


######################################################################################################################
#Creating low scoring Periphyton, Odor, and Turbidity subset of SBU to run through baysean statistics
######################################################################################################################

junk <- matched$PERIPHYTON < 4
clean <- matched[junk,]
junk <- matched$WATER_CLARITY <1
clean <- clean[junk,]
junk <- matched$ODOR <1
clean <- clean[junk,]
rm(junk)
clean$sample_id <- do.call(paste,c(clean[c("LATITUDE","LONGITUDE")],sep="_"))
keep <- c("sample_id","coll_date")
clean <- clean[keep]
rm(keep)
clean <- merge(SBU,clean,by=c("sample_id","coll_date"))
clean$user <- "clean"


#Creating high scoring Periphyton, Odor, ******OR****** Turbidity subset of SBU to run through baysean stats
junk <- matched$PERIPHYTON >3
peri <- matched[junk,]
junk <- matched$WATER_CLARITY >0
turb <- matched[junk,]
junk <- matched$ODOR >0
odor <- matched[junk,]
rm(junk)
dirty <- merge(peri,turb,all=TRUE)
dirty <- merge(dirty,odor,all=TRUE)
rm(odor)
rm(peri)
rm(turb)
dirty$sample_id <- do.call(paste,c(dirty[c("LATITUDE","LONGITUDE")],sep="_"))
keep <- c("sample_id","coll_date")
dirty <- dirty[keep]
rm(keep)
dirty <- merge(SBU,dirty,by=c("sample_id","coll_date"))
dirty$user <- "dirty"

#merge the two to make one SBU file
SBU <- merge(clean,dirty,all=TRUE)
rm(clean)
rm(dirty)

#clean up
rm(matched)

#plot the data for visualization
dirty <- SBU[SBU$user=="dirty",]
clean <- SBU[SBU$user=="clean",]
par(mfrow=c(2,1))
hist(clean$bap)
hist(dirty$bap)

#sort into impact categories
#create an impact column
dirty$impact <- dirty$bap
dirty$impact <- ifelse(dirty$bap>=7.5,"1.high",dirty$impact)
dirty$impact <- ifelse(dirty$bap>=5&dirty$bap<7.5,"2.slight",dirty$impact)
dirty$impact <- ifelse(dirty$bap<5,"3.low",dirty$impact)

#sort into impact categories
#create an impact column
clean$impact <- clean$bap
clean$impact <- ifelse(clean$bap>=7.5,"1.high",clean$impact)
clean$impact <- ifelse(clean$bap>=5&clean$bap<7.5,"2.slight",clean$impact)
clean$impact <- ifelse(clean$bap<5,"3.low",clean$impact)


#NOW RUN THE 21.Calculating Frequencies script
