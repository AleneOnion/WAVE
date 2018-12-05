# Program:  concatenating watershed outputs from Stream Stats
# Programmer: Alene Onion 
# Date: 11/27/2017

# set the working directory
setwd("C:/Rscripts/WAVE/watershed.info")

#pull the files - names and number within this folder
file_list <- list.files()
nfile_list <- length(file_list)

#read the first table
shed <- read.csv(file_list[1])

#read subsequent files
for(i in 2:nfile_list){
  temp <- read.csv(file_list[i])
  shed <- merge(shed,temp,all=TRUE)
  rm(temp)
}

#pull just the fields I'm interested in
keep <- c("Name","EL1200","STORAGE","FOREST","DRNAREA")
shed2 <- shed[keep]

#remove unique records
shed2 <- unique(shed2[c("Name","EL1200","STORAGE","FOREST","DRNAREA")])

#removing NAs in drainage
shed2 <- shed2[!is.na(shed2$DRNAREA),]

#tidy up. This removes everything stored in short term memory
rm(list=ls())