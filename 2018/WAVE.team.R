# Program:  Producing WAVE team map
# Purpose: reading WAVE tables and producing WAVE team map
# Programmer: Alene Onion 
# Date: March 2018


#tidy up. This removes everything stored in short term memory
rm(list=ls())

# set the working directory
setwd("C:/Rscripts/WAVE/2017")

#Read data tables
Visual<-read.csv("Visual.final.csv")
Visual<-unique(Visual[c("Latitude","Longitude","date","participants.names")])

# set the working directory
setwd("C:/Rscripts/WAVE/2018")

#read the registration file
registration <- read.csv("2018 Registration - Form Responses.csv")
#rename some columns
names(registration)[names(registration)=="team.name"]<-"Team"
names(registration)[names(registration)=="X5..Please.provide.your.contact.information."]<-"First.Name"
names(registration)[names(registration)=="Address..just.write...your.city...NY."]<-"Address"
#restrict the file to only those with a team name
registration<-registration[registration$Team!="",]
registration$name<-do.call(paste,c(registration[c("First.Name","Last.Name")],sep=" "))
#remove commas and numbers from the address
registration$Address<-gsub(",","",registration$Address)
registration$Address<-gsub(" \\d","",registration$Address)
registration$Address<-gsub("\\d","",registration$Address)
#restrict to unique values only
registration<-unique(registration[c("name","Address")])

#cap names so they are equivalent
registration$Address<-toupper(registration$Address)
registration$name<-toupper(registration$name)
Visual$participants.names<-toupper(Visual$participants.names)

#create a column with the number of samples
peeps <-unique(registration$name)
npeeps <- length(peeps)

for(i in 1:npeeps){
  look<-paste(peeps[i],".+",sep="")
  Visual$participants.names<-gsub(look,peeps[i],Visual$participants.names)
  look<-paste(".+",peeps[i],sep="")
  Visual$participants.names<-gsub(look,peeps[i],Visual$participants.names)
}
rm(list=c('i','look'))

#now remove all other records that don't include a coordinator
#first convert peeps to a data frame
peeps<-as.data.frame(peeps)
names(peeps)[names(peeps)=="peeps"]<-"participants.names"
#now merge and exclude those that don't overlap
Visual<-merge(Visual,peeps,by=c("participants.names"),all=FALSE)

#convert date to date
Visual$date<-format(as.Date(Visual$date,"%m/%d/%Y"),"%Y")

#remove lat/lon and pull unique values
Visual<-unique(Visual[c("participants.names","date")])

#sort by name/date decreasing
Visual<-Visual[order(Visual$participants.names,Visual$date,na.last=TRUE,decreasing = TRUE),]

#collapse to a list of names with sampling dates listed after each
peeps<-unique(Visual$participants.names)
npeeps<-length(peeps)
#for the first participant
#restrict to one participant
name<-peeps[1]
temp<-Visual[Visual$participants.names==name,]
dates<-unique(temp$date)
dates<- paste(dates,collapse=";")
sampling = data.frame(name,dates)
rm(list=c('temp','dates','name'))

#for subsequent participant
for(i in 2:npeeps){
  name<-peeps[i]
  temp<-Visual[Visual$participants.names==name,]
  dates<-unique(temp$date)
  dates<- paste(dates,collapse=";")
  tMap = data.frame(name,dates)
  sampling<-merge(sampling,tMap,all=TRUE)
  rm(list=c('temp','tMap','dates'))
}
rm(list=c('i','npeeps','peeps','Visual','name'))

########################################################
#Adding teams manually
########################################################
name="LEE WILLBANKS"
dates="NA"
tMap = data.frame(name,dates)
sampling<-merge(sampling,tMap,all=TRUE)
rm(list=c('tMap','name','dates'))

#merge map and registration files
sampling<-merge(sampling,registration,by=c("name"),all=FALSE)
rm(registration)

#Convert the names to team names (sometimes different than personal names)
#read the registration file again
registration <- read.csv("2018 Registration - Form Responses.csv")
#rename some columns
names(registration)[names(registration)=="team.name"]<-"Teamn"
names(registration)[names(registration)=="X5..Please.provide.your.contact.information."]<-"First.Name"
#restrict the file to only those with a team name
registration<-registration[registration$Teamn!="",]
registration$name<-do.call(paste,c(registration[c("First.Name","Last.Name")],sep=" "))
registration<-unique(registration[c("name","Teamn")])
registration$name<-toupper(registration$name)
registration$Teamn<-toupper(registration$Teamn)
#merge the two files
sampling<-merge(sampling,registration,by=c("name"),all=FALSE)
rm(registration)

#meld name and dates
sampling$Team<-paste(sampling$Teamn,"(",sampling$dates,")",sep="")
sampling<-unique(sampling[c("Team","Address")])

#collapse to a list of towns with participants listed after each
towns<-unique(sampling$Address)
ntowns<-length(towns)
#Fort the first
Town<-towns[1]
temp<-sampling[sampling$Address==Town,]
Teams<-unique(temp$Team)
Teams<-paste(Teams,collapse="\n")
Map = data.frame(Town,Teams)
rm(list=c('temp','Teams','Town'))

#now for the rest
for(i in 2:ntowns){
  Town<-towns[i]
  temp<-sampling[sampling$Address==Town,]
  Teams<-unique(temp$Team)
  Teams<-paste(Teams,collapse="\n")
  tMap = data.frame(Town,Teams)
  Map<-merge(Map,tMap,all=TRUE)
  rm(list=c('temp','Teams','Town','tMap'))
}
rm(list=c('i','ntowns','towns','sampling'))

########################################################
#MAKE SURE TO CHECK THOSE NAMES THAT ARE MANUALLY ADDED
########################################################


#write.table
write.table(Map,file="WAVE.Team.map.csv",sep=",",row.names=FALSE)
