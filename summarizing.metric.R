#title:"Summarizing.Metric"
#author: Alene Onion
#date: October 2018

#set libraries
#library for the count function
library(plyr)
#for running rmarkdown
library(rmarkdown)
#for creating tables
library(knitr)
library(kableExtra)
library(formattable)
#for restructuring tables
library(reshape)

#run the rmarkdown script for this list
render("summarizing.metric.Rmd")

