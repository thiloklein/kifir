## -------------------------------------------------------------------------
## --- R script to obtain acceptance rates of school sites in 2014 KIFIR ---
## -------------------------------------------------------------------------


## --- set your working directory where data is located
rm(list=ls())
setwd("~/Documents/Research/Matching/kifir/")
source("http://klein.uk/R/myfunctions.R")


## --- 1. read 2014 KIFIR data

kifir2014 <- read.csv("input/kifir2014.dat", dec = ",", sep="\t", 
                      fileEncoding="iso-8859-1", stringsAsFactors=FALSE)
kifir2014 <- kifir2014[, which(names(kifir2014) %in% 
                               c("TAG_ID","FELVETTEK","tgl_letszam"))]

str(kifir2014)
showNAs(kifir2014)

# variables used::
#TAG_ID      : school site id
#FELVETTEK   : result (proposed for inclusion). FELVETTEK = ifelse(EREMENY==3,1,0)
#tgl_letszam : capacity quota


## --- 2. acceptance rates = school site capacity quotas / applicants

## number of applicants, admissions and the school site quota
TAG_IDs    <- aggregate(kifir2014$TAG_ID, list(kifir2014$TAG_ID), FUN = length)[,1]
applicants <- aggregate(kifir2014$TAG_ID, list(kifir2014$TAG_ID), FUN = length)[,2]
admissions <- aggregate(kifir2014$FELVETTEK, list(kifir2014$TAG_ID), FUN = sum)[,2]
quota      <- aggregate(kifir2014$tgl_letszam, list(kifir2014$TAG_ID), FUN = median)[,2]

## acceptance rate
acceptance_rate <- data.frame(TAG_ID = TAG_IDs, 
                              admissions_per_applicant = admissions/applicants, 
                              quota_per_applicant = quota/applicants, stringsAsFactors=FALSE)


## --- 4. Checks and return results

getwd()
write.table(acceptance_rate, file="output/acceptance_rate.dat", sep="\t", quote=FALSE, 
            fileEncoding="iso-8859-1", row.names=FALSE)



