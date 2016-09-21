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
                               c("TAG_ID","ISK_OMKOD","FELVETTEK","tgl_letszam"))]
str(kifir2014)
showNAs(kifir2014)

# variables used::
#TAG_ID      : school site id
#FELVETTEK   : result (proposed for inclusion). FELVETTEK = ifelse(EREMENY==3,1,0)
#tgl_letszam : capacity quota


## --- 2. acceptance rates = school capacity quotas / applicants

## number of applicants, admissions and the school quota

applicants <- aggregate(kifir2014$ISK_OMKOD, list(kifir2014$ISK_OMKOD), FUN = length)
names(applicants) <- c("ISK_OMKOD","applicants")

admissions <- aggregate(kifir2014$FELVETTEK, list(kifir2014$ISK_OMKOD), FUN = sum)
names(admissions) <- c("ISK_OMKOD","admissions")

h          <- with(kifir2014, aggregate(tgl_letszam, list(TAG_ID, ISK_OMKOD), FUN=mean))
names(h)   <- c("TAG_ID","ISK_OMKOD","tgl_letszam")
quota      <- aggregate(h$tgl_letszam, list(h$ISK_OMKOD), FUN = sum)
names(quota) <- c("ISK_OMKOD","quota")

## combine into data.frame
acceptance_rate <- merge(x=admissions, y=applicants, by="ISK_OMKOD")
acceptance_rate <- merge(x=acceptance_rate, y=quota, by="ISK_OMKOD")

## add acceptance rates
acceptance_rate$admissions_per_applicant <- with(acceptance_rate, admissions/applicants)
acceptance_rate$quota_per_applicant      <- with(acceptance_rate, quota/applicants)

## identify school sites by postcode and street name?


## --- 4. Checks and return results

getwd()
write.table(acceptance_rate, file="output/acceptance_rate.dat", sep="\t", quote=FALSE, 
            fileEncoding="iso-8859-1", row.names=FALSE)



