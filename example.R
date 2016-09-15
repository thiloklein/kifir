## -------------------------------------------------------------------
## --- R script to obtain all feasible matchings in the 2015 KIFIR --- 
## -------------------------------------------------------------------


## ------------------------
## --- 1. Load datasets ---

## --- set your working directory where data is located
rm(list=ls())
setwd("~/Documents/Research/Matching/kifir/")

## --- 1-a. read 2015 NABC (10th grade) to restrict analysis to 4 grade grammar schools.
nabc2015_10 <- read.csv("10_evfolyam_telephelyi_adatok.dat", 
                        dec = ",", sep="\t", fileEncoding="iso-8859-1", stringsAsFactors=FALSE)
nabc2015_10 <- nabc2015_10[nabc2015_10$tipus == 4,]
nabc2015_10 <- nabc2015_10[!duplicated(nabc2015_10$OMid),]
nabc2015_10 <- nabc2015_10[, which(names(nabc2015_10) %in% c("OMid","jaras_kod","tipus"))]

# variables used::
#tipus     : grammar school with 4 grades
#OMid      : school ID
#jaras_kod : district ID


## --- 1-b. read 2015 NABC (8th grade) to restrict analysis to primary school students 
##          and define school markets (jaras_kod).
nabc2015_8 <- read.csv("8_evfolyam_tanuloi_adatok.dat", 
                       dec = ",", sep="\t", fileEncoding="iso-8859-1", stringsAsFactors=FALSE)
names(nabc2015_8)[names(nabc2015_8) == "ï..azon"] <- "azon"
nabc2015_8 <- nabc2015_8[, which(names(nabc2015_8) %in% c("azon","tipus"))]

# variables used::
#tipus     : primary school
#azon      : student ID


## --- 1-c. read 2015 KIFIR.
kifir2015 <- read.csv("kifir2015.dat", dec = ",", sep="\t", 
                      fileEncoding="iso-8859-1", stringsAsFactors=FALSE)
names(kifir2015)[names(kifir2015) == "ï..azon"] <- "azon"
kifir2015 <- kifir2015[kifir2015$DIAKSTATUSZ == "F",
                       which(names(kifir2015) %in% 
                               c("azon","TAG_ID","ISK_OMKOD","JEL_SORSZ","DIAKSORSZTAG","FELVETTEK"))]

# variables used::
#DIAKSTATUSZ == "F" : only use student preferences if college finds student acceptable
#azon               : student id (s.id)
#TAG_ID             : college id (c.id)
#ISK_OMKOD          : secondary school identifier (equivalent to OMid in 10th grade NABC?)
#JEL_SORSZ          : student's preference list of school sides (s.prefs)
#DIAKSORSZTAG       : school's ranking position of student (c.prefs)
#FELVETTEK          : result (proposed for inclusion). FELVETTEK = ifelse(EREMENY==3,1,0)
#                     where EREMENY = 0: the school rejected the application of the student
#                                     1: headcount is full
#                                     2: the student has already been assigned to another school
#                                     3: student is assigned to the school


## -------------------------
## --- 2. Merge datasets ---

## --- 2-a. restrict KIFIR to the market for 'grammar schools with 4 grades'
#install.packages("dplyr")
library(dplyr)
kifir2015 <- left_join(x = kifir2015, y = nabc2015_10, by = c("ISK_OMKOD" = "OMid"))
kifir2015 <- kifir2015[!is.na(kifir2015$tipus),] 
kifir2015$tipus <- NULL

## --- 2-b. restrict KIFIR to current 'primary school' students
kifir2015 <- left_join(x = kifir2015, y = nabc2015_8, by = "azon")
kifir2015 <- kifir2015[!is.na(kifir2015$tipus) & kifir2015$tipus == 1,] 
kifir2015$tipus <- NULL

## --- 2-c. use KIFIR for school districts 1 and 2 only
kifir2015 <- kifir2015[kifir2015$jaras_kod %in% c(1,2),]


## -------------------------------------------
## --- 3. Obtain feasible stable matchings ---

## --- 3-a. split data by district ID and drop schools not in these markets 
##          (and all students admitted to them)

kifir2015 <- split(kifir2015, kifir2015$jaras_kod)

## for each market (jaras_kod): drop students (azon) not matched to any school site (TAG_ID)
kifir2015 <- lapply(kifir2015, function(z){
  ## students matched to at least one of the school sites
  student.ids <- with(z, unique(azon[FELVETTEK == 1]))
  z[z$azon %in% student.ids,]
})

## for each market (jaras_kod): drop school sites (TAG_ID) not matched to any student (azon)
kifir2015 <- lapply(kifir2015, function(z){
  ## school sites matched to at least one of the students
  college.ids <- with(z, unique(TAG_ID[FELVETTEK == 1]))
  z[z$TAG_ID %in% college.ids,]
})


## --- 3-b. add s.id and c.id based on 'azon' and 'TAG_ID'

kifir2015 <- lapply(kifir2015, function(z){
  z$s.id <- as.integer(as.factor(z$azon))
  z$c.id <- as.integer(as.factor(z$TAG_ID))
  z
})


## --- 3-c. create preference matrices (s.prefs, c.prefs) based on 'JEL_SORSZ' and 'DIAKSORSZTAG'

## sort data by s.id and JEL_SORSZ
kifir2015 <- lapply(kifir2015, function(z){
  with(z, z[order(s.id,JEL_SORSZ),])
})
## obtain student preference ranking over colleges (s.prefs) in matrix format
s.prefs <- lapply(kifir2015, function(d){
  s.prefs <- with(d, split(c.id, s.id))
  nas <- max(unlist(lapply(s.prefs, function(z) length(unique(z)) )))
  s.prefs <- lapply(s.prefs, function(z){
    z <- unique(z)
    c(z, rep(NA,nas-length(z)))
  })
  do.call(cbind,s.prefs)
})

## sort data by s.id and JEL_SORSZ
kifir2015 <- lapply(kifir2015, function(z){
  with(z, z[order(c.id,DIAKSORSZTAG),])
})
## obtain college preference ranking over students (c.prefs) in matrix format
c.prefs <- lapply(kifir2015, function(d){
  c.prefs <- with(d, split(s.id, c.id))
  nas <- max(unlist(lapply(c.prefs, function(z) length(unique(z)) )))
  c.prefs <- lapply(c.prefs, function(z){
    z <- unique(z)
    c(z, rep(NA,nas-length(z)))
  })
  do.call(cbind,c.prefs)
})

## obtain number of places at each college
nSlots <- lapply(kifir2015, function(d){
  aggregate(FELVETTEK ~ c.id, data=d, sum)$FELVETTEK
})


## --- 3-d. obtain feasible stable matchings as edge list with s.id and c.id

# install.packages("http://klein.uk/R/matchingMarkets_0.3-2.tar.gz", repos=NULL, type="source")
library(matchingMarkets)

res <- list()
for(i in 1:length(nSlots)){
  res[[i]] <- hri(s.prefs=s.prefs[[i]], c.prefs=c.prefs[[i]], nSlots=nSlots[[i]])$matchings

  ## add to edge list: jaras_kod, OMid, azon
  
  res[[i]]$jaras_kod <- kifir2015[[i]]$jaras_kod[1]
  res[[i]]$OMid      <- kifir2015[[i]]$ISK_OMKOD[ match(res[[i]]$college, kifir2015[[i]]$c.id) ]
  res[[i]]$TAG_ID    <- kifir2015[[i]]$TAG_ID[ match(res[[i]]$college, kifir2015[[i]]$c.id) ]
  res[[i]]$azon      <- kifir2015[[i]]$azon[ match(res[[i]]$student, kifir2015[[i]]$s.id) ]
  
  res[[i]] <- with(res[[i]], res[[i]][order(matching,college,slots),])
  
  res[[i]]$ids       <- with(res[[i]], paste(college,student,sep="_"))
  kifir2015[[i]]$ids <- with(kifir2015[[i]], paste(c.id,s.id,sep="_"))
  res[[i]]$ok        <- kifir2015[[i]]$FELVETTEK[ match(res[[i]]$ids, kifir2015[[i]]$ids) ]
  res[[i]]$ok        <- with(res[[i]], ifelse(sOptimal==1, ifelse(ok==0, 0, 1), 1))
  kifir2015[[i]]$ids <- NULL; res[[i]]$ids <- NULL
  
  res[[i]]$sOptimal <- NULL
  res[[i]]$cOptimal <- NULL
  res[[i]]$slots    <- NULL
  res[[i]]$sRank    <- NULL
  res[[i]]$cRank    <- NULL  
  res[[i]]$student  <- NULL
  res[[i]]$college  <- NULL
}
res


## ------------------------------------
## --- 4. Checks and return results ---

getwd()
write.table(do.call("rbind", res), file="res.dat", sep="\t", quote=FALSE, 
            fileEncoding="iso-8859-1", row.names=FALSE)


