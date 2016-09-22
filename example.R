## -------------------------------------------------------------------
## --- R script to obtain all feasible matchings in the 2015 KIFIR --- 
## -------------------------------------------------------------------


## ------------------------
## --- 1. Load datasets ---

## --- set your working directory where data is located
rm(list=ls())
setwd("~/Documents/Research/Matching/kifir/")
source("http://klein.uk/R/myfunctions.R")
m.id <- "jaras_kod"

## --- set parameters

max_size    <- 2000
min_overlap <- 0.01


## --- 1-a. read 2015 NABC (10th grade) and TAG2015 to restrict analysis to 4 grade grammar schools.
#install.packages("dplyr")
library(dplyr)
nabc2015_10 <- read.csv("input/10_evfolyam_telephelyi_adatok.dat", 
                        dec = ",", sep="\t", fileEncoding="iso-8859-1", stringsAsFactors=FALSE)
nabc2015_10 <- nabc2015_10[nabc2015_10$tipus %in% c(4,5),]
nabc2015_10$OMid_telephely <- with(nabc2015_10, paste(OMid, telephely, sep="_"))
nabc2015_10 <- nabc2015_10[!duplicated(nabc2015_10$OMid_telephely),]
nabc2015_10 <- nabc2015_10[, which(names(nabc2015_10) %in% c("OMid","telephely",m.id,"tipus","OMid_telephely"))]

str(nabc2015_10)
showNAs(nabc2015_10)

# variables used::
#tipus     : grammar school with 4 grades
#OMid      : school ID
#telephely : school site ID
#jaras_kod : district ID


## --- 1-b. read 2015 NABC (8th grade) to restrict analysis to primary school students 
##          and define school markets (jaras_kod).
nabc2015_8 <- read.csv("input/8_evfolyam_tanuloi_adatok.dat", 
                       dec = ",", sep="\t", fileEncoding="iso-8859-1", stringsAsFactors=FALSE)
names(nabc2015_8)[names(nabc2015_8) == "ï..azon"] <- "azon"
nabc2015_8 <- nabc2015_8[, which(names(nabc2015_8) %in% c("azon","tipus"))]

str(nabc2015_8)
showNAs(nabc2015_8)

# variables used::
#tipus     : primary school
#azon      : student ID


## --- 1-c. read 2015 KIFIR.
kifir2015 <- read.csv("input/kifir2015.dat", dec = ",", sep="\t", 
                      fileEncoding="iso-8859-1", stringsAsFactors=FALSE)
names(kifir2015)[names(kifir2015) == "ï..azon"] <- "azon"
kifir2015 <- kifir2015[kifir2015$DIAKSTATUSZ == "F",
                       which(names(kifir2015) %in% 
                               c("azon","TAG_ID","ISK_OMKOD","JEL_SORSZ","DIAKSORSZTAG","FELVETTEK"))]

str(kifir2015)
showNAs(kifir2015)

# variables used::
#DIAKSTATUSZ == "F" : only use student preferences if college finds student acceptable
#azon               : student id (s.id)
#TAG_ID             : course id (c.id)
#ISK_OMKOD          : secondary school identifier (equivalent to OMid in 10th grade NABC?)
#JEL_SORSZ          : student's preference list of school sides (s.prefs)
#DIAKSORSZTAG       : school's ranking position of student (c.prefs)
#FELVETTEK          : result (proposed for inclusion). FELVETTEK = ifelse(EREMENY==3,1,0)
#                     where EREMENY = 0: the school rejected the application of the student
#                                     1: headcount is full
#                                     2: the student has already been assigned to another school
#                                     3: student is assigned to the school


## --- 1-d. read TAG IDs for 2015.
TAG2015     <- read.csv("input/TAG2015.dat", dec = ",", sep="\t", 
                        fileEncoding="iso-8859-1", stringsAsFactors=FALSE)

# variables used::
#OMid      : school ID
#telephely : school site ID
#TAG_ID    : course ID


## -------------------------
## --- 2. Merge datasets ---

## --- 2-a. restrict KIFIR to the market for 4-grade programmes

## add OMid_telephely to KIFIR
kifir2015 <- left_join(x = kifir2015, y = TAG2015, by = "TAG_ID")
if( !isTRUE(all.equal(kifir2015$ISK_OMKOD, kifir2015$OMid)) ){
  head(kifir2015)
  print("ISK_OMKOD != OMid !")
}
kifir2015$OMid_telephely <- with(kifir2015, paste(ISK_OMKOD, telephely, sep="_"))
kifir2015$OMid <- NULL
kifir2015$telephely <- NULL

## merge kifir2015 and nabc2015_10 based on OMid_telephely
kifir2015 <- left_join(x = kifir2015, y = nabc2015_10, by = "OMid_telephely")
if( !isTRUE(all.equal(kifir2015$ISK_OMKOD, kifir2015$OMid)) ){
  head(kifir2015)
  print("ISK_OMKOD != OMid !")
}
kifir2015$OMid <- NULL

## drop NAs
kifir2015 <- kifir2015[!is.na(kifir2015$tipus),] 
kifir2015$tipus <- NULL

str(kifir2015)
showNAs(kifir2015)


## --- 2-b. restrict KIFIR to current 'primary school' students
kifir2015 <- left_join(x = kifir2015, y = nabc2015_8, by = "azon")
kifir2015 <- kifir2015[!is.na(kifir2015$tipus) & kifir2015$tipus == 1,] 
kifir2015$tipus <- NULL

str(kifir2015)
showNAs(kifir2015)


## --- 2-c. use KIFIR for school districts 1 to 5 only
#kifir2015 <- kifir2015[kifir2015[m.id] %in% 1:5,]


## -----------------------------------------
## --- 3. Define suitable school markets ---

kifir2015 <- split(kifir2015, kifir2015[m.id])
do.call(data.frame,lapply(kifir2015, nrow))

while(length(kifir2015)>0){
  
  ## --- Step 1: individual district stats 
  kifirStats <- lapply(seq_along(kifir2015), function(i){
    x <- list()
    x$applicant_ids <- unique(kifir2015[[i]]$azon)
    x$applicant_no  <- length(x$applicant_ids)
    x$admissions_no <- length(unique(kifir2015[[i]]$azon[kifir2015[[i]]$FELVETTEK==1]))
    x$index         <- i
    x
  })
  
  ## --- Step 2: joint district stats for all feasible district pairs
  for(i in 1:length(kifirStats)){    
    for(j in 1:i){
      
      ## applicants
      applicants_ij <- min(kifirStats[[i]][["applicant_no"]], kifirStats[[j]][["applicant_no"]])
      overlaps_ij   <- sum(kifirStats[[i]][["applicant_ids"]] %in% kifirStats[[j]][["applicant_ids"]])
      overlap_perc  <- overlaps_ij / applicants_ij
      
      ## admissions
      admissions_ij <- kifirStats[[i]][["admissions_no"]] + kifirStats[[j]][["admissions_no"]]
      
      ## edgelist
      if((i == j) & (i == 1)){
        kifirEdgelist <- data.frame(A = kifirStats[[i]][["index"]],
                                    B = kifirStats[[j]][["index"]],
                                    overlap = overlap_perc, 
                                    admissions = admissions_ij)
      } else{
        kifirEdgelist <- rbind(kifirEdgelist, 
                               data.frame(A = kifirStats[[i]][["index"]],
                                          B = kifirStats[[j]][["index"]],
                                          overlap = overlap_perc, 
                                          admissions = admissions_ij))
      }
    }
  }
  
  ## --- Step 3: merge the two district with largest overlap 
  kifirEdgelist <- kifirEdgelist[order(kifirEdgelist$overlap, decreasing=TRUE),]
  kifirEdgelist <- kifirEdgelist[kifirEdgelist$A != kifirEdgelist$B,]
  
  ## check if kifirEdgelist is non-empty
  if( nrow(kifirEdgelist)==0 ){ break }
  
  ## check whether to stop because of too large joint district size
  if( min(kifirEdgelist[,"admissions"]) > max_size ){ break 
  } else{
    
    ## drop district pairs with more than 3000 admitted students
    kifirEdgelist <- kifirEdgelist[kifirEdgelist$admissions < max_size,]
  }
  
  ## check whether to stop because of lack of joint student applications
  if(kifirEdgelist[1,"overlap"] < min_overlap){ break
  } else{
    
    ## print result
    print(paste("Merge districts: ", names(kifir2015)[ kifirEdgelist[1,"A"] ], 
                " and ", names(kifir2015)[ kifirEdgelist[1,"B"] ], ".", sep=""))
    
    ## merge the 2 top overlapping districts
    kifir2015[[ kifirEdgelist[1,"A"] ]] <- rbind( kifir2015[[ kifirEdgelist[1,"A"] ]], 
                                                  kifir2015[[ kifirEdgelist[1,"B"] ]] )
    ## rename the merged district
    names(kifir2015)[ kifirEdgelist[1,"A"] ] <- with(kifirEdgelist[1,], 
                                                     paste(names(kifir2015)[ kifirEdgelist[1,"A"] ], 
                                                           names(kifir2015)[ kifirEdgelist[1,"B"] ], 
                                                           sep="_"))
    ## drop the other district    
    kifir2015[[ names(kifir2015)[ kifirEdgelist[1,"B"] ] ]] <- NULL
  }
}


## -------------------------------------------
## --- 4. Obtain feasible stable matchings ---

## --- 4-a. split data by district ID and drop schools not in these markets 
##          (and all students admitted to them)

## for each market (m.id): drop students (azon) not matched to any school site (TAG_ID)
kifir2015 <- lapply(kifir2015, function(z){
  ## students matched to at least one of the school sites
  student.ids <- with(z, unique(azon[FELVETTEK == 1]))
  z[z$azon %in% student.ids,]
})
do.call(data.frame,lapply(kifir2015, nrow))

## for each market (m.id): drop school sites (TAG_ID) not matched to any student (azon)
kifir2015 <- lapply(kifir2015, function(z){
  ## school sites matched to at least one of the students
  college.ids <- with(z, unique(TAG_ID[FELVETTEK == 1]))
  z[z$TAG_ID %in% college.ids,]
})
do.call(data.frame,lapply(kifir2015, nrow))


## --- 4-b. add s.id and c.id based on 'azon' and 'TAG_ID'

kifir2015 <- lapply(kifir2015, function(z){
  z$s.id <- as.integer(as.factor(z$azon))
  z$c.id <- as.integer(as.factor(z$TAG_ID))
  z
})


## --- 4-c. create preference matrices (s.prefs, c.prefs) based on 'JEL_SORSZ' and 'DIAKSORSZTAG'

## drop markets with only one college
kifir2015 <- kifir2015[ unlist(lapply(kifir2015, function(z) length(unique(z$c.id)) )) > 2]

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
do.call(data.frame,lapply(s.prefs, dim))

## sort data by c.id and DIAKSORSZTAG
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
do.call(data.frame,lapply(c.prefs, dim))

## obtain number of places at each college
nSlots <- lapply(kifir2015, function(d){
  aggregate(FELVETTEK ~ c.id, data=d, sum)$FELVETTEK
})


## --- 4-d. obtain feasible stable matchings as edge list with s.id and c.id

# install.packages("http://klein.uk/R/matchingMarkets_0.3-2.tar.gz", repos=NULL, type="source")
library(matchingMarkets)

res <- list()
for(i in 1:length(nSlots)){  
  
  res[[i]] <- tryCatch({
    hri(s.prefs=s.prefs[[i]], c.prefs=c.prefs[[i]], nSlots=nSlots[[i]])$matchings
  }, error=function(cond){
    message(cond)
    return(NULL)
  })
  
  ## add to edge list: jaras_kod/megye_kod/regio_kod, OM_kod, azon
  
  res[[i]][m.id]     <- kifir2015[[i]][,m.id][ match(res[[i]]$college, kifir2015[[i]]$c.id) ]
  res[[i]]$OM_kod    <- kifir2015[[i]]$ISK_OMKOD[ match(res[[i]]$college, kifir2015[[i]]$c.id) ]
  res[[i]]$telephely <- kifir2015[[i]]$telephely[ match(res[[i]]$college, kifir2015[[i]]$c.id) ]
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
  
  if(i == 1){
    print(paste("Generating stable matchings for ", length(nSlots), " markets ...", sep=""))
  }
  print(paste("Market ", i, " of ", length(nSlots), " completed.", sep=""))
}
#res


## ------------------------------------
## --- 5. Checks and return results ---

getwd()
res <- res[!sapply(res, is.null)] 
write.table(do.call("rbind", res), file="output/res.dat", sep="\t", 
            quote=FALSE, fileEncoding="iso-8859-1", row.names=FALSE)





