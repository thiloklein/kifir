## ---------------------------------------------------------
## --- R script to simulate data from 2 matching markets ---
## ---------------------------------------------------------


## set working directory
getwd()
setwd("~/Documents/Research/Matching/kifir/")
rm(list=ls())

## get matchings from two simulated matching markets
library(matchingMarkets)

x <- list()
x[[1]] <- hri(nStudents=10, nSlots=c(3,3,3), seed=54, s.range=c(2,3))
x[[2]] <- hri(nStudents=7, nSlots=c(3,3,3), seed=100, s.range=c(1,3))

matrix2edgelist <- function(x, i){

  ## number of students and colleges
  nColleges <- ncol(x$c.prefs.hri)
  nStudents <- ncol(x$s.prefs.hri)
  
  ## college and student ids
  y <- data.frame(c.id = rep(1:nColleges, nStudents), 
                  s.id = c(sapply(1:nStudents, function(z) rep(z, nColleges))))
  
  ## student rank over colleges
  y$s.prefs <- c(sapply( 1:nStudents, function(z) match(1:nColleges, x$s.prefs.hri[,z]) ))
  
  ## college rank over student
  h <- sapply( 1:nColleges, function(z) match(1:nStudents, x$c.prefs.hri[,z]) )
  y$c.prefs <- c(sapply( 1:nStudents, function(z) h[z,]))
  
  ## add s-optimal matching indicator
  y$id <- paste(y$c.id, y$s.id, sep="_")
  x$matchings$id <- paste(x$matchings$college, x$matchings$student, sep="_")
  y$sOptimal <- ifelse(y$id %in% x$matchings$id[x$matchings$sOptimal == 1], 1, 0)
  y$id <- NULL
  
  #y <- na.omit(y)
  y$m.id <- i
  rownames(y) <- NULL 
  return(y)
}

y <- list()
for(i in 1:length(x)){
  y[[i]] <- matrix2edgelist(x=x[[i]], i=i)
}
y <- do.call(rbind, y)


## make into orginal variables
kifir2015 <- with(y, data.frame(azon = 10080602957 + nrow(y)*m.id + s.id,
                                TAG_ID = 2378 + nrow(y)*m.id + c.id,
                                ISK_OMKOD = ifelse(m.id==1, 
                                                   ifelse(c.id==3, "027446", "028001"),
                                                   ifelse(c.id==3, "027553", "029553")),
                                JEL_SORSZ = s.prefs,
                                DIAKSORSZTAG = c.prefs,
                                DIAKSTATUSZ = ifelse(is.na(c.prefs), "E", "F"),
                                FELVETTEK = sOptimal,
                                jaras_kod = m.id, 
                                #megye_kod = m.id, 
                                #regio_kod = m.id, 
                                stringsAsFactors=FALSE
))

## write resulting files

## 2015 KIFIR
write.table(kifir2015[,!names(kifir2015) %in% c("jaras_kod","megye_kod","regio_kod")], file="input/kifir2015.dat", 
            sep="\t", quote=FALSE, fileEncoding="iso-8859-1", row.names=FALSE)

## 2015 NABC, 10th grade
nabc2015_10 <- kifir2015[kifir2015$FELVETTEK==1,]
nabc2015_10 <- with(nabc2015_10, data.frame(OMid=ISK_OMKOD,
                                            telephely=as.integer(as.factor(TAG_ID)),
                                            jaras_kod=jaras_kod,
                                            #megye_kod=megye_kod,
                                            #regio_kod=regio_kod,
                                            tipus=4, stringsAsFactors=FALSE))
write.table(nabc2015_10, file="input/10_evfolyam_telephelyi_adatok.dat", sep="\t", quote=FALSE, 
          fileEncoding="iso-8859-1", row.names=FALSE)

## 2015 NABC, 8th grade
nabc2015_8 <- kifir2015[kifir2015$FELVETTEK==1,]
nabc2015_8 <- with(nabc2015_8, data.frame(azon=azon,  
                                          tipus=1, stringsAsFactors=FALSE))
write.table(nabc2015_8, file="input/8_evfolyam_tanuloi_adatok.dat", sep="\t", quote=FALSE, 
            fileEncoding="iso-8859-1", row.names=FALSE)

## TAG to OMid-telephely correspondence table for secondary schools
TAG2015 <- with(kifir2015, data.frame(OMid=ISK_OMKOD,
                                      telephely=as.integer(as.factor(TAG_ID)),
                                      tipus=4,
                                      TAG_ID=TAG_ID))
write.table(TAG2015[!duplicated(TAG2015),], file="input/TAG2015.dat", sep="\t", quote=FALSE, 
            fileEncoding="iso-8859-1", row.names=FALSE)

## 2014 KIFIR
kifir2014 <- kifir2015[, which(names(kifir2015) %in% c("TAG_ID","ISK_OMKOD","FELVETTEK"))]
set.seed(1)
h <- floor(runif(length(unique(kifir2014$TAG_ID)), min=3, max=6))
kifir2014$tgl_letszam <- h[as.integer(as.factor(kifir2014$TAG_ID))]
write.table(kifir2014, file="input/kifir2014.dat", sep="\t", quote=FALSE, 
            fileEncoding="iso-8859-1", row.names=FALSE)



