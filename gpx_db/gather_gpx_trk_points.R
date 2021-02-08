#!/usr/bin/env Rscript

#### Gather and store gpx track points systematically.
## Will be processed by other scripts


####_ Set environment _####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()
if(!interactive())pdf(file=sub("\\.R$",".pdf",Script.Name))
sink(file=sub("\\.R$",".out",Script.Name,),split=TRUE)

library(sf)
library(dplyr)
library(data.table)
library(trip)
library(myRtools)


## You can turn warnings into errors with:
# options(warn=2)
options(warn=1)


gpx_repo       <- "~/GISdata/GPX/"
polar_repo     <- "~/Documents/Running/Polar/"
gc_repo        <- "~/TRAIN/GoldenCheetah/"
EPSG           <- 3857
trackpoints_fl <- paste0( "~/GISdata/Count_sl2_",EPSG,".Rds")


#### list GPX files ####
gpxlist   <- list.files(gpx_repo, ".gpx$",
                        recursive   = T,
                        full.names  = T,
                        ignore.case = T)

polarlist <- list.files(polar_repo, ".gpx$",
                        recursive   = T,
                        full.names  = T,
                        ignore.case = T)

gclist    <- list.files(gc_repo, ".gpx$",
                        recursive   = T,
                        full.names  = T,
                        ignore.case = T)

## combine list
gpxlist <- c(polarlist, gpxlist, gclist)
## exclude some files
# gpxlist <- gpxlist[grep("orig", basename(gpxlist), ignore.case = TRUE, invert = T)]


## load or start a new data table
if (file.exists(trackpoints_fl)) {
    ## load old data
    data <- readRDS(trackpoints_fl)
    ## remove missing files
    data <- data[ file.exists(file) ]
    ## get parsed files
    dblist <- unique( data[, c("file","F_mtime")] )
    ## get all files
    fllist <- data.frame(file = gpxlist,
                         F_mtime = file.mtime(gpxlist))
    ## files to do
    ddd <- anti_join( fllist, dblist )

    ## check for changed files
    ##FIXME  not tested
    data <- data[ ! file %in% ddd$file ]

    gpxlist <- ddd$file

} else {
    data <- data.table()
}


cnt   <- 0
total <- length(gpxlist)

for (af in gpxlist) {
    cnt <- cnt + 1
    if (!file.exists(af)) { next() }
    cat(paste(cnt,total,af,"\n"))

    ## get all points
    temp <- read_sf(af, layer = "track_points")
    ## This assumes that dates in file are correct.......
    temp <- temp[ order(temp$time, na.last = FALSE), ]
    if (nrow(temp)<2) { next() }

    ## keep initial coordinates
    latlon <- st_coordinates(temp$geometry)
    latlon <- data.table(latlon)
    names(latlon)[names(latlon)=="X"] <- "Xdeg"
    names(latlon)[names(latlon)=="Y"] <- "Ydeg"

    ## add distance between points in meters
    temp$dist <- c(0, trackDistance(st_coordinates(temp$geometry), longlat = TRUE)) * 1000

    ## add time between points
    temp$timediff <- 0
    for (i in 2:nrow(temp)) {
        temp$timediff[i] <- difftime( temp$time[i], temp$time[i-1] )
    }

    ## parse coordinates for process in meters
    temp   <- st_transform(temp, EPSG)
    trkcco <- st_coordinates(temp)
    temp   <- data.table(temp)
    temp$X <- unlist(trkcco[,1])
    temp$Y <- unlist(trkcco[,2])
    temp   <- cbind(temp, latlon)

    ## data to keep
    temp   <- temp[, .(time,X,Y,Xdeg,Ydeg,dist,timediff, file = af, F_mtime = file.mtime(af))]

    ## some files don't have tracks
    if (!nrow(temp)>0) { next() }
    data <- rbind(data,temp)

    ## partial write
    if (cnt %% 40 == 0) {
        write_RDS(data, trackpoints_fl)
    }
}
## final write
write_RDS(data, trackpoints_fl)



####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f mins\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
