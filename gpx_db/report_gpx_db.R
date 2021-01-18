#!/usr/bin/env Rscript

#### Analysis and bad data detection


####_ Set environment _####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()
if(!interactive())pdf(file=sub("\\.R$",".pdf",Script.Name),width = 14)
sink(file=sub("\\.R$",".out",Script.Name,),split=TRUE)


library(data.table)

EPSG            <- 3857
trackpoints_fl  <- paste0("~/GISdata/Count_sl2_",EPSG,".Rds")
baseoutput      <- "~/GISdata/"
resolution_spac <- 50       ## spacial resolution in meters
resolution_temp <- 5 * 60   ## temporal resolution in seconds




####  Prepare data  ############################################################

data            <- readRDS(trackpoints_fl)
data[, F_mtime:=NULL]

## remove fake dates
data[ time < "1971-01-01", time := NA ]

cat(paste( length(unique( data$file )), "total files parsed\n" ))
cat(paste( nrow( data ), "points parsed\n" ))

## create speed
data$kph <- (data$dist / 1000) / (data$timediff / 3600)




####  Clean problematic data  ##################################################

if ( nrow(data[ is.na(X) |
                is.na(Y) |
                is.infinite(X) |
                is.infinite(Y) |
                !is.numeric(X) |
                !is.numeric(Y)   ]) != 0) {
    cat("\nMissing coordinates!!\n")
    cat("Add some code to fix!!\n")
}

if ( nrow( data[ is.na(time)] ) > 0 ) {
    cat(paste(nrow(data[is.na(time)]), "Points missing times\n"))

    mistime <- data[ is.na(time), .N, by = file]
    cat(paste(nrow(mistime), "Files with missing times\n"))

    ## show on terminal
    # print(mistime[,.(N,file)])
    ## write to file
    gdata::write.fwf(mistime[,.(N,file)],
                     sep = " ; ",
                     file = paste0(baseoutput,"Files_points_no_time.csv") )
    ## clean bad data
    data <- data[!is.na(time)]
}




####  Detect possible duplicate files  #########################################
cat(paste("Get posible duplicate files\n"))

## get files with points in the same date
file_dates <- data[, .N, by = .(Date = as.Date(time), file)]
same_date  <- list()
for (ad in unique(file_dates$Date)) {
    ad   <- as.Date(ad, origin = "1970-01-01")
    temp <- file_dates[Date == ad]
    if (nrow(temp) > 1 ) {
        # cat(paste(temp$file),"\n")
        same_date <- c(same_date, list(t(temp$file)))
    }
}

gather_tm <- data.table()
for (set in same_date) {
    temp <- data[file %in% set,]
    temp[,dist     := NULL]
    temp[,timediff := NULL]
    temp[,kph      := NULL]

    ## cast to spatial resolution
    temp[ , X :=  (X %/% resolution_spac * resolution_spac) + (resolution_spac/2) ]
    temp[ , Y :=  (Y %/% resolution_spac * resolution_spac) + (resolution_spac/2) ]

    # match_sp <- temp[, .N, by = c("X","Y","time","file")]
    # match_sp <- match_sp[, .(list(file),list(N)), by = c("X","Y")]
    # match_sp <- match_sp[sapply(match_sp$V1, length) > 1,]

    ## cast to temporal resolutions
    temp[ , time := (as.numeric(time) %/% resolution_temp * resolution_temp) ]
    temp[ , time := as.POSIXct(time,origin="1970-01-01")]

    match_tm <- temp[, .N, by = c("X","Y","time","file")]
    match_tm <- match_tm[, .(list(file),list(N)), by = c("X","Y","time")]
    match_tm <- match_tm[sapply(match_tm$V1, length) > 1,]

    gather_tm <- rbind(gather_tm,match_tm)
}


gather_tm$V1 <- sapply(gather_tm$V1, function(x) {paste(sort(unlist(x)), collapse = " ")})
full <- gather_tm[, .N, by = c("time","V1")]
# full[,X:=NULL]
# full[,Y:=NULL]
# max(full$N)

ccnt<-1
full$cnt<-0
for (fl in unique(full$V1)) {
    full[V1==fl, cnt:=ccnt]
    ccnt <- ccnt + 1
}

full <- full[order(full$V1),]

gdata::write.fwf(full[,.(cnt,N,time,V1)],
                 sep = " ", quote = FALSE,
                 file = paste0(baseoutput,"Dups_records_full.csv") )

short <- full[, .(Sum=sum(N)), by = "V1"]
short <- short[order(Sum)]

gdata::write.fwf(short[,.(Sum,V1)],
                 sep = " ", quote = FALSE,
                 file = paste0(baseoutput,"Dups_records.csv") )




####  Find data within a box  ##################################################

# South
latmin=39.93
# North
latmax=40.03

# West
lonmin=23.53
# East
lonmax=23.69



data[ Xdeg >= lonmin & Xdeg <= lonmax ]
data[ Ydeg >= latmin & Ydeg <= latmax ]

## find files in the box
## list them
## find the line number of matches in the file




# ## prepare for output for human edit
# gather_tm[, X := NULL ]
# gather_tm[, Y := NULL ]
#
# export <- gather_tm[, .(Mintime=min(time),Maxtime=max(time)) , by = V1]
# export[Mintime==Maxtime, Maxtime:=NA]
# export[, Mintime:=format(Mintime,"%FT%R:%S")]
# export[, Maxtime:=format(Maxtime,"%FT%R:%S")]
#
# export <- export[order(export$V1),]
#
# cat(paste(resolution_spac,"m spatial resolution\n"))
# cat(paste(resolution_temp/3600,"mins temporal resolution\n"))
#
# cat(paste(nrow(export),"set exported\n"))
# cat(paste(nrow(full),"set full exported\n"))
#
#
# gdata::write.fwf(export[order(Mintime),.(Mintime,Maxtime,V1)],
#                  sep = " ", quote = FALSE,
#                  file = paste0(baseoutput,"Dups_records.csv") )
#











####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f mins\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
