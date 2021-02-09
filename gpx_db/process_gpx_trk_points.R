#!/usr/bin/env Rscript

#### Process track points data
## Filter, aggregate, analyze points and tracks
## Find possible bad or duplicate data
## Create reports

####_ Set environment _####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()
if(!interactive())pdf(file=sub("\\.R$",".pdf",Script.Name),width = 14)
sink(file=sub("\\.R$",".out",Script.Name,),split=TRUE)



library(data.table)
library(sf)


EPSG           <- 3857
trackpoints_fl <- paste0( "~/GISdata/Count_sl2_",EPSG,".Rds")
baseoutput     <- "~/GISdata/"


## load data
data           <- readRDS(trackpoints_fl)
data[, F_mtime:=NULL]

## remove fake dates
data[ time < "1971-01-01", time := NA ]


hist(data$time , breaks = 100)


cat(paste( length(unique( data$file )), "total files parsed\n" ))
cat(paste( nrow( data ), "points parsed\n" ))

## create speed
data$kph <- (data$dist / 1000) / (data$timediff / 3600)


#### clean problematic data ####
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







####  Detect possible duplicate files  ####
cat(paste("Get possible duplicate files\n"))

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

## check the files if have dups points
dup_points <- data.frame()
for (il in 1:length(same_date)) {

    temp <- data[ file %in% same_date[[il]]]

    ## only when we have time
    temp <- temp[ !is.na(time) ]

    setkey(temp, time, X, Y)
    dups <- duplicated(temp, by = key(temp))
    temp[, fD := dups | c(tail(dups, -1), FALSE)]
    duppoints <- temp[fD == TRUE]

    ## files with dups points
    countP <- duppoints[ , .(DupPnts = .N, STime = min(time), ETime = max(time)) , by = .(file ) ]
    countP$TotPnts <- 0
    countP$Set     <- il
    for (af in unique(countP$file)) {
        countP[file==af, TotPnts := data[file == af, .N] ]
    }
    dup_points <- rbind(dup_points, countP)
}

dup_points[, Cover := DupPnts / TotPnts]

cat(paste(nrow(duppoints), "Duplicate points found\n"))


dup_points$STime <- format( dup_points$STime, "%FT%R:%S" )
dup_points$ETime <- format( dup_points$ETime, "%FT%R:%S" )


## get sets with big coverage
gdata::write.fwf(dup_points[ Set %in% unique(dup_points[ Cover >= 0.95, Set]),
                             .(Set, file, DupPnts, TotPnts, Cover, STime, ETime) ],
                 sep = " ; ", quote = FALSE,
                 file = paste0(baseoutput,"Dups_point_suspects.csv") )

## get all sets
gdata::write.fwf(dup_points[,.(Set, file, DupPnts, TotPnts, Cover, STime, ETime)],
                 sep = " ; ", quote = FALSE,
                 file = paste0(baseoutput,"Dups_point_suspects_all.csv") )



####  Filter data by speed  #####

##TODO
hist(data$timediff)
hist(data$dist)
hist(data$kph)

table((data$timediff %/% 5) * 5 )
table((data$dist     %/% 1000) * 1000 )
table(abs(data$kph   %/% 200) * 200 )

cat(paste("\nGreat distances\n"))
data[dist > 100000, .( .N, MaxDist = max(dist)) , by = file]

cat(paste("\nGreat speeds\n"))
data[kph > 500000 & !is.infinite(kph), .(.N, MaxKph = max(kph), time[which.max(kph)]) , by = file ]

cat(paste("\nGreat times\n"))
data[timediff > 600 , .(.N, MaxTDiff = max(timediff), time = time[which.max(timediff)]) , by = file ]


# esss <- data[kph > 200, .(file, kph, timediff, dist ,time) ]
# setorder(esss, kph)

# data[dist > 200, .(max(kph), time[which.max(kph)] ), by = file ]

# data[timediff==0]

# data[dist==0 & timediff==0]
# data[dist>0 & dist < 10 & timediff==0]

# data[is.infinite(kph), .(max(dist), time[which.max(dist)]) ,by = file]

# data[dist<0]





####   Binned data output   ####
cat(paste("\nBin points by location\n"))

## bin sizes to export in meters
rsls <- unique(c(
       5,
      10,
      20,
      50,
     100,
     500,
    1000,
    5000,
   10000,
   20000,
   50000 ))


### Stats on all my gps data ####

## exclude some data paths not mine
data <- data[ grep("/Plans/",   file, invert = T ), ]
data <- data[ grep("/E_paths/", file, invert = T ), ]
data <- data[ grep("/ROUT/",    file, invert = T ), ]

cat(paste( length(unique( data$file )), "files to bin\n" ))
cat(paste( nrow( data ), "points to bin\n" ))






####  Stats on all data  #######################################################

## get unique points
setkey( data, time, X, Y )

## remove duplicate points
ddaa <- unique( data[list(time, X, Y), nomatch = 0]  )

## clean coordinates
ddaa <- ddaa[ !is.na(X) ]
ddaa <- ddaa[ !is.na(Y) ]

cat(paste( length(unique( ddaa$file )), "unique files sorted\n" ))
cat(paste( nrow( ddaa ), "unique points to bin\n" ))


####  export for each year  ####
traindb <- "~/GISdata/Layers/Grid_ALL_yearly.gpkg"
years   <- unique(year(data$time))
for (ay in years) {
  for (res in rsls) {
    ## get year data and simplify coordinates
    counts <- copy(ddaa[year(time)==ay])
    counts[ , X :=  (X %/% res * res) + (res/2) ]
    counts[ , Y :=  (Y %/% res * res) + (res/2) ]

    cat(paste(nrow(counts[, .N ,by = .(X,Y) ]), "bins at", res/1000, "km for", ay ),"\n")

    ## count point in cells
    temp <- counts[ , .(.N, res = res, data = ay), by = .(X,Y) ]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%s %5sm %s", ay, res, "point cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

    ## count days in each cell
    temp <- counts[ , .(N = length(unique(as.Date(time))), res = res, data = ay), by = .(X,Y)]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%s %5sm %s", ay, res, "days cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

    ## count hours in each cell
    temp <- counts[ , .(N = length(unique( as.numeric(time) %/% 3600 * 3600 )), res = res, data = ay), by = .(X,Y)]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%s %5sm %s", ay, res, "hour cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

  }
}

####  export all data  ####
traindb <- "~/GISdata/Layers/Grid_ALL_all.gpkg"
for (res in rsls) {
  ## get year data and simplify coordinates
  counts <- copy(ddaa)
  counts[ , X :=  (X %/% res * res) + (res/2) ]
  counts[ , Y :=  (Y %/% res * res) + (res/2) ]

  cat(paste(nrow(counts[, .N ,by = .(X,Y) ]), "bins at", res/1000, "km for all" ),"\n")

  ## count point in cells
  temp <- counts[ , .(.N, res = res, data = ay), by = .(X,Y) ]
  temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
  layr <- sprintf("%5sm %s", res, "point cnt")
  st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

  ## count days in each cell
  temp <- counts[ , .(N = length(unique(as.Date(time))), res = res, data = ay), by = .(X,Y)]
  temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
  layr <- sprintf("%5sm %s", res, "days cnt")
  st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

  ## count hours in each cell
  temp <- counts[ , .(N = length(unique( as.numeric(time) %/% 3600 * 3600 )), res = res, data = ay), by = .(X,Y)]
  temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
  layr <- sprintf("%5sm %s", res, "hour cnt")
  st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)
}



####  Stats on training data only  #############################################

ddaa <-rbind(
  data[ grep("/TRAIN/", file ), ],
  data[ grep("/Running/Polar/", file ), ]
)



cat(paste( length(unique( ddaa$file )), "files to bin\n" ))
cat(paste( nrow( ddaa ), "points to bin\n" ))

## get unique points
setkey( ddaa, time, X, Y )

## remove duplicate points
ddaa <- unique( ddaa[list(time, X, Y), nomatch = 0]  )

## clean coordinates
ddaa <- ddaa[ !is.na(X) ]
ddaa <- ddaa[ !is.na(Y) ]

cat(paste( length(unique( ddaa$file )), "unique files sorted\n" ))
cat(paste( nrow( ddaa ), "unique points to bin\n" ))


####  export for each year training data  ####
traindb <- "~/GISdata/Layers/Grid_train_yearly.gpkg"
years   <- unique(year(data$time))
for (ay in years) {
    for (res in rsls) {
        ## get year data and simplify coordinates
        counts <- copy(ddaa[year(time)==ay])
        counts[ , X :=  (X %/% res * res) + (res/2) ]
        counts[ , Y :=  (Y %/% res * res) + (res/2) ]

        cat(paste(nrow(counts[, .N ,by = .(X,Y) ]), "bins at", res/1000, "km for", ay ),"\n")

        ## count point in cells
        temp <- counts[ , .(.N, res = res, data = ay), by = .(X,Y) ]
        temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
        layr <- sprintf("%s %5sm %s", ay, res, "point cnt")
        st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

        ## count days in each cell
        temp <- counts[ , .(N = length(unique(as.Date(time))), res = res, data = ay), by = .(X,Y)]
        temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
        layr <- sprintf("%s %5sm %s", ay, res, "days cnt")
        st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

        ## count hours in each cell
        temp <- counts[ , .(N = length(unique( as.numeric(time) %/% 3600 * 3600 )), res = res, data = ay), by = .(X,Y)]
        temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
        layr <- sprintf("%s %5sm %s", ay, res, "hour cnt")
        st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

    }
}


####  export all training data ####
traindb <- "~/GISdata/Layers/Grid_train_all.gpkg"
for (res in rsls) {
    ## get year data and simplify coordinates
    counts <- copy(ddaa)
    counts[ , X :=  (X %/% res * res) + (res/2) ]
    counts[ , Y :=  (Y %/% res * res) + (res/2) ]

    cat(paste(nrow(counts[, .N ,by = .(X,Y) ]), "bins at", res/1000, "km for all" ),"\n")

    ## count point in cells
    temp <- counts[ , .(.N, res = res, data = ay), by = .(X,Y) ]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%5sm %s", res, "point cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

    ## count days in each cell
    temp <- counts[ , .(N = length(unique(as.Date(time))), res = res, data = ay), by = .(X,Y)]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%5sm %s", res, "days cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

    ## count hours in each cell
    temp <- counts[ , .(N = length(unique( as.numeric(time) %/% 3600 * 3600 )), res = res, data = ay), by = .(X,Y)]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%5sm %s", res, "hour cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)
}






####  Stats on non training data only  #########################################

ddaa <- data[ ! grep("/Running/Polar/", file ), ]

cat(paste( length(unique( ddaa$file )), "files to bin\n" ))
cat(paste( nrow( ddaa ), "points to bin\n" ))

## get unique points
setkey( ddaa, time, X, Y )

## remove duplicate points
ddaa <- unique( ddaa[list(time, X, Y), nomatch = 0]  )

## clean coordinates
ddaa <- ddaa[ !is.na(X) ]
ddaa <- ddaa[ !is.na(Y) ]

cat(paste( length(unique( ddaa$file )), "unique files sorted\n" ))
cat(paste( nrow( ddaa ), "unique points to bin\n" ))


####  export each year non training data  ####
traindb <- "~/GISdata/Layers/Grid_rest_yearly.gpkg"
years   <- unique(year(data$time))
for (ay in years) {
  for (res in rsls) {
    ## get year data and simplify coordinates
    counts <- copy(ddaa[year(time)==ay])
    counts[ , X :=  (X %/% res * res) + (res/2) ]
    counts[ , Y :=  (Y %/% res * res) + (res/2) ]

    cat(paste(nrow(counts[, .N ,by = .(X,Y) ]), "bins at", res/1000, "km for", ay ),"\n")

    ## count point in cells
    temp <- counts[ , .(.N, res = res, data = ay), by = .(X,Y) ]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%s %5sm %s", ay, res, "point cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

    ## count days in each cell
    temp <- counts[ , .(N = length(unique(as.Date(time))), res = res, data = ay), by = .(X,Y)]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%s %5sm %s", ay, res, "days cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

    ## count hours in each cell
    temp <- counts[ , .(N = length(unique( as.numeric(time) %/% 3600 * 3600 )), res = res, data = ay), by = .(X,Y)]
    temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
    layr <- sprintf("%s %5sm %s", ay, res, "hour cnt")
    st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

  }
}


####  export all non training data  ####
traindb <- "~/GISdata/Layers/Grid_rest_all.gpkg"
for (res in rsls) {
  ## get year data and simplify coordinates
  counts <- copy(ddaa)
  counts[ , X :=  (X %/% res * res) + (res/2) ]
  counts[ , Y :=  (Y %/% res * res) + (res/2) ]

  cat(paste(nrow(counts[, .N ,by = .(X,Y) ]), "bins at", res/1000, "km for all" ),"\n")

  ## count point in cells
  temp <- counts[ , .(.N, res = res, data = ay), by = .(X,Y) ]
  temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
  layr <- sprintf("%5sm %s", res, "point cnt")
  st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

  ## count days in each cell
  temp <- counts[ , .(N = length(unique(as.Date(time))), res = res, data = ay), by = .(X,Y)]
  temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
  layr <- sprintf("%5sm %s", res, "days cnt")
  st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)

  ## count hours in each cell
  temp <- counts[ , .(N = length(unique( as.numeric(time) %/% 3600 * 3600 )), res = res, data = ay), by = .(X,Y)]
  temp <- st_as_sf(temp, coords = c("X", "Y"), crs = EPSG, agr = "constant")
  layr <- sprintf("%5sm %s", res, "hour cnt")
  st_write(temp, traindb, layer = layr, append = FALSE, delete_layer= TRUE)
}





####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f mins\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
