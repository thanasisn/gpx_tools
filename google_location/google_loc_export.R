#!/usr/bin/env Rscript

#'
#' Export google location history to RDS or plot on map
#'

####    Set environment    ####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()


library(jsonlite)
library(data.table)
# library(dplyr)
# library(OpenStreetMap)
# library(rgdal)


## This is a big file to read
locations <- data.table(fromJSON("~/LOGs/Takeout/Location History/Location History.json"))
locations <- data.table(locations[[1]][[1]])

## proper dates
locations[, Date := as.POSIXct(as.numeric(timestampMs)/1000, tz='GMT', origin='1970-01-01') ]
locations[, timestampMs := NULL]

## proper coordinates
locations[, Lat  := latitudeE7  / 1e7 ]
locations[, Long := longitudeE7 / 1e7 ]
locations[, latitudeE7  := NULL]
locations[, longitudeE7 := NULL]

## clean data
locations[ Long == 0, Long := NA ]
locations[ Lat  == 0, Lat  := NA ]
locations <- locations[ !is.na(Long) ]
locations <- locations[ !is.na(Lat)  ]

hist(locations$Lat)
hist(locations$Long)

locations <- locations[ abs(Lat)  <  89.9999 ]
locations <- locations[ abs(Long) < 179.9999 ]


unique(locations$activity)

stop("refactor")

# source("~/FUNCTIONS/R/plotting.R")
#
# basedir  = "~/DATA_RAW/Other/GLH/"
# pdfbyday = paste0(basedir,"Montly_plots_by_day.pdf")
# pdfbyact = paste0(basedir,"Montly_plots_by_act.pdf")

ACTIVITY_MATCH_THRESHOLD = 60*3  ## time distance of the valid characterization


## test
# if (F) {
#     subscr <- data.frame(lat=c(10.1237,10.2161,10.2993),
#                        lon = c(59.7567,59.7527,59.6863), pop=c(58,12,150))
#     coordinates(subscr) <- ~lat+lon
#     proj4string(subscr) <- CRS("+init=epsg:4326")
#     lat <- c(59.7916,59.6563)
#     lon <- c(10.0937,10.3293)
#     map <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=10,'osm')
#     plot(map)
#     points(spTransform(subscr,osm()))
# }


# https://www.martijnvanvreeden.nl/2018/08/11/analysing-google-loc(ation-data/

## files are prepared and spited in here
# folderin <- "/dev/shm/glh/"

# fillist <- list.files( path       = folderin,
#                        pattern    = "GLH_part_.*.json",
#                        full.names = T)
#
# cat(paste("Parse json files\n"))
# gather <- data.table()
# for (afil in fillist) {
#     cat(paste(afil),sep = "\n")
#     locations <- data.table(fromJSON(afil))
#
#     ## proper dates
#     locations[, Date := as.POSIXct(as.numeric(timestampMs)/1000, tz='GMT', origin='1970-01-01') ]
#     locations[, timestampMs := NULL]
#
#     ## proper coordinates
#     locations[, Lat  := latitudeE7  / 1e7 ]
#     locations[, Long := longitudeE7 / 1e7 ]
#
#     locations[, latitudeE7  := NULL]
#     locations[, longitudeE7 := NULL]
#
#     locations[ Long == 0, Long := NA ]
#     locations[ Lat  == 0, Lat  := NA ]
#
#     locations <- locations[ !is.na(Long) ]
#     locations <- locations[ !is.na(Lat)  ]
#
#     ## this will break with more data probably
#     gather <- plyr::rbind.fill(gather, locations)
#
#     # plot(locations$Long, locations$Lat)
# }
#
# gather <- data.table(gather)
#
# gather <- gather[ abs(Lat)  <  89.9999 ]
# ## 0 - 360? 0r -180 - 180
# gather <- gather[ abs(Long) < 179.9999 ]


####  export daily data  ####
for (aday in unique(as.Date(gather$Date))) {
    daydata <- gather[ as.Date(Date) == aday  ]
    setorder(daydata, Date)
    today   <- as.Date(daydata[1,Date])
    cat(paste("Save:", today))

    ydirec <- paste0(basedir, year(daydata[1,Date]), "/" )
    dir.create(ydirec,showWarnings = F)

    ## add main activity data
    activities <- daydata$activity
    sel        <- sapply(activities, function(x) !is.null(x[[1]]))
    activities <- activities[sel]
    df3        <- do.call("bind_rows", activities)

    main_activity <- sapply(df3$activity, function(x) x[[1]][1][[1]][1])
    activities_2  <- data.table(main_activity = main_activity,
                               time = as.POSIXct(as.numeric(df3$timestampMs)/1000, origin = "1970-01-01"))
    setorder(activities_2, time)

    activities_2$main_activity <- factor(activities_2$main_activity)

    ## find nearest
    mi <- RAerosols::nearest( target =  as.numeric(activities_2$time),
                              probe  =  as.numeric(daydata$Date ))

    ## add posible main activity
    daydata$main_activity <- activities_2$main_activity[mi]

    ## apply a time thersholld of validity for main activity
    not_valid_idx <- which( as.numeric( abs(activities_2$time[mi] - daydata$Date) ) > ACTIVITY_MATCH_THRESHOLD  )

    daydata$main_activity[ not_valid_idx ] <- "UNKNOWN"

    cat(print(table(daydata$main_activity)),"\n")

    saveRDS( object = daydata,
             file   = paste0(ydirec,"GLH_",today,".Rds"),
             compress = "xz")
}




####  add main activity to data  ####
activities <- gather$activity
sel        <- sapply(activities, function(x) !is.null(x[[1]]))
activities <- activities[sel]
df3        <- do.call("bind_rows", activities)

main_activity <- sapply(df3$activity, function(x) x[[1]][1][[1]][1])
activities_2  <- data.table(main_activity = main_activity,
                            time = as.POSIXct(as.numeric(df3$timestampMs)/1000, origin = "1970-01-01"))
setorder(activities_2, time)

activities_2$main_activity <- factor(activities_2$main_activity)

## find nearest
mi <- RAerosols::nearest( target =  as.numeric(activities_2$time),
                          probe  =  as.numeric(gather$Date ))

## add posible main activity
gather$main_activity <- activities_2$main_activity[mi]

## apply a time thersholld of validity for main activity
not_valid_idx <- which( as.numeric( abs(activities_2$time[mi] - gather$Date) ) > ACTIVITY_MATCH_THRESHOLD  )

gather$main_activity[ not_valid_idx ] <- "UNKNOWN"




####  Plot monthly by day  ####
cat(paste("Plot monthly by day"),"\n")
months <- unique(gather[ , .(Month = month(Date), Year = year(Date)) ])

pdf(file = pdfbyday,width = 8, height = 8)
for (aa in 1:nrow(months)) {
    cat(paste(aa,nrow(months)),sep = "\n")
    pp <- gather[ month(Date) == months$Month[aa] & year(Date) == months$Year[aa], c("Lat","Long","Date") ]

    coordinates(pp) <- ~ Long + Lat
    proj4string(pp) <- CRS("+init=epsg:4326")

    tmp <- as.data.frame(spTransform(pp,osm()))

    ## always include thessaloniki
    lat   <- range(pp$Lat , 40.633 )
    lon   <- range(pp$Long, 22.951 )

    latex <- abs(diff(lat)) * 0.3 + 0.2
    lat   <- c(lat[1] - latex, lat[2] + latex  )
    lonex <- abs(diff(lon)) * 0.2 + 0.1
    lon   <- c(lon[1] - lonex, lon[2] + lonex  )

    uda   <- length(unique(as.Date(tmp$Date)))
    uda2  <- uda
    if (uda2 <= 1 ) uda2 <- 2

    # ## zoom = 10 and NULL is good 12 is extreme
    map <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=NULL,'osm')
    plot(map)
    points(tmp$Long, tmp$Lat, col = clr_smth(data = mday(tmp$Date),
                                             colors = c("magenta","blue","green","orange","red"),
                                             breaks = uda2 )$cols, pch = 19, cex = .9)
    title(main = paste(" d:", uda," p:", length(tmp$Long), "  Y:", year(tmp$Date[1])," M:", month(tmp$Date[1]) ))

    }
dev.off()


####  Plot monthly by activity  ####
cat(paste("Plot monthly by activity"),"\n")
pdf(file = pdfbyact,width = 8, height = 8)
for (aa in 1:nrow(months)) {
    cat(paste(aa,nrow(months)), sep = "\n")
    pp <- gather[ month(Date) == months$Month[aa] & year(Date) == months$Year[aa], c("Lat","Long","Date","main_activity") ]

    coordinates(pp) <- ~ Long + Lat
    proj4string(pp) <- CRS("+init=epsg:4326")

    tmp <- as.data.frame(spTransform(pp,osm()))

    ## always include thessaloniki
    lat   <- range(pp$Lat , 40.633 )
    lon   <- range(pp$Long, 22.951 )

    latex <- abs(diff(lat)) * 0.4 + 0.25
    lat   <- c(lat[1] - latex, lat[2] + latex  )
    lonex <- abs(diff(lon)) * 0.2 + 0.1
    lon   <- c(lon[1] - lonex, lon[2] + lonex  )


    uda  <- length(unique( tmp$main_activity ))
    uda2 <- uda
    if (uda2 <= 1 ) uda2 <- 2

    # ## zoom = 10 and NULL is good 12 is extreme
    map <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=NULL,'osm')
    plot(map)
    points(tmp$Long, tmp$Lat,
           col = clr_smth(data = as.numeric(tmp$main_activity),
                                             colors = c("magenta","blue","green","orange","red"),
                                             breaks = uda2 )$cols, pch = 19, cex = .9)

    ## color by date
    title(main = paste(" a:", uda," p:", length(tmp$Long), "  Y:", year(tmp$Date[1])," M:", month(tmp$Date[1]) ))
}
dev.off()

cat("Export ended\n")

