#!/usr/bin/env Rscript

#### Gather gpx waypoints, clean, group and report


####_ Set environment _####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()
if(!interactive())pdf(file=sub("\\.R$",".pdf",Script.Name),width = 14)
sink(file=sub("\\.R$",".out",Script.Name,),split=TRUE)


library(sf)
library(data.table)
library(dplyr)

options(warn=1)

regions_fl   <- "~/GISdata/Layers/path_regions.shp"
regions_fl2  <- "~/GISdata/Layers/path_regions_cnt.shp"
gpx_repo     <- "~/GISdata/GPX/"
polar_repo   <- "~/Documents/Running/Polar/"

results      <- "~/GISdata/Location_list.Rds"
waypoints_fl <- "~/GISdata/Location_waypoins.Rds"
wpt_seed     <- "~/GISdata/seed2.Rds"
wpt_seed3    <- "~/GISdata/seed3.Rds"



EPSG       <- 3857
close_flag <- 10       ## meters between point to flag as close
update     <- FALSE


#### list GPX files ####
gpxlist   <- list.files(gpx_repo, ".gpx$",
                        recursive   = T,
                        full.names  = T,
                        ignore.case = T)

polarlist <- list.files(polar_repo, ".gpx$",
                        recursive   = T,
                        full.names  = T,
                        ignore.case = T)

gpxlist <- c(polarlist, gpxlist)


## filter out some files
gpxlist <- gpxlist[grep("orig", basename(gpxlist), ignore.case = TRUE, invert = T)]



## check if we need to update data ####
if (file.exists(waypoints_fl)) {
    ## load old data
    gather_wpt <- readRDS(waypoints_fl)
    gather_wpt <- unique(gather_wpt)

    ## remove all data from missing files
    gather_wpt <- gather_wpt[ file.exists(gather_wpt$file), ]

    ## list parsed files
    dblist <- gather_wpt[, c("file","mtime") ]
    dblist$geometry <- NULL
    dblist <- unique( dblist )

    ## list all files
    fllist <- data.frame(file = gpxlist,
                         mtime = file.mtime(gpxlist),
                         stringsAsFactors = F)

    ## files to do
    ddd <- anti_join( fllist, dblist )

    ## remove data from changed files
    gather_wpt <- gather_wpt[ ! gather_wpt$file %in% ddd$file, ]

    gpxlist <- ddd$file

} else {
    gather_wpt <- readRDS(wpt_seed)
}



####  Read polygons for the regions  ####
regions <- st_read(regions_fl, stringsAsFactors = FALSE)
regions <- st_transform(regions, EPSG)
regions$NFiles  <- 0
regions$NPoints <- 0



wecare <- c("ele", "time", "magvar", "geoidheight", "name", "cmt", "desc", "src", "sym", "type", "ageofdgpsdata", "dgpsid", "geometry", "Region","file","mtime")


# gather     <- data.table()
# ff <- gather_wpt[1,]
# ff$geometry[[1]][1] <- 0
# ff$geometry[[1]][2] <- 0
# ff <- st_transform(ff,EPSG)
# saveRDS(ff, wpt_seed3)


ffff <- readRDS(wpt_seed3)



####  Get all waypoints from files  ####
if (length(gpxlist)>0) {
    update <- TRUE
    for ( af in gpxlist) {
        if (!file.exists(af)) { next() }
        cat(paste(af,"\n"))

        ####  get waypoints  ####
        gpx     <- read_sf(af, layer = "waypoints")
        if (nrow(gpx)>0) {

            wpt     <- st_transform(gpx, EPSG) # apply transformation to points sf

            ## get waypoints for the region
            wpt$file   <- af
            wpt$Region <- NA
            wpt$mtime  <- file.mtime(af)
            # gather_wpt <- c(gather_wpt, selc)

            gather_wpt <- rbind(gather_wpt,
                                wpt[,wecare])
            ## seed
            # saveRDS(wpt[,wecare] ,wpt_seed )
            # cat(paste(names(selc), collapse = '", "'))

        } else {
            ## keep track of empty files
            ff <- ffff
            ff$file   <- af
            ff$Region <- NA
            ff$mtime  <- file.mtime(af)

            gather_wpt <- rbind(gather_wpt,  ff)

        }
    }
}


## characterize all wpt ####
## go through polygons
for (ii in 1:length(regions$Name)) {

    cat(paste("Characterize", regions$Name[ii],"\n"))
    vec <- apply(st_intersects(regions$geometry[ii], gather_wpt$geometry, sparse = FALSE), 2,
                 function(x) { x })
    gather_wpt$Region[ vec ] <- regions$Name[ii]

}
table( gather_wpt$Region )

## store for all R
if (update) {
    RAerosols::write_RDS(gather_wpt, waypoints_fl)
}

## remove dummy data for analysis ####
ssel <- gather_wpt$geometry == ffff$geometry
gather_wpt <- gather_wpt[ ! ssel, ]


##TEST for testing ####
# gather_wpt <- readRDS(waypoints_fl)

## clean
gather_wpt <- gather_wpt[ ! lapply(gather_wpt$geometry, length) != 2, ]
gather_wpt <- gather_wpt[ unique(which(apply(!is.na(st_coordinates(gather_wpt$geometry)),1,all))), ]

## transform to degrees
gather_wpt <- st_transform(gather_wpt, 4326)

cat(paste("\n", nrow(gather_wpt),"waypoints parsed \n\n" ))


#### export unfiltered gpx ####
copywpt <- gather_wpt

## rename
names(copywpt)[names(copywpt)=="file"] <- 'desc'

## drop data
copywpt$file   <- NULL
copywpt$mtime  <- NULL
copywpt$Region <- NULL
write_sf(copywpt, '~/GISdata/Layers/Gathered_unfilt_wpt.gpx', driver = "GPX", append = F, overwrite = T)


## compute distance matrix unfiltered ####
distm <- raster::pointDistance(p1 = gather_wpt, lonlat = T, allpairs = T)
distm <- round(distm, digits = 3)


## find close points
dd <- which(distm < close_flag, arr.ind = T)
## remove diagonal
dd <- dd[dd[,1] != dd[,2], ]
paste( nrow(dd), "point couples under", close_flag,"m distance" )

## remove pairs 2,3 == 3,2
for (i in 1:nrow(dd)) {
    dd[i, ] = sort(dd[i, ])
}

# pA <- gather_wpt[dd[,1],]
# pA <- data.table(pA)
# pA <- pA[, .(Total_Dups = .N),by=file]
#
# for (ii in 1:nrow(pA)){
#     afi <- unlist(pA[ii,file])
#     pA[ii,Total_Dups]
#     gather_wpt$file == afi
# }



dd <- unique(dd)
paste( nrow(dd), "point couples under", close_flag,"m distance" )




####
suspects <- data.table(
    name_A = gather_wpt$name    [dd[,1]],
    geom_A = gather_wpt$geometry[dd[,1]],
    file_A = gather_wpt$file    [dd[,1]],
    name_B = gather_wpt$name    [dd[,2]],
    geom_B = gather_wpt$geometry[dd[,2]],
    file_B = gather_wpt$file    [dd[,2]],
    time_A = gather_wpt$time    [dd[,1]],
    time_B = gather_wpt$time    [dd[,2]],
    elev_A = gather_wpt$ele     [dd[,1]],
    elev_B = gather_wpt$ele     [dd[,2]]
)
suspects$Dist <- distm[ cbind(dd[,2],dd[,1]) ]
suspects <- suspects[order(suspects$Dist, decreasing = T) , ]
# suspects <- suspects[order(suspects$file_A,suspects$file_B, decreasing = T) , ]



## reformat for faster cvs use
suspects$time_A <- format( suspects$time_A, "%FT%R:%S" )
suspects$time_B <- format( suspects$time_B, "%FT%R:%S" )

wecare = grep("geom", names(suspects),invert = T,value = T )
wecare <- c("Dist","elev_A","time_A","name_A","name_B","file_A","file_B" )

gdata::write.fwf(suspects[, ..wecare],
                 sep = " ; ", quote = TRUE,
                 file = "~/GISdata/Layers/Suspects.csv" )


## ignore points in the same file
suspects <- suspects[name_A != name_B]

## count cases in files
filescnt <- suspects[, .(file_A,file_B) ]
filescnt <- filescnt[, .N , by = (paste(file_A,file_B))]
filescnt$Max_dist <- close_flag
setorder(filescnt, N)
gdata::write.fwf(filescnt,
                 sep = " ; ", quote = TRUE,
                 file = "~/GISdata/Layers/Suspect_point_to_clean.csv" )




####  Export filtered gpx for usage  ###########################################

## deduplicate WPT
gather_wpt <- unique(gather_wpt)
gather_wpt <- gather_wpt %>% distinct_at(vars(-file,-mtime), .keep_all = T)

## rename vars
gather_wpt$desc <- NULL
names(gather_wpt)[names(gather_wpt)=="file"] <- 'desc'

## drop data
gather_wpt$file   <- NULL
gather_wpt$mtime  <- NULL

## characterize missing regions
gather_wpt$Region[ is.na( gather_wpt$Region ) ] <- "Other"

## Clean waypoints names
gather_wpt <- gather_wpt[ grep("^Arrive at.*",           gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Dromos[0-9]*$",         gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^GRA[0-9]+$",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^GRE[0-9]*$",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Head [a-z]+",           gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^PIN[0-9]+$",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Turn .*",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^WPT[0-9]+$",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^XDRXRD.*$",             gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^[0-9]+!$",              gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^[0-9]+$",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^[0-9]+R",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^arxh",                  gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^at roundab$",           gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^dexia$",                gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^end$",                  gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^finish$",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^foto$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^from$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^go left$",              gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^hotmail.com",           gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^kato$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^keep .*",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^lap [0-9].*$",          gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^life [0-9]+",           gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^null$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^pagida *[0-9]+$",       gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^pagida a[0-9]+$",       gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^photo",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^scat [0-9]+$",          gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^scat$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^skat [0-9]+$",          gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^skat$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^start$",                gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^to$",                   gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^tor *[0-9]*",           gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^via[0-9]+$",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^xdr[0-9]+$",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Αρκ [0-1]+",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Αρκ$",                  gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Αρκ!$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^τριχεσ$",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Λυκ?",                  gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Γουρ!$",                gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Αρχή Μονοπατιού.*$",    gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Δείγ Ερθρλ",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Διασταυρωση$",          gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Διασταύρωση Junction$", gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Διασταύρωση$",          gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Κάτω δεξιά",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Κατω δεξιά",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^Κατω δεξια",            gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^από εδώ$",              gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^αριστερά$",             gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^αριστερα$",             gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^αρχή μονοπάτι",         gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^γου$",                  gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^γουρ$",                 gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^διαδρομή$",             gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^εδω",                   gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^εδώ",                   gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^κ[0-9]+",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^σκατ",                  gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("^ως εδώ$",               gather_wpt$name, invert = T, ignore.case = T), ]
gather_wpt <- gather_wpt[ grep("hotmail.com",            gather_wpt$name, invert = T, ignore.case = T), ]

# gather_wpt$name


cat(paste("\n", nrow(gather_wpt),"waypoints after filtering \n\n" ))

## ignore some waypoints files not relevant to our usage
drop_files <- c(
    "grammos2012/Acquired_from_GPS.gpx",
    "WPT_hair_traps_rodopi_2015-06-28.gpx",
    "WPT_stanes_rodopi.gpx"
)



## export gpx waypoints by region ####
for (ar in unique(gather_wpt$Region)) {

    temp <- gather_wpt[gather_wpt$Region == ar ,]
    temp$Region <- NULL
    temp <- temp[order(temp$name),]

    cat(paste("export",nrow(temp),"wpt",ar,"\n"))

    ## ignore some files
    for (ast in drop_files) {
        sel  <- !grepl(ast, temp$desc)
        temp <- temp[sel,]
    }


    ## export all data rot qgis
    if (nrow(temp)<1) { next() }
    write_sf(temp, paste0("~/LOGs/waypoints/wpt_",ar,".gpx"), driver = "GPX", append = F, overwrite = T)


    ## remove many data for etrex
    ##TODO you are removing useful info!!
    temp$cmt  <- NA
    temp$desc <- NA
    temp$src  <- NA

    write_sf(temp, paste0("~/LOGs/waypoints_etrex//wpt_",ar,".gpx"), driver = "GPX", append = F, overwrite = T)
}

## export all points for qgis
gather_wpt$Region <- NULL
write_sf(gather_wpt, '~/GISdata/Layers/Gathered_wpt.gpx', driver = "GPX", append = F, overwrite = T)



## compute distance matrix filtered ####
distm <- raster::pointDistance(p1 = gather_wpt, lonlat = T, allpairs = T)

## find close points
dd <- which(distm < close_flag, arr.ind = T)
## remove diagonal
dd <- dd[dd[,1] != dd[,2], ]
cat(paste( nrow(dd), "point couples under", close_flag,"m distance" ),"\n")

## remove pairs 2,3 == 3,2
for (i in 1:nrow(dd)) {
    dd[i, ] = sort(dd[i, ])
}
dd <- unique(dd)
cat(paste( nrow(dd), "point couples under", close_flag,"m distance" ),"\n")


####
suspects <- data.table(
    name_A = gather_wpt$name    [dd[,1]],
    geom_A = gather_wpt$geometry[dd[,1]],
    file_A = gather_wpt$desc    [dd[,1]],
    name_B = gather_wpt$name    [dd[,2]],
    geom_B = gather_wpt$geometry[dd[,2]],
    file_B = gather_wpt$desc    [dd[,2]],
    time_A = gather_wpt$time    [dd[,1]],
    time_B = gather_wpt$time    [dd[,2]],
    elev_A = gather_wpt$ele     [dd[,1]],
    elev_B = gather_wpt$ele     [dd[,2]]
)
suspects$Dist <- distm[ cbind(dd[,2],dd[,1]) ]
suspects <- suspects[order(suspects$Dist, decreasing = T) , ]

## ignore points in the same file
suspects <- suspects[name_A != name_B]

## count cases in files
filescnt <- suspects[, .(file_A,file_B) ]
filescnt <- filescnt[, .N , by = (paste(file_A,file_B))]
filescnt$Max_dist <- close_flag
setorder(filescnt, N)
write.csv(filescnt, "~/GISdata/Layers/Suspect_point_to_clean_filtered.csv", row.names = FALSE)
# gdata::write.fwf(filescnt,
#                  sep = " ; ", quote = TRUE,
#                  file = "~/GISdata/Layers/Suspect_point_to_clean_filtered.csv" )



wecare = grep("geom", names(suspects),invert = T,value = T )
wecare <- c("Dist","name_A","name_B","file_A","file_B" )

write.csv(suspects[,..wecare], "~/GISdata/Layers/Suspects_filtered.csv", row.names = FALSE)
# gdata::write.fwf(suspects[,..wecare],
#                  sep = " ; ", quote = TRUE,
#                  file = "~/GISdata/Layers/Suspects_filtered.csv" )

## export all points for gps devices
gather_wpt$Region <- NULL
gather_wpt$cmt  <- NA
gather_wpt$desc <- NA
gather_wpt$src  <- NA
write_sf(gather_wpt, '~/LOGs/waypoints_etrex/WPT_ALL.gpx', driver = "GPX", append = F, overwrite = T)






####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
