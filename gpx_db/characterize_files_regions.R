#!/usr/bin/env Rscript


#### Characterize gpx files by regions in which they are


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
library(sfheaders)
library(myRtools)

##TODO create a new shapefile with count in each region

## this is layer with multiple polygons in which the gpx files may be
regions_fl     <- "~/GISdata/Layers/path_regions.shp"

EPSG           <- 3857
trackpoints_fl <- paste0("~/GISdata/Count_sl2_",EPSG,".Rds")

resolution     <- 50

## prepare data
data           <- readRDS(trackpoints_fl)
data[, F_mtime := NULL]
data[, time    := NULL]
data[, dist    := NULL]
data[, timediff:= NULL]


cat(paste( length(unique( data$file )), "total files parsed\n" ))
cat(paste( nrow( data ), "points parsed\n" ))

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

## drop resolution of files
data[ , X :=  (X %/% resolution * resolution) + (resolution/2) ]
data[ , Y :=  (Y %/% resolution * resolution) + (resolution/2) ]

data <- unique(data)
data[ , N:=.N, by = file]



#### read polygons for the regions ####
regions <- st_read(regions_fl, stringsAsFactors = FALSE)
regions <- st_transform(regions, EPSG)


## characterize all files with all regions
gather         <- data.table()
reproj         <- sf_point(data, x = "X", y = "Y")
st_crs(reproj) <- 3857

for (ii in 1:length(regions$Name)) {

    cat(paste("Characterize", regions$Name[ii],"\n"))

    vec <- apply(st_intersects(regions$geometry[ii], reproj, sparse = FALSE), 2,
                 function(x) { x })

    cat(paste(sum(vec),"points\n"))

    gather <- rbind(gather,
                    cbind(data[vec, .(FN=.N), by= .(file,N)], Region = regions$Name[ii]))

}

## characterize rest of files
files  <- unique(data$file)
gather <- rbind(gather,
                cbind(file = files[ ! files %in% gather$file], Region = "Other"),fill=T)

write_RDS(gather, file = "~/GISdata/Location_list_2.Rds")



####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
