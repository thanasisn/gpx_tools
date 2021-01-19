#!/usr/bin/env Rscript

#'
#' #### Split google location history to smaller manageable files
#' Probably we don't this.
#' The output may need some manual adjustment
#'


####    Set environment    ####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()


## break every n data points
breaks <- 10000

## input file
file  <- "~/LOGs/Takeout/Location History/Location History.json"

## output base
outdir <- "/dev/shm/glh/"
dir.create(outdir, showWarnings = F )

nfile <- paste0(outdir, "master_temp.json")

## create a working file
file.copy(file, nfile)
rm(file)

## remove some decorations of the file
system(paste("sed -i '/\"locations\" :/d'", nfile ))
system(paste("sed -i 's/^{$/[ {/'", nfile ))
system(paste("sed -i '$d'", nfile ))

## read the file
lines <- readLines(nfile)

## find the location of each point in the file
ntim <- grep("timestampMs", lines)
nlat <- grep("latitudeE7", lines)
nlon <- grep("longitudeE7", lines)

nlat <- nlat - 1
nlon <- nlon - 2

## these are sets anyway
stopifnot( all(nlat == nlon) )

## the location of every point
npoints <- intersect(ntim,nlat)
## distribute the point to chunks
targets <- which( npoints %% breaks == 1)
## index of spit points
spltlin <- c(1,npoints[targets], length(lines))

for ( ii in 1:(length(spltlin)-1) ) {
    from  <- spltlin[ii]
    until <- spltlin[ii+1]

    cat("\n")
    cat(paste(from, until,"\n"))

    # ## inspect chunk start
    # cat(lines[(from-1):(from+4)],sep = "\n")
    #
    # cat("\n")
    #
    # ## inspect chunk end
    # cat(lines[(until-5):(until-1)],sep = "\n")

    temp <- lines[(from-1):(until-1)]

    ## fix proper ends
    if (grepl("\\}, \\{", temp[length(temp)])) {
        temp[length(temp)] <- "} ]"
    }

    ## fix proper starts
    if (grepl("\\}, \\{", temp[1])) {
        temp[1] <- "[ {"
    }

    ## fix end all for the last chunk
    if ( ii+1 == length(spltlin) ) {
        temp <- c(temp,"} ]")
    }

    ## inspect output
    cat(head(temp),sep = "\n")
    cat("......\n")
    cat(tail(temp),sep = "\n")

    ## write splited files
    writeLines( temp, paste0(outdir,"GLH_part_", sprintf("%04d",ii), ".json"))
}
# file.remove(nfile)


####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
