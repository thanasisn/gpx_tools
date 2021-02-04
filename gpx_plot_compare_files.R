#!/usr/bin/env Rscript


####_ Set environment _####
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")

args <- commandArgs(trailingOnly=TRUE)

##TEST
# args <- c("2.gpx", "1.gpx")

## Use a system command to also open the files
system_command <- "viking"

## TODO a metho to plot waypoints
## cases with only waypoints
## cases with only tracks


## test if there is at least one argument: if not, return an error
if (length(args)==0) {
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
}


####  Check  args  ####
files <- c()
for (aa in args) {
    if (file.exists(aa)) {
        files <- c(files,aa)
    }
}
files <- unique(files)

if (length( files ) == 0) {
    cat(paste("No file exist\n"))
} else {
    cat(paste("Found",length(files),"files\n"))
    cat(paste(files,"\n"))
}



####  GPX files analysis  ####
library(sf)
library(data.table)

## Read track points
wecare <- c("ele","time","file")
gather <- data.table()
for (af in files) {
    tracks      <- read_sf(af, layer = "track_points")
    tracks$file <- af
    trkcco      <- st_coordinates(tracks)
    tracks      <- data.table(tracks)
    tracks      <- tracks[, ..wecare ]
    tracks$X    <- unlist(trkcco[,1])
    tracks$Y    <- unlist(trkcco[,2])
    gather      <- rbind(gather, tracks)
}

## Read waypoints
wecare <- c("ele","time","file","name")
gather_wpt <- data.table()
for (af in files) {
    wpt        <- read_sf(af, layer = "waypoints")
    wpt$file   <- af
    wptcco     <- st_coordinates(wpt)
    wpt        <- data.table(wpt)
    wpt        <- wpt[, ..wecare ]
    wpt$X      <- unlist(wptcco[,1])
    wpt$Y      <- unlist(wptcco[,2])
    gather_wpt <- rbind(gather_wpt, wpt)
}

## TODO by file
cat(paste("Waypoints:  ", nrow(gather_wpt)),"\n")
cat(paste("Trackpoints:", nrow(gather)    ),"\n")


if (all(is.na(gather$time))) {
    NO_TIMES <- TRUE
} else {
    NO_TIMES <- FALSE
}



####  Plots  ####

##TODO
## Add map?
## Plot speed
## Plot elevation


X11()

if (!NO_TIMES) {
    layout(matrix(c(1,1,2), 3, 1, byrow = TRUE))
}

## plot track points
par(mar=c(2.5,2,0.3,0.3))
plot(1, type="n", xlab="", ylab="", xlim=range(gather$X), ylim=range(gather$Y),cex.axis = 0.7)
cols <- 2
fnam <- c()
lcol <- c()
for (af in unique(gather$file)) {
    temp <- gather[file==af]
    lines(temp$X, temp$Y, col=scales::alpha(cols , 0.5), lwd = 3)

    lcol <- c(lcol, cols)
    fnam <- c(fnam, basename(af))

    cols  <- cols + 1
}
par(xpd=TRUE)
# legend(min(gather$X),max(gather$Y), legend = fnam, pch=19, col = lcol,
#        cex = 0.6, ncol = 2, bty="n")
legend("topleft", legend = fnam, pch=19, col = lcol,
       cex = 0.7, ncol = 2, bty="n")



if (!NO_TIMES) {

    ##TODO
    ## A better method to plot files with and without time?
    ## A method to plot files with no times?

    cols <- 2
    comp <- 0.1

    plot(1, xaxt="n",yaxt="n", type="n", xlab="", ylab="", xlim=range(gather$time,na.rm = T), ylim=c(0,cols+length(unique(gather$file))))

    fnam <- c()
    lcol <- c()
    for (af in unique(gather$file)) {
        temp <- gather[file==af]
        # temp$col <- 2
        temp$col <- cols
        points(temp$time, comp*temp$col, col = scales::alpha(cols , 0.5))

        lcol <- c(lcol, cols)
        fnam <- c(fnam, basename(af))

        cols  <- cols+1
    }
    # legend(min(gather$X),max(gather$Y), legend = fnam, pch=19, col = lcol,
    #        cex = 0.6, ncol = 2, bty="n")
    legend("bottomleft", legend = fnam, pch=19, col = lcol,
           cex = 0.6, ncol = 2, bty="n")
    axis.POSIXct(1, gather$time, cex.axis = 0.7)

}







####  Call a system program  ####
try(
    system(paste(system_command,
                 paste(shQuote(path.expand(files), "cmd"),collapse = " "),
                 " &"))
)

locator(1)


####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f mins\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
