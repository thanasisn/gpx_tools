#!/usr/bin/env Rscript


####_ Set environment _####
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")

args <- commandArgs(trailingOnly=TRUE)

##TEST
# args <- c("2.gpx", "1.gpx")


## test if there is at least one argument: if not, return an error
if (length(args)==0) {
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
}


## check files
files <- c()
for (aa in args) {
    if (file.exists(aa)) {
        files <- c(files,aa)
    }
}

if (length( files ) == 0) {
    cat(paste("No file exist\n"))
} else {
    cat(paste("Found",length(files),"files\n"))
    cat(paste(files,"\n"))
}



## GPX files analysis ##
library(sf)
library(data.table)

wecare <- c("ele","time","file")
gather <- data.table()
for (af in files) {
    tracks <- read_sf(af, layer = "track_points")
    tracks$file <- af
    trkcco <- st_coordinates(tracks)
    tracks <- data.table(tracks)
    tracks <- tracks[, ..wecare ]
    tracks$X <- unlist(trkcco[,1])
    tracks$Y <- unlist(trkcco[,2])

    gather <- rbind(gather, tracks)
}


if (all(is.na(gather$time))) {
    NO_TIMES <- TRUE
} else {
    NO_TIMES <- FALSE
}


##TODO
## Add map?


{
X11()

if (!NO_TIMES) {
    layout(matrix(c(1,1,2), 3, 1, byrow = TRUE))
}

## plot points
par(mar=c(2.5,2,0.3,0.3))
plot(1, type="n", xlab="", ylab="", xlim=range(gather$X), ylim=range(gather$Y),cex.axis = 0.7)
col = 2
fnam <- c()
lcol <- c()
for (af in unique(gather$file)) {
    temp <- gather[file==af]
    lines(temp$X, temp$Y, col=col, lwd = 2)

    lcol <- c(lcol, col)
    fnam <- c(fnam, basename(af))

    col  <- col+1
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


plot(1, xaxt="n",yaxt="n", type="n", xlab="", ylab="", xlim=range(gather$time,na.rm = T), ylim=c(0.8,length(unique(gather$file))))
col = 2
fnam <- c()
lcol <- c()
for (af in unique(gather$file)) {
    temp <- gather[file==af]
    temp$col <- col-1
    points(temp$time, temp$col, col=col, lwd = 2)

    lcol <- c(lcol, col)
    fnam <- c(fnam, basename(af))

    col  <- col+1
}
# legend(min(gather$X),max(gather$Y), legend = fnam, pch=19, col = lcol,
#        cex = 0.6, ncol = 2, bty="n")
legend("bottomleft", legend = fnam, pch=19, col = lcol,
       cex = 0.6, ncol = 2, bty="n")
axis.POSIXct(1, gather$time, cex.axis = 0.7)

}

}
locator(1)


####_ END _####
