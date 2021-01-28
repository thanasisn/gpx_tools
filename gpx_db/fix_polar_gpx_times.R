#!/usr/bin/env Rscript

#'
#' Find and fix a problem with Polar gpx files
#' The gpx files from Polar watch/software have mis-formated time stamps
#' This tries to find this and ather possible problems and fix it
#'


####_ Set environment _####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()

library(sf)
library(data.table)
library(myRtools)

##TODO include cases with bad data start at the beginning
## For now these file are skipped and corrected manual.
## If some of the first time stamps are correct this script can work

gpxpath <- "~/Documents/Running/Polar/Athan/2020/"

files <- list.files(path         = gpxpath,
                    pattern      = "gpx",
                    recursive    = TRUE,
                    include.dirs = FALSE,
                    ignore.case  = TRUE,
                    full.names   = TRUE )
## problems for later
files <- grep("18010401",files,invert = T, value = T)

for (af in files) {

    ## Just check file for time continuity
    temp <- read_sf(af, layer = "track_points")
    cat("\n")
    cat(paste(af),"\n")
    ## check date order
    if ( is.unsorted(temp$time) ) {
        cat(paste("Unsorted times in: ", af),"\n")
    } else {
        cat(paste("Times ok!"),"\n")
    }

    cat(paste("Unsorted:",sum(sort(temp$time)!=temp$time),"of",length(temp$time)),"\n")

    ## Print time steps in file
    # cat("Time steps")
    # print(table(diff(as.numeric(temp$time))))
    # plot(temp$time)

    ## Try to fix
    if ( is.unsorted(temp$time) ) {
        linn <- readLines(af)
        ## we assume each time is in one line only
        ## get lines id
        whil <- grep("<time>[-:0-9Z]+", linn)
        ## Find where track starts
        trkid <- grep("<trk>", linn)[1]
        ## get dates
        wstr <- grep("<time>[-:0-9Z]+", linn, value = T)
        wstr <- gsub("[ ]*<time>", "", wstr)
        wstr <- gsub("Z</time>", "", wstr)
        wpox <- as.POSIXct(strptime(wstr,"%FT%T",tz="UTC"))
        ## keep only track data
        sel  <- whil > trkid
        whil <- whil[sel]
        wstr <- wstr[sel]
        wpox <- wpox[sel]

        ## check dup dates
        # if( any(!duplicated(wpox)) ) {
        #     cat(paste("Duplicate dates found"))
        #     duu <- !duplicated(wpox)
        #
        #     test <- data.frame( Line   = whil[duu],
        #                 String = linn[whil[duu]],
        #                 Str = wstr[duu],
        #                 Date   = wpox[duu])
        #
        # }

        ## check frequency ##
        ## select a point to check dates
        mark <- 1
        markstp <- wpox[mark+1] - wpox[mark]
        markmed <- median(diff(as.numeric(temp$time)))

        if ( is.na(markmed) | is.na(markstp) | markmed != markstp ) {
            cat(paste("SKIP: ", af),"\n")
            cat(paste("not equal markmed != markstp"),"\n")
            ## try manual fix
            stop()
            next
        }

        check <- data.frame(
            Lin = whil,
            Str = wstr,
            Old = wpox,
            New = seq(wpox[1],  by = markmed, length.out = length(whil))
        )

        check$NewT <- strftime(check$New, format = "%FT%T")

        sum(check$NewT != check$Str)

        ## compare string not dates
        # check <- check[check$Old != check$New, ]
        check <- check[check$NewT != check$Str, ]
        check <- check[!is.na(check$Lin),]

        ## replace dates and hope for the best
        check$NewS <- strftime(check$New, format = "<time>%FT%TZ</time>")

        linn[check$Lin] <- check$NewS

        newfile <- paste0(af,".test.gpx")
        if ( file.exists(newfile) ) {
            cat(paste("Exist:",newfile))
        } else {
            writeLines(linn, newfile)
        }
    }
}

files <- list.files(path        = gpxpath,
                    pattern     = "gpx",
                    ignore.case = TRUE,
                    full.names  = TRUE )

cat(paste("\n---------------------------\n"))
cat(paste("Original files:", length(grep("origin",files, value = T))),"\n")
cat(paste("Test files:    ", length(grep("test",files, value = T))),"\n")



cat("\n-- END -- \n")
