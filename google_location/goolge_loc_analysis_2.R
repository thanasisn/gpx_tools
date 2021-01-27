#!/usr/bin/env Rscript

#'
#' Plot tracks from google, polar and other gps files
#' - Google data have been exported with google_loc_export.R
#' - Monthly pdf file with one plot per day
#' - Depending on the data map servers may get angry with you
#'


####    Set environment    ####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()
# if(!interactive())pdf(file=sub("\\.R$",".pdf",Script.Name))
sink(file=sub("\\.R$",".out",Script.Name,),split=TRUE)

library(stringr)
library(plotKML)
library(OpenStreetMap)
library(rgdal)
library(data.table)

## Input paths
google_dir    <- "~/DATA_RAW/Other/GLH/"
gpx_dir       <- "~/GISdata/"
gpx_pol       <- "~/Documents/Running/Polar"
## Output paths
intersect_pdf <- "~/DATA/Other/Intersect_"
glh_pdf       <- "~/DATA/Other/Glh_only_"
gpx_pdf       <- "~/DATA/Other/Gpx_only_"
locations_pdf <- "~/DATA/Other/Locations_"


## Controls
PLOT_COMMOM <- FALSE
PLOT_GOOGLE <- FALSE
PLOT_GPX    <- FALSE

PLOT_COMMOM <- TRUE
PLOT_GOOGLE <- TRUE
PLOT_GPX    <- TRUE


## Get all available files
glh_files <- list.files( path       = google_dir,
                         pattern    = ".*.Rds",
                         recursive  = T,
                         full.names = T)

gpx_files <- list.files( path        = gpx_dir,
                         pattern     = ".*.gpx",
                         recursive   = T,
                         ignore.case = T,
                         full.names  = T)

run_files <- list.files( path        = gpx_pol,
                         pattern     = ".*[0-9].gpx$",
                         recursive   = T,
                         ignore.case = T,
                         full.names  = T  )

## Just get the dates from file names
glh_dates   <- str_extract(basename(glh_files), "[0-9]{4}-[0-9]{2}-[0-9]{2}" )
gpx_dates   <- str_extract(basename(gpx_files), "[0-9]{4}-[0-9]{2}-[0-9]{2}" )
run_dates   <- paste0("20", str_extract(basename(run_files), "[0-9]{2}[0-9]{2}[0-9]{2}" ))

run_dates_p <- gsub('^([0-9]{4})(.+)$',          '\\1-\\2', run_dates)
run_dates_p <- gsub('^([0-9]{4}-[0-9]{2})(.+)$', '\\1-\\2', run_dates_p)

datelist    <- sort(unique(c(run_dates_p, glh_dates, gpx_dates)))


## Names and color dictionaries
sources <- c( "Other", "Google", "Polar" )
colorsl <- c( "blue" , "green",  "red"   )


## Plot by month
comb    <- expand.grid( MONTH = 1:12,
                        YEAR  = year(min(datelist)):year(max(datelist)) )
# comb <- expand.grid( MONTH = 1:12,
#                      YEAR  = 2019:year(max(datelist)) )



## load and plot data for each comb
act_id <- 1
for (aa in 1:nrow(comb)) {
    YEAR     <- comb[aa,2]
    MONTH    <- comb[aa,1]
    ## get months dates
    mrange   <- seq.Date(as.Date(paste0(YEAR,"-",MONTH,"-","01")), by = "month", length.out = 2)
    m_dates  <- seq.Date(mrange[1], mrange[2] - 1, by = "day")
    pdf_file <- paste0(locations_pdf, YEAR, "-", sprintf("%02d",MONTH),".pdf")

    ##TODO check the mtimes of input files against the pdf mtime to skip reploting existing

    ## plot all days of the month
    pdf(file = pdf_file, width = 8, height = 8)
    for ( md  in m_dates ) {
        day     <- as.Date(md, origin = "1970-01-01")
        day_str <- paste(day)
        cat(paste("\n",day_str,"\n"))

        HAVE_GOOGLE <- FALSE
        HAVE_POLAR  <- FALSE
        HAVE_GPX    <- FALSE

        gather <- list()
        lix    <- 0

        ## have google location
        if (any(grepl(day_str, glh_dates))) {
            ## get daily google file
            glh_fl <- list.files(path       = google_dir,
                                 pattern    = day_str,
                                 recursive  = T,
                                 full.names = T)
            cat(paste(glh_fl),"\n")
            stopifnot(length(glh_fl) == 1)
            ## read google history
            glh_points <- readRDS(glh_fl)
            glh_points <- glh_points[ , .(Date,Long,Lat,altitude,verticalAccuracy,main_activity) ]
            # setorder(glh_points, Date)
            glh_points$source <- "Google"
            glh_points$act_id <- act_id
            act_id <- act_id + 1
            lix    <- lix    + 1
            HAVE_GOOGLE <- TRUE
            gather[[lix]] <- glh_points
        }

        ## have polar run
        if (any(grepl(day_str, run_dates_p))) {
            ## get polar files
            pl_date <- gsub("-", "", gsub("^[0-9]{2}" ,"", day_str ))
            plr_fls <- list.files(path       = gpx_pol,
                                  pattern    = paste0("^",pl_date,"[0-9]*.gpx$"),
                                  full.names = T,
                                  recursive  = T)
            cat(paste(plr_fls),"\n")
            ## read polar gpx files
            if (length(plr_fls) > 0) {
                plr_points <- data.table()
                for (afil in plr_fls) {
                    gpx_tmp    <- readGPX(gpx.file = afil)$tracks[[1]][[1]]
                    gpx_tmp    <- gpx_tmp[ abs(gpx_tmp$lon) < 179.999, ]
                    gpx_tmp    <- gpx_tmp[ abs(gpx_tmp$lat) <  89.999, ]
                    plr_points <- plyr::rbind.fill(gpx_tmp, plr_points)
                    plr_points$act_id <- act_id
                    act_id <- act_id + 1
                }
                plr_points$Date <- as.POSIXct(strptime( plr_points$time, "%Y-%m-%dT%H:%M:%SZ" ))
                plr_points$time <- NULL
                plr_points$source <- "Polar"
                # setorder(plr_points, Date)
                names(plr_points)[names(plr_points) == "lon"] <- "Long"
                names(plr_points)[names(plr_points) == "lat"] <- "Lat"
                lix    <- lix    + 1
                HAVE_POLAR <- TRUE
                gather[[lix]] <- plr_points
            }
        }

        ## have other gpx files
        if (any(grepl(day_str, gpx_dates))) {
            ## get gpx file
            gpx_fls <- list.files(path       = gpx_dir,
                                  pattern    = day_str,
                                  full.names = T,
                                  recursive  = T)
            cat(paste(gpx_fls),"\n")
            ## read all gpx files
            gpx_points <- data.table()
            for (afil in gpx_fls) {
                gpx_tmp    <- readGPX(gpx.file = afil)$tracks[[1]][[1]]
                if (is.null( gpx_tmp ) || nrow(gpx_tmp) == 0) {
                    cat(paste("No points, SKIP\n"))
                    HAVE_GPX <- FALSE
                } else {
                    gpx_tmp    <- gpx_tmp[ abs(gpx_tmp$lon) < 179.999, ]
                    gpx_tmp    <- gpx_tmp[ abs(gpx_tmp$lat) <  89.999, ]
                    gpx_points <- plyr::rbind.fill(gpx_tmp, gpx_points)
                    gpx_points$act_id <- act_id
                    act_id <- act_id + 1
                    HAVE_GPX <- TRUE
                }
            }
            ## destroy fractional seconds
            gpx_points$time <- gsub("\\.[0-9]+" ,"",gpx_points$time )
            gpx_points$Date <- as.POSIXct(strptime( gpx_points$time, "%Y-%m-%dT%H:%M:%SZ" ))
            gpx_points$time <- NULL
            ## ordering may hide some problems with dates
            ## or make the plot jump with missing dates
            # setorder(gpx_points, Date)
            names(gpx_points)[names(gpx_points) == "lon"] <- "Long"
            names(gpx_points)[names(gpx_points) == "lat"] <- "Lat"
            gpx_points$source <- "Other"
            lix    <- lix    + 1
            gather[[lix]] <- gpx_points
        }
        ## skip empty days
        if (lix == 0 ) next()
        ## whate have to do
        if (lix == 1 ) {
            allpoints <- gather[[1]]
        }
        if (lix == 2 ) {
            allpoints <- merge( gather[[1]], gather[[2]], all = TRUE )
        }
        if (lix == 3 ) {
            allpoints <- merge( gather[[1]], gather[[2]], all = TRUE )
            allpoints <- merge( allpoints,   gather[[3]], all = TRUE )
        }
        ## something is wrong
        if (lix > 3) stop("Have to code something more")

        par(mar = c(1, 1, 1, 1))

        ## prepare data for plot
        setorder(allpoints, Date)
        coordinates(allpoints) <- ~ Long + Lat
        proj4string(allpoints) <- CRS("+init=epsg:4326")

        ## try to set a relative good box around the data
        osm   <- as.data.frame(spTransform(allpoints, osm()))
        lat   <- range(allpoints$Lat  )
        lon   <- range(allpoints$Long )

        # latex <- abs(diff(lat)) * 0.15 + 0.005
        # lonex <- abs(diff(lon)) * 0.15 + 0.005

        max_lat_edge <- 0.7
        max_lon_edge <- 0.7

        latex_c <- 0.005
        lonex_c <- 0.005
        latex_f <- 0.20
        lonex_f <- 0.20

        cat(paste(signif(c(lat, lon))),"\n")

        ## extend limits
        latex <- abs(diff(lat)) * latex_f + latex_c
        if (latex > max_lat_edge) latex <- max_lat_edge
        lat   <- c(lat[1] - latex, lat[2] + latex  )
        lonex <- abs(diff(lon)) * lonex_f + lonex_c
        if (lonex > max_lon_edge) lonex <- max_lon_edge
        lon   <- c(lon[1] - lonex, lon[2] + lonex  )

        cat(paste(signif(c(lat, lon, latex, lonex))),"\n")

        ## get map
        ## zoom = 10 and NULL is good 12 is extreme
        map <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom = NULL,'osm')
        plot(map)

        ## plot all data on map
        for (ty  in unique(osm$source)) {
            tmp <- osm[ osm$source == ty, ]
            ## plot each line not good for bad date sorting
            for (tyii  in unique(tmp$act_id)) {
                tmpl <- tmp[ tmp$act_id == tyii, ]
                lines( tmpl$Long, tmpl$Lat, col = colorsl[sources == ty], lwd = 1.3 )
            }
            ## plot point don't care about dates
            points(tmp$Long, tmp$Lat, col = colorsl[sources == ty], pch = 19, cex = 0.5 )
        }
        title(main = paste(day_str), cex.main = .8)
    }
    dev.off()

    cat(paste("Ready",pdf_file),"\n\n")
}



####_ END _####
tac = Sys.time()
cat(sprintf("\n%s H:%s U:%s S:%s T:%f mins\n\n",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")))
