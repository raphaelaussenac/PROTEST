# clean up environment
rm(list = setdiff(ls(), "user"))

# load packages
library(rgdal)
library(raster)
library(gdalUtils)
library(sf)
library(rgeos)

# set work directory
setwd(user$WorkingDir)

# import polygons

# polygons exported from shapeTools
forestStands <- rgdal::readOGR(dsn = "./data", layer = "BDid_1", encoding = "UTF-8", use_iconv = TRUE)
forestStands@proj4string <- sp::CRS("+init=epsg:2154")
forestStands$area <- area(forestStands)
hist(forestStands$area)
sum(forestStands$area)
forestStands$id <- paste(c(1:nrow(forestStands@data)), 'id', sep = "")

###############################################################
# merging small plots algo
###############################################################
start_time <- Sys.time()

# size threshold (m²)
threshold <- 5000
# list of smallPlots
smallPlots <- forestStands[forestStands$area <= threshold, ]
# matrix of polygones links
touch <- gTouches(forestStands, byid = TRUE, returnDense=TRUE)
rownames(touch) <- forestStands$id
colnames(touch) <- forestStands$id

# chek whether smallPlots have neighbours (conditions for main while loop)
smallTouch <- touch[smallPlots$id,]
# 1 - transform F -> 0 / T -> 1
smallTouch[smallTouch == FALSE] <- 0
smallTouch[smallTouch == TRUE] <- 1
# 2 - sum lines
smallTouch <- data.frame(smallTouch)
smallTouch$sum <- apply(smallTouch, 1, sum)
# 3 - keep only smallPlots with neighbours (i.e. those for which sum > 0)
smallTouch <- smallTouch[smallTouch$sum > 0,]
# 4- randomly select a plot with a nei (to be --> "i")
iSmallPlot <- rownames(smallTouch)[round(runif(1, min = 1, max = nrow(smallTouch)))]
# i <- smallPlot with neighbours
i <- iSmallPlot

ptji <- c() # [p]lots [t]o [j]oin + iSmallPlot (see below)
rfsl <- c() # [r]emove [f]rom [s]mallPlots [l]ist (see below)
geomBugue <- c()
spatialPointsPlots <- c()
while(nrow(smallTouch) > 0){

  a <- data.frame(touch[i,])
  a$nei <- rownames(a)
  colnames(a) <- c('plot', 'nei')
  a <- a[a$plot == TRUE, ]

  # identify intersecting lines
  # + manage some rare geometry problems created by gIntersection (--> create subsubgeometry)
  t <- try(rgeos::gIntersection(forestStands[forestStands$id == i,], forestStands[forestStands$id %in% c(a$nei),], byid = TRUE))
  if("try-error" %in% class(t)){
    print('gIntersection Bugue --> impossible to merge')
    rfsl <- c(rfsl, iSmallPlot) # [r]emove [f]rom [s]mallPlots [l]ist because of a geometry error
    geomBugue <- c(geomBugue, i) # save all plots with geometry problems
  } else {
    lines <- rgeos::gIntersection(forestStands[forestStands$id == i,], forestStands[forestStands$id %in% c(a$nei),], byid = TRUE)
  }

  # do nothing if intersection is a point
  if (class(lines)[1] == "SpatialPoints" | i %in% rfsl){
    # remove definitely from the smallPlots List
    if (i %in% rfsl){
      # no need to add in rfsl --> already in rfsl if in geomBugue
    } else {
      rfsl <- c(rfsl, iSmallPlot) # [r]emove [f]rom [s]mallPlots [l]ist
      spatialPointsPlots <- c(spatialPointsPlots, iSmallPlot) # keep track of spatialpoints plots
    }
    print('SpatialPoints --> impossible to merge')
  } else { # if intersection is SpatialLines or SpatialCollections
    if(class(lines)[1] == "SpatialCollections"){ # if intersection is SpatialCollections
      # identify wich intersection is/are SpatialPoints / SpatialLines in the SpatialCollections
      a$type <- NA
      for (j in a$nei){
        a[a$nei == j, 'type'] <- class(rgeos::gIntersection(forestStands[forestStands$id == i,], forestStands[forestStands$id %in% j,], byid = TRUE))[1]
      }
      # remove SpatialPoints from "a" dataframe
      a <- a[a$type == "SpatialLines",]
      a$type <- NULL
      # reidentify intersecting lines
      lines <- rgeos::gIntersection(forestStands[forestStands$id == i,], forestStands[forestStands$id %in% c(a$nei),], byid = TRUE)
      print('SpatialCollections --> merge polygons sharing more than a point (i.e. a line)')
    }

    # calculate length of intersecting lines
    l_lines <- sp::SpatialLinesLengths(lines)
    # plot(forestStands[forestStands$id %in% c(i, c(a$nei)),])
    # plot(lines, add = TRUE, col = 1 + 1:length(lines), lwd = 5)

    # neighbours size
    a <- cbind(a, l_lines)
    a <- merge(a, data.frame(forestStands[, c('area', 'id')]), by.x = 'nei', by.y = 'id')

    # add, one by one, neighbours with longest common boundary untill the size
    # threshold is reached
    a <- a[order(-a$l_lines),] # sort by boundary length -> longest first
    iArea <- data.frame(forestStands[forestStands$id == i, 'area'])[1,1]
    nbNeimax <- nrow(a)
    iAreaMerge <- iArea
    counter <- 1
    nbNei <- 0
    while (iAreaMerge < threshold && nbNei < nbNeimax){
      iAreaMerge <- iArea + sum(a[1:counter,'area'])
      nbNei <- counter
      counter <- counter + 1
     }

    # list of plots to join / merge
    ptj <- a[1:nbNei, 'nei'] # [p]lots [t]o [j]oin
    print(paste('merging', length(ptj)+1, 'plots together'))
    uni <- aggregate(forestStands[forestStands$id %in% c(i, ptj),], dissolve=TRUE)
    ptji <- c(ptji, a[1:nbNei, 'nei'], iSmallPlot) # ptj + iSmallPlot

    # save attribute of biggest polygon in the UNION
    a <- rbind(a[a$nei %in% ptj,], c(i, TRUE, 0, iArea))
    a$area <- as.numeric(a$area)
    a$l_lines <- as.numeric(a$l_lines)
    biggest <- a[a$area == max(a$area), 'nei']
    attr <- data.frame(forestStands[forestStands$id == biggest, ])

    # remove the polygons that were joined
    forestStands <- forestStands[!(forestStands$id %in% c(i, ptj)), ]
    # include back these 2 polygons as 1 merged one
    forestStands <- bind(forestStands, uni)
    # assign attribute of biggest polygon to the new merged polygon
    forestStands[nrow(forestStands),] <- attr
    # correct area (daughter + mother)
    forestStands[nrow(forestStands), 'area'] <- area(forestStands[forestStands$id == biggest, ])

    # create new smallPlots list
    smallPlots <- forestStands[forestStands$area <= threshold, ]
  }

  # remove plots that could not be merged because their union with their
  # neighbours is a SpatialPoint
  smallPlots <- smallPlots[!(smallPlots$id %in% rfsl),]

  # chek whether these new smallPlots have neighbours (conditions for main while loop)
  smallTouch <- touch[smallPlots$id,]
  # 1 - transform F -> 0 / T -> 1
  smallTouch[smallTouch == FALSE] <- 0
  smallTouch[smallTouch == TRUE] <- 1
  # 2 - sum lines
  smallTouch <- data.frame(smallTouch)
  smallTouch$sum <- apply(smallTouch, 1, sum)
  # 3 - keep only smallPlots with neighbours (i.e. those for which sum > 0)
  smallTouch <- smallTouch[smallTouch$sum > 0,]
  # 4- randomly select a plot with a nei (to be --> "i")
  if (nrow(smallTouch) > 0){
    iSmallPlot <- rownames(smallTouch)[round(runif(1, min = 1, max = nrow(smallTouch)))]
    # i <- smallPlot with neighbours
    i <- iSmallPlot

    # if the new smallPlot (i.e. iSmallPlot) is not touching any of the previously joined plots
    # (i.e. ptji) --> no need to calculate another touch matrix
    a <- data.frame(touch[i,])
    a$nei <- rownames(a)
    colnames(a) <- c('plot', 'nei')
    a <- a[a$plot == TRUE, ]

    if (any(a$nei %in% ptji)){
      rm(list = c('touch')) # otherwise memory error
      touch <- gTouches(forestStands, byid = TRUE, returnDense=TRUE)
      rownames(touch) <- forestStands$id
      colnames(touch) <- forestStands$id
      ptji <- c()
    }
  }
  print(paste(nrow(smallPlots), 'small plots left, among which', nrow(smallTouch),'have neighbours to be merged with', '( i =', iSmallPlot, ')'))
}

end_time <- Sys.time()
end_time - start_time

hist(forestStands$area)
sum(forestStands$area)

# time elapsed
# 5.450937 hours with threshold == 5000 m²
# Time difference of 5.651599 hours  with threshold == 5000 m²

shapefile(forestStands, filename = './data/forestStands3HaPolyMerge', overwrite = TRUE)


# comparison donne after extraction


# ###############################################################
# # compare surfaces before filters and check wether they represent
# # accurately the landscape (difference between surfaces measured
# # on input rasters and final plot should be reduced to a minimum)
# ###############################################################
# 
# # ownership ----------------------------------
# --> "Private"
# sum(area(forestStands[forestStands$owner == "Priv" & !is.na(forestStands$owner),])) / 10000
# [1] 31137.81   -- polymerge --> 30918.04
# 
# --> "Public"
# sum(area(forestStands[forestStands$owner == "Pub" & !is.na(forestStands$owner),])) / 10000
# [1] 21687.13   -- polymerge --> 21680.52
# 
# --> "NA"
# sum(area(forestStands[is.na(forestStands$owner),])) / 10000
# [1] 0.07500767   -- polymerge --> 0.01030308
# 
# # nonHarv ----------------------------------
# sum(area(forestStands[forestStands$nonHarv == 0,])) / 10000
# [1] 4230.202   -- polymerge --> 4206.674
# 
# sum(area(forestStands[forestStands$nonHarv == 1,])) / 10000
# [1] 48594.81   -- polymerge --> 48391.88
# 
# # dist ----------------------------------
# sum(area(forestStands[forestStands$dist == 0,])) / 10000
# [1] 23145.33   -- polymerge --> 23053.91
# 
# sum(area(forestStands[forestStands$dist == 1,])) / 10000
# [1] 12145.08   -- polymerge --> 12055.45
# 
# sum(area(forestStands[forestStands$dist == 2,])) / 10000
# [1] 17534.59   -- polymerge --> 17489.19
# 
# ############################################################### rasters
# # under QGIS
# # --> clip raster with polygon
# # --> polygoniser
# # --> save shp
# test <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "test03", encoding = "UTF-8", use_iconv = TRUE)
# 
# # ownership ----------------------------------
# sum(area(test[test$DN == 1,]))/10000
# [1] 31228.44
# 
# sum(area(test[test$DN == 2,]))/10000
# [1] 21597.92
# 
# # nonHarv ----------------------------------
# sum(area(test[test$DN == 1,])) / 10000
# [1] 4235.743
# 
# sum(area(test[test$DN == 0,])) / 10000
# [1] 48590.31
# 
# # dist ----------------------------------
# sum(area(test[test$DN == 0,])) / 10000
# [1] 23267.67
# 
# sum(area(test[test$DN == 1,])) / 10000
# [1] 12143.89
# 
# sum(area(test[test$DN == 2,])) / 10000
# [1] 17415.04
