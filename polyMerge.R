# clean up environment
rm(list = ls())

# load packages
library(rgdal)
library(raster)
library(velox)
library(gdalUtils)
library(sf)
library(rgeos)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# import polygons
forestPlots <- readOGR(dsn = ".", layer = "forestPlots3Ha", encoding = "UTF-8", use_iconv = TRUE)
forestPlots$area <- area(forestPlots)
forestPlots$id <- paste(c(1:nrow(forestPlots@data)), 'id', sep = "")

###############################################################
# merging small plots algo
###############################################################
start_time <- Sys.time()

# size threshold (m²)
threshold <- 200
# list of smallPlots
smallPlots <- forestPlots[forestPlots$area <= threshold, ]
# matrix of polygones links
touch <- gTouches(forestPlots, byid = TRUE, returnDense=TRUE)
rownames(touch) <- forestPlots$id
colnames(touch) <- forestPlots$id

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
  t <- try(rgeos::gIntersection(forestPlots[forestPlots$id == i,], forestPlots[forestPlots$id %in% c(a$nei),], byid = TRUE))
  if("try-error" %in% class(t)){
    print('gIntersection Bugue --> impossible to merge')
    rfsl <- c(rfsl, iSmallPlot) # [r]emove [f]rom [s]mallPlots [l]ist because of a geometry error
    geomBugue <- c(geomBugue, i) # save all plots with geometry problems
  } else {
    lines <- rgeos::gIntersection(forestPlots[forestPlots$id == i,], forestPlots[forestPlots$id %in% c(a$nei),], byid = TRUE)
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
        a[a$nei == j, 'type'] <- class(rgeos::gIntersection(forestPlots[forestPlots$id == i,], forestPlots[forestPlots$id %in% j,], byid = TRUE))[1]
      }
      # remove SpatialPoints from "a" dataframe
      a <- a[a$type == "SpatialLines",]
      a$type <- NULL
      # reidentify intersecting lines
      lines <- rgeos::gIntersection(forestPlots[forestPlots$id == i,], forestPlots[forestPlots$id %in% c(a$nei),], byid = TRUE)
      print('SpatialCollections --> merge polygons sharing more than a point (i.e. a line)')
    }

    # calculate length of intersecting lines
    l_lines <- sp::SpatialLinesLengths(lines)
    # plot(forestPlots[forestPlots$id %in% c(i, c(a$nei)),])
    # plot(lines, add = TRUE, col = 1 + 1:length(lines), lwd = 5)

    # neighbours size
    a <- cbind(a, l_lines)
    a <- merge(a, data.frame(forestPlots[, c('area', 'id')]), by.x = 'nei', by.y = 'id')

    # add, one by one, neighbours with longest common boundary untill the size
    # threshold is reached
    a <- a[order(-a$l_lines),] # sort by boundary length -> longest first
    iArea <- data.frame(forestPlots[forestPlots$id == i, 'area'])[1,1]
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
    uni <- aggregate(forestPlots[forestPlots$id %in% c(i, ptj),], dissolve=TRUE)
    ptji <- c(ptji, a[1:nbNei, 'nei'], iSmallPlot) # ptj + iSmallPlot

    # save attribute of biggest polygon in the UNION
    a <- rbind(a[a$nei %in% ptj,], c(i, TRUE, 0, iArea))
    a$area <- as.numeric(a$area)
    a$l_lines <- as.numeric(a$l_lines)
    biggest <- a[a$area == max(a$area), 'nei']
    attr <- data.frame(forestPlots[forestPlots$id == biggest, ])

    # remove the polygons that were joined
    forestPlots <- forestPlots[!(forestPlots$id %in% c(i, ptj)), ]
    # include back these 2 polygons as 1 merged one
    forestPlots <- bind(forestPlots, uni)
    # assign attribute of biggest polygon to the new merged polygon
    forestPlots[nrow(forestPlots),] <- attr
    # correct area (daughter + mother)
    forestPlots[nrow(forestPlots), 'area'] <- area(forestPlots[forestPlots$id == biggest, ])

    # create new smallPlots list
    smallPlots <- forestPlots[forestPlots$area <= threshold, ]
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
    paste(unique(a$plot))
    a <- a[a$plot == TRUE, ]

    if (any(a$nei %in% ptji)){
      touch <- gTouches(forestPlots, byid = TRUE, returnDense=TRUE)
      rownames(touch) <- forestPlots$id
      colnames(touch) <- forestPlots$id
      ptji <- c()
    }
  }
  print(paste(nrow(smallPlots), 'small plots left, among which', nrow(smallTouch),'have neighbours to be merged with', '( i =', iSmallPlot, ')'))
}

end_time <- Sys.time()
end_time - start_time

#--> estimation du temps complet
# (39mn pour 580 smallPlot < 10 m2)
# (3.96h pour +- 3000 smallPlot < 2500 m2)



#--> comparer nb de plot avec forestPlots --> moins de small plots ---------------------------> ok
#--> comparer hist(area()) --> moins de small plots ------------------------------------------> ok
#--> comparer surface totale -----------------------------------------------------------------> ok
#--> enregistrer comme shp & comprarer parcellaire forestPlots1 vs forestPlots sous Qgis -----> semble ok
#    (notamment spatialcollection) -----------------------------------------------------------> semble ok
#--> vérifier si apres algo il reste encore des parcelles (avec neighbours) < threshold ------> ok
# reste les 'SpatialPoints', 'geomBugue' et les isolés (i.e.sans voisins)


---> supprimer les SmallPlots restants (les 'SpatialPoints', 'geomBugue' et les isolés (i.e.sans voisins)) ?
