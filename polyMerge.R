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
forestPlots <- readOGR(dsn = ".", layer = "superID_1", encoding = "UTF-8", use_iconv = TRUE)
forestPlots$area <- area(forestPlots)
forestPlots$id <- paste(c(1:nrow(forestPlots@data)), 'id', sep = "")

###############################################################
# merging small plots algo
###############################################################
start_time <- Sys.time()

# size threshold (m²)
threshold <- 10
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
while(sum(smallTouch == TRUE) > 0){

  a <- data.frame(touch[i,])
  a$nei <- rownames(a)
  colnames(a) <- c('plot', 'nei')
  paste(unique(a$plot))

  if(length(unique(a$plot)) == 1){
    # no neighbours
  } else {
    a <- a[a$plot == TRUE, ]
    lines <- rgeos::gIntersection(forestPlots[forestPlots$id == i,], forestPlots[forestPlots$id %in% c(a$nei),], byid = TRUE)
    if (class(lines)[1] == "SpatialPoints"){
      # remove definitely from the smallPlots List
      rfsl <- c(rfsl, iSmallPlot) # [r]emove [f]rom [s]mallPlots [l]ist
      print('SpatialPoints --> impossible to merge')
    } else if(class(lines)[1] == "SpatialCollections") {
      rfsl <- c(rfsl, iSmallPlot) # [r]emove [f]rom [s]mallPlots [l]ist
      # lines <- lines@lineobj
      # find a way to identify wich intersection  is/are SpatialPoints and remove
      # it from 'a' dataframe
      print('SpatialCollections --> impossible to merge')
    } else {
      l_lines <- sp::SpatialLinesLengths(lines)
      # plot(forestPlots[forestPlots$id %in% c(i, c(a$nei)),])
      # plot(lines, add = TRUE, col = 1 + 1:length(lines), lwd = 5)

      a <- cbind(a, l_lines)

      # neighbours size
      a <- merge(a, data.frame(forestPlots[, c('area', 'id')]), by.x = 'nei', by.y = 'id')

      # add neighbours untill threshold is reached
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
      # list of plots to join
      ptj <- a[1:nbNei, 'nei'] # [p]lots [t]o [j]oin
      print(paste('merging', length(ptj)+1, 'plots together'))
      uni <- aggregate(forestPlots[forestPlots$id %in% c(i, ptj),], dissolve=TRUE)
      ptji <- c(ptji, a[1:nbNei, 'nei'], iSmallPlot) # ptj + iSmallPlot
      # no need to calculate a Touch matrix if the neighbours are touching a
      # plot that was not merged because --> SpatialPoints or SpatialCollections
      ptji <- ptji[!(ptji %in% rfsl)]

      # save attribute of biggest polygon in the UNION
      if(max(a[a$nei %in% ptj, 'area']) > iArea){
        biggest <- a[a$area == max(a[a$nei %in% ptj, 'area']), 'nei']
        attr <- data.frame(forestPlots[forestPlots$id == biggest, ])
      } else {
        attr <- data.frame(forestPlots[forestPlots$id == i, ])
      }

      # remove the polygons that were joined
      forestPlots <- forestPlots[!(forestPlots$id %in% c(i, ptj)), ]
      # include back these 2 polygons as 1 merged one
      forestPlots <- bind(forestPlots, uni)
      # assign attribute of biggest polygon to the new merged polygon
      forestPlots[nrow(forestPlots),] <- attr
      # correct area (daughter + mother)
      forestPlots[nrow(forestPlots), 'area'] <- area(forestPlots[forestPlots$id == biggest, ])

    }
  }

  # create new smallPlots list
  smallPlots <- forestPlots[forestPlots$area <= threshold, ]
  # remove plots that could not be merged because their union with their
  # neighbours is aa SpatialPoints or a SpatialCollections
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
  iSmallPlot <- rownames(smallTouch)[round(runif(1, min = 1, max = nrow(smallTouch)))]
  # i <- smallPlot with neighbours
  i <- iSmallPlot

  # if the new smallPlot (i.e. iSmallPlot) is not touching any of the joined plots
  # (i.e. ptj + i) no need to calculate another touch matrix
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

  print(paste(nrow(smallPlots), 'small plots left, among which', nrow(smallTouch),'have neighbours to be merged with', '( i =', iSmallPlot, ')'))

}


end_time <- Sys.time()

end_time - start_time
