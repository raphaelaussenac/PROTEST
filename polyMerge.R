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

# size threshold
threshold <- 10
# list of smallPlots
smallPlots <- forestPlots[forestPlots$area <= threshold, ]
# matrix of polygones links
touch <- gTouches(forestPlots, byid = TRUE, returnDense=TRUE)
rownames(touch) <- forestPlots$id
colnames(touch) <- forestPlots$id

# chek whether smallPlots have neighbours (conditions for main while loop)
smallTouch <- data.frame(touch[smallPlots$id,])

# and select the first plot with a nei to be --> "i"
if(sum(smallTouch == TRUE) > 0){
  counter <- 1
  while(sum(smallTouch[counter,] == TRUE) == 0){
    counter <- counter + 1
  }
}
iSmallPlot <- rownames(smallTouch[counter,])


# nei <- data.frame(i = NA, nei = NA)
# for (i in smallPlots$id){
#   a <- data.frame(touch[i,])
#   FT <- length(unique(a[,1])) # [F]ALSE [T]RUTH
#   if(FT == 2){
#     FT <- data.frame("i" = i, "nei" = "y")
#   } else {
#     FT <- data.frame("i" = i, "nei" = "n")
#   }
#   colnames(FT)
#   nei <- rbind(nei, FT)
# }
# nei <- nei[-1,]








while(sum(smallTouch == TRUE) > 0){

  touch <- gTouches(forestPlots, byid = TRUE, returnDense=TRUE)
  rownames(touch) <- forestPlots$id
  colnames(touch) <- forestPlots$id

  # i <- smallPlot with neighbours
  i <- iSmallPlot

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
      print('SpatialPoints')
    } else if(class(lines)[1] == "SpatialCollections") {
      lines <- lines@lineobj
      # find a way to identify wich intersection  is/are SpatialPoints and remove
      # it from 'a' dataframe
      print('SpatialCollections')
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
      ptj <- a[1:nbNei, 'nei']
      uni <- aggregate(forestPlots[forestPlots$id %in% c(i, ptj),], dissolve=TRUE)

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

  # chek whether these new smallPlots have neighbours (conditions for main while loop)
  smallTouch <- data.frame(touch[smallPlots$id,])

  # and select the first plot with a nei to be --> "i"
  if(sum(smallTouch == TRUE) > 0){
    counter <- 1
    while(sum(smallTouch[counter,] == TRUE) == 0){
      counter <- counter + 1
    }
  }
  iSmallPlot <- rownames(smallTouch[counter,])

  # nei <- data.frame(i = NA, nei = NA)
  # for (i in smallPlots$id){
  #   a <- data.frame(touch[i,])
  #   FT <- length(unique(a[,1])) # [F]ALSE [T]RUTH
  #   if(FT == 2){
  #     FT <- data.frame("i" = i, "nei" = "y")
  #   } else {
  #     FT <- data.frame("i" = i, "nei" = "n")
  #   }
  #   colnames(FT)
  #   nei <- rbind(nei, FT)
  # }
  # nei <- nei[-1,]


  print('et hop un tour de plus')

}


end_time <- Sys.time()

end_time - start_time
