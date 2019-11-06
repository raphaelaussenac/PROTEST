###############################################################
# initialisation
###############################################################
# clean up environment
rm(list = ls())

# load packages
library(rgdal)
library(maptools)
library(raster)

# load forest plots
forestPlots <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "forestPlots3Ha", encoding = "UTF-8", use_iconv = TRUE)
forestPlotsDf <- data.frame(forestPlots)
forestPlotsDfAgg <- aggregate(forestPlotsDf[, 'access'], list(forestPlotsDf$access), unique)
row.names(forestPlotsDfAgg) <- as.character(forestPlotsDfAgg$Group.1)

# join plots with same accessibility type
accJoin <- unionSpatialPolygons(forestPlots, forestPlots$access)

# put back attribute in spatialPolygon
accJoinAgg <- SpatialPolygonsDataFrame(accJoin, forestPlotsDfAgg)
colnames(accJoinAgg@data) <- c('access', 'x')
accJoinAgg@data$x <- NULL

#save
shapefile(accJoinAgg, filename = 'C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/accJoinAgg', overwrite = TRUE)
