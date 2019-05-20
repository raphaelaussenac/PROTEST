###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
library(rgdal)
# install ggmap from github
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
# library(ggmap)
# Set your API Key
# ggmap::register_google(key = "!!!!!!!!!!!!!!!!!!!!!!!")
# library(ggplot2)
library(raster)
library(velox)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")


###############################################################
# google map background
###############################################################

# lat <- c(45.45, 45.9)
# lon <- c(5.9, 6.45)
# map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 10, maptype = "satellite", source = "google")
# bckgrd <- ggmap(map)+
#   scale_x_continuous(limits = lon, expand = c(0, 0)) +
#   scale_y_continuous(limits = lat, expand = c(0, 0))
# bckgrd


###############################################################
# MNT
###############################################################

MNT <- raster("MNT_all_5m.tif")
plot(MNT, col=colorRampPalette(c("black", "white"))(255))

# convert raster into a df and plot with ggplot
#MNT <- as.data.frame(MNT, xy = TRUE)
#MNT <- MNT[1:20658916,]
#ggplot() +
#  geom_raster(data = MNT, aes(x = x, y = y, fill = MNT_all_5m))


###############################################################
# BD foret
###############################################################

BDforet <- readOGR(dsn = ".", layer = "BD_Foret_V2_PNR_2014", encoding = "UTF-8", use_iconv = TRUE)
plot(BDforet, col = BDforet$CODE_TFV, add = TRUE)

# convert shp into a df and plot with ggplot
#BDforet_df <- as(BDforet, "data.frame")
#BDforet_df <- fortify(BDforet)
#ggplot() +
#  geom_polygon(data = BDforet_df,
#               aes(x = long, y = lat, group = group, fill = "blue"),
#               color = 'black', size = .2)+


###############################################################
# intersection
###############################################################

# # plot
# plot(BDforet[BDforet$CODE_TFV == "FF1-10-10",])
# plot(MNT, col=colorRampPalette(c("black", "white"))(255), xlim = c(928800, 929800), ylim = c(6519500, 6520500), alpha = 0.5, interpolate = FALSE, add = TRUE)
#
# # intersect
# inter <- intersect(MNT, BDforet[BDforet$CODE_TFV == "FF1-10-10",])
# #plot(inter, col=colorRampPalette(c("black", "white"))(255), asp = 1)
#
# # mask
# ma <- mask(MNT, BDforet[BDforet$CODE_TFV == "FF1-10-10",])
# plot(ma)
# mean(ma@data@values, na.rm = TRUE)


###############################################################
# extract mean altitude values for each plot
###############################################################

###################################
# extract with raster package
# ex <- extract(MNT, BDforet, fun = mean, sp = T) # lasts 2h00, try velox package
# names(ex)[names(ex) == "MNT_all_5m"] <- "mean_alt"
# hist(ex$mean_alt)
# plot(ex, col = ex$CODE_TFV)
# plot(ex, col = ex$mean_alt)

###################################
# extract with velox package
# convert "Raster" into "VeloxRaster"
vraster <- velox(MNT)

# extract
start_time <- Sys.time()
velox_ex <- vraster$extract(sp = BDforet, fun = mean, small = TRUE)
end_time <- Sys.time()
end_time - start_time

# convert into df
velox_ex_df <- data.frame(velox_ex)
colnames(velox_ex_df) <- "mean_alt"

# integrate into the shp file
BDforet@data <- cbind(BDforet@data, velox_ex_df)


###############################################################
# save new shp with altitudes
###############################################################

shapefile(BDforet, filename = 'BDforet_alti_velox')


###############################################################
# combine shapefile and google background
###############################################################
#
# BDforet <- spTransform(BDforet, CRS("+proj=longlat +datum=WGS84"))
# BDforet <- fortify(BDforet)
#
# testMAP <- bckgrd +
#   geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2, color='black', data = BDforet, alpha = 0.25) +
# testMAP
