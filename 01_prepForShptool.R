# README
#
# 1 - run this script to create a new BDforet shapefile with only ID
#     (and an attribute table with those ID and the rest of the data)
#
# 2 - split the plots in shptool
#
# 3 - import and merge the attribute table in shptool containning the unique
#     ID and the rest of the data (click "Join" and then click "Join plot data
#     on the type leve for selected layers")
#
# 4 - import the shp in R (click "Save...")
#
# 5 - a csv file is created --> make a copie of it before closing shptool
#
# 5 - run the "extract" script on this new shp
#
###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
library(rgdal)
library(raster)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# load BDforet shapefile
BDforet <- readOGR(dsn = "X:/ProjetsCommuns/PROTEST/T1/Donnees_SIG/BD_Foret", layer = "BD_Foret_V2_PNRfilled_Foret_2014", encoding = "UTF-8", use_iconv = TRUE)

###############################################################
# create new shapefile with only ID
###############################################################

# change column ID name to avoid conflict with shptools ID
colnames(BDforet@data)[colnames(BDforet@data) == 'ID'] <- 'IDD'

# create unique ID for each polygon
BDforet$SuperID <- c(1:nrow(BDforet@data))

# save the attribute table with the unqiue ID and the rest of the data
write.table(BDforet@data, file = "BDsuperID.csv", sep = "\t", row.names = FALSE)

# remove all other colums from the shapefile attribute table
BDforet@data <- data.frame(BDforet$SuperID)
colnames(BDforet@data) <- "superID"

# save new shapefile
shapefile(BDforet, filename = 'superID', overwrite = T)
