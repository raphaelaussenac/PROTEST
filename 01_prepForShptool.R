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
# 6 - run the "extract" script on this new shp
#
###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = setdiff(ls(), "user"))

# load packages
# library(rgdal)
# library(raster)
# library(rgeos)

# set work directory
setwd(user$WorkingDir)

# load BDforet shapefile
BDforet <- rgdal::readOGR(dsn = paste0(user$NetworkProtestDir, "T1/Donnees_SIG/BD_Foret"), layer = "BD_Foret_V2_PNRfilled_Foret_2014", encoding = "UTF-8", use_iconv = TRUE)

###############################################################
# create new shapefile with only ID
###############################################################

# remove useless column
BDforet <- BDforet[, c('CODE_TFV', 'INSEE_DEP')]

# create unique ID for each polygon
BDforet$BDid <- paste(c(1:nrow(BDforet@data)), 'BDid', sep = "")

# save the attribute table with the unqiue ID and the rest of the data
write.table(BDforet@data, file = "./data/BDid.csv", sep = "\t", row.names = FALSE)

# remove all other colums from the shapefile attribute table
BDforet@data <- data.frame(BDforet$BDid)
colnames(BDforet@data) <- "BDid"

# save new shapefile
raster::shapefile(BDforet, filename = './data/BDid', overwrite = T)
