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

# load shapefiles
savoie <- readOGR(dsn = "W:/BRGM/BD Charm-50/73", layer = "GEO050K_HARM_073_S_FGEOL_CGH_2154", encoding = "UTF-8", use_iconv = TRUE)
hsavoie <- readOGR(dsn = "W:/BRGM/BD Charm-50/74", layer = "GEO050K_HARM_074_S_FGEOL_CGH_2154", encoding = "UTF-8", use_iconv = TRUE)

# bind the two regions
shs <- bind(savoie, hsavoie)
plot(shs, col = shs$CODE, border = shs$CODE)

# load a mask of the study area
PNR <- readOGR(dsn = "Z:/Private/PNR Bauges/Sans_Trou", layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)
plot(PNR, add = TRUE, col = "green")

# intersect
shs <- spTransform(shs, crs(PNR)) # change shs projection
geol <- intersect(shs, PNR)
plot(geol, col = geol$CODE, border = geol$CODE)

# save new shapefile
shapefile(geol, filename = 'geol')
