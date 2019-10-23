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
library(rgeos)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# load BDforet shapefile
BDforet <- readOGR(dsn = "X:/ProjetsCommuns/PROTEST/T1/Donnees_SIG/BD_Foret", layer = "BD_Foret_V2_PNRfilled_Foret_2014", encoding = "UTF-8", use_iconv = TRUE)

# load non-harvestable forest
nonHarv <- raster("X:/ProjetsCommuns/PROTEST/T1/Accessibilite/sylvaccess/sylvaccessv3.1/Skidder/Foret_non_buch.tif")
# set projection
crs(nonHarv) <- crs(BDforet)
# plot(nonHarv, col=colorRampPalette(c("black", "red"))(255))

# load inaccessible forest
nonAcc <- raster("X:/ProjetsCommuns/PROTEST/T1/Accessibilite/sylvaccess/sylvaccessv3.1/Skidder/Foret_inaccessible.tif")
# set projection
crs(nonAcc) <- crs(BDforet)
# plot(nonAcc, col=colorRampPalette(c("black", "red"))(255))

# load skidding distance
dist <- raster("X:/ProjetsCommuns/PROTEST/T1/Accessibilite/sylvaccess/sylvaccessv3.1/Skidder/Distance_totale_foret_route_forestiere.tif")
# set projection
crs(nonAcc) <- crs(BDforet)
# plot(nonAcc, col=colorRampPalette(c("black", "red"))(255))

###############################################################
# create new raster with nonHarv / nonAcc feature
###############################################################

# remove useless attributes in nonHarv
isBecomes <- cbind(c(0, 1),
                   c(NA, 1))
nonHarv <- reclassify(nonHarv, rcl = isBecomes)
# plot(nonHarv, col=colorRampPalette(c("black", "red"))(255))

# remove useless attributes in nonHarv
isBecomes <- cbind(c(1, 255),
                   c(1, NA))
nonAcc <- reclassify(nonAcc, rcl = isBecomes)
# plot(nonAcc, col=colorRampPalette(c("black", "red"))(255))

# merge rasters --> [f]ree [e]volution [f]orest
fef <- mosaic(nonHarv, nonAcc, fun = mean)

###############################################################
# create a raster with dist > 500 & dist < 500
###############################################################

# retrieve area < 500m
near <- dist < 500
isBecomes <- cbind(c(0, 1),
                   c(NA, 1))
near <- reclassify(near, rcl = isBecomes)

# retrieve area > 500m
far <- dist > 500
isBecomes <- cbind(c(0, 1),
                   c(NA, 2))
far <- reclassify(far, rcl = isBecomes)

# merge rasters --> [f]ree [e]volution [f]orest
dist <- mosaic(near, far, fun = mean)








################################################################################################################################

Work in progress

###############################################################
# create new shapefile with nonHarv / nonAcc feature
###############################################################

# convert raster into SpatialPolygon with GDAL and python
Sys.which("gdal_polygonize.py") # check wether the path is available

## Define the function
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

fef <- gdal_polygonizeR(fef)

# simplify a tad the polygons (otherwise topoly error in intersect)
fef <- gSimplify(fef, tol = 0.00001)
fef <- gBuffer(fef, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)
# sum(gIsValid(spydf_states, byid=TRUE)==FALSE)

# add attribute: 1 --> non harvestable / inaccessible
fef$nonHarAcc <- rep(1, length(fef))


##########################################################

# reduce extent for testing
fef <- crop(fef, c(938480, 6515480, 938500, 6515500))
BDforet <- crop(BDforet, c(938480, 6515480, 938500, 6515500))

# 3 - intersect with BDforet
# inter <- intersect(BDforet, fef)


# diff <- symdif(BDforet, fef)
diff <- gDifference(BDforet, fef)
diff <- gSimplify(diff, tol = 0.00001)
diff <- gBuffer(diff, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit = 1.0)
interBDDiff <- intersect(BDforet, diff)


diffBDInterBDDiff <- gDifference(BDforet, interBDDiff)
interdiffBDInterBDDiff <- intersect(BDforet, diffBDInterBDDiff)


result <- bind(interBDDiff, interdiffBDInterBDDiff)


plot(test, col = 'orange', border = 'orange')
plot(BDforet, col = 'green', border = 'green', add = TRUE)
plot(fef, col = 'red', border = 'red', add = TRUE)
plot(test1, col = 'blue', border = 'blue', add = TRUE)

# check
sum(area(test)) / 10000

sum(area(test[!is.na(test$CODE_TFV), ])) / 10000
sum(area(BDforet)) / 10000

sum(area(fef)) / 10000
sum(area(test[!is.na(test$nonHarAcc), ])) / 10000


plot(test1[!is.na(test1$nonHarAcc)], col = 'red', border = 'red', add = TRUE)

################################################################################################################################






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
