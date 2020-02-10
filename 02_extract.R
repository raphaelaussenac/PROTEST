###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
library(rgdal)
library(raster)
library(velox)
library(gdalUtils)
library(sf)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

###############################################################
# study area extend
###############################################################

pnr <- readOGR(dsn = "Z:/Private/PNR Bauges/Sans_Trou", layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)

###############################################################
# elevation
###############################################################

elev <- raster("MNT_all_5m.tif")
# set projection
crs(elev) <- crs(pnr)
# plot(elev, col=colorRampPalette(c("black", "white"))(255))

# slope
slo <- terrain(elev, opt = 'slope', unit = 'degrees', neighbors = 8)

# orientation
orien <- terrain(elev, opt = 'aspect', unit = 'degrees', neighbors = 8)

###############################################################
# greco
###############################################################

greco <- readOGR(dsn = "Z:/Private/DonneesIFN/Shapes_SER_GRECO", layer = "greco_l93", encoding = "UTF-8", use_iconv = TRUE)
greco <- spTransform(greco, crs(pnr)) # change projection
# plot(greco, col = greco$CODEGRECO)
#
# # convert into a Raster
# ext <- floor(extent(elev))
# r <- raster(ext, res=res(elev))
# grecoRaster <- rasterize(greco, r, field="CODEGRECO")
# crs(grecoRaster) <- crs(pnr)
# # plot(grecoRaster)
# #
# # # save raster
# writeRaster(grecoRaster, "grecoRaster.tif")

# load grecoRaster raster
grecoRaster <- raster('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/grecoRaster.tif')
# plot(grecoRaster, col=colorRampPalette(c("green", "blue"))(255))

###############################################################
# dendro
###############################################################

# forest length
gdalwarp('X:/ProjetsCommuns/PROTEST/T1/Donnees_SIG/Foret_protection.tif',
          dstfile="Z:/Private/rasterVanneck/forProtec.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
          te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
forProtec <- raster('Z:/Private/rasterVanneck/forProtec.tif')

# Dg_pred
dgPred <- raster('X:/ProjetsCommuns/PROTEST/T1/Observatoire/Analyse/rastDg75error.clean.tif')
crs(dgPred) <- crs(pnr)
dgPred <- dgPred / 100 # cm to m

# G_pred
gPred <- raster('X:/ProjetsCommuns/PROTEST/T1/Observatoire/Analyse/rastG75error.clean.tif')
crs(gPred) <- crs(pnr)

# GGB_pred
ggbPred <- raster('X:/ProjetsCommuns/PROTEST/T5/Livrables/T1/model_pnr_bauges_73_74/raster/GGB_pred.tif')
crs(ggbPred) <- crs(pnr)

# N_pred
nPred <- raster('X:/ProjetsCommuns/PROTEST/T5/Livrables/T1/model_pnr_bauges_73_74/raster/N_pred.tif')
crs(nPred) <- crs(pnr)

# p100GF_pred --> deciduous proportion
p100gfPred <- raster('X:/ProjetsCommuns/PROTEST/T1/Observatoire/Analyse/propGR_ONF_25filled.tif')
p100gfPred <- 100 - p100gfPred
crs(p100gfPred) <- crs(pnr)

# calculate N from g and dg (more reliable than N_pred)
ni <- (4*gPred) / (pi*dgPred^2)
# calculate N from g and dg (more reliable than N_pred)
niDgi2 <- ni * dgPred^2

###############################################################
# SILVAE data
###############################################################

# pH
gdalwarp('Z:/Private/donneesSilvae/ph_2008.tif',
          dstfile="Z:/Private/donneesSilvae/ph.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
          te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
ph <- raster('Z:/Private/donneesSilvae/ph.tif')

# ru
gdalwarp('Z:/Private/donneesSilvae/rum_500_v2009.tif',
          dstfile="Z:/Private/donneesSilvae/rum.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
          te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
rum <- raster('Z:/Private/donneesSilvae/rum.tif')

###############################################################
# Universal Soil Loss Equation (USLE) parameters
###############################################################

# K
gdalwarp('Z:/Private/rasterVanneck/K_new_crop.tif',
          dstfile="Z:/Private/rasterVanneck/k.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
          te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
k <- raster('Z:/Private/rasterVanneck/k.tif')

# Ls
gdalwarp('Z:/Private/rasterVanneck/EU_LS_Mosaic_100m.tif',
          dstfile="Z:/Private/rasterVanneck/ls.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
          te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
ls <- raster('Z:/Private/rasterVanneck/ls.tif')

# P
gdalwarp('Z:/Private/rasterVanneck/EU_PFactor_V2.tif',
          dstfile="Z:/Private/rasterVanneck/p.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
          te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
p <- raster('Z:/Private/rasterVanneck/p.tif')

# R
gdalwarp('Z:/Private/rasterVanneck/Rf_gp1.tif',
          dstfile="Z:/Private/rasterVanneck/r.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
          te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
r <- raster('Z:/Private/rasterVanneck/r.tif')

###############################################################
# land ownership
###############################################################

# load ownership raster
ownership <- raster('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/BDForetv2_foret_PNRfilled_propriete_5m.tif')
# plot(ownership, col=colorRampPalette(c("green", "blue"))(255))

###############################################################
# accesssibility
###############################################################

# load non-harvestable forest --------------------------------------------------
nonHarv <- raster("X:/ProjetsCommuns/PROTEST/T1/Accessibilite/sylvaccess/sylvaccessv3.3/Skidder/PNRfilled_Foret_non_buch.tif")
# set projection
crs(nonHarv) <- crs(pnr)
# plot(nonHarv, col=colorRampPalette(c("red", "green"))(255))

# remove useless attributes in nonHarv
isBecomes <- cbind(c(0, 1, NA),
                   c(1, 0, 1))
nonHarv <- reclassify(nonHarv, rcl = isBecomes)
# plot(nonHarv, col=colorRampPalette(c("red", "green"))(255))

# load skidding distance -------------------------------------------------------
dist <- raster("X:/ProjetsCommuns/PROTEST/T1/Accessibilite/sylvaccess/sylvaccessv3.3/Skidder/PNRfilled_F.distance.tif")
# set projection
crs(dist) <- crs(pnr)
# remove extreme (error) value
dist[dist > 100000] <- NA
# transform NA into -1000
# dist[is.na(dist[])] <- -1000

# plot(dist, col=colorRampPalette(c("black", "red"))(255))

# # retrieve area < 100000m
# near <- dist < 100000
# isBecomes <- cbind(c(0, 1),
#                    c(NA, 1))
# near <- reclassify(near, rcl = isBecomes)
#
# # retrieve area > 100000m
# far <- dist > 100000
# isBecomes <- cbind(c(0, 1),
#                    c(NA, 2))
# far <- reclassify(far, rcl = isBecomes)
#
# # merge rasters --> [f]ree [e]volution [f]orest
# dist <- mosaic(near, far, fun = sum)
# # isBecomes <- cbind(c(NA, 1, 2), # replace NA -> 0
# #                    c(0, 1, 2))
# # dist <- reclassify(dist, rcl = isBecomes)
# # plot(dist, col=colorRampPalette(c("black", "red"))(255))
# # writeRaster(dist, "Testdist.tif")

###############################################################
# prepare NFI data for extraction
###############################################################

# load IFN points
source('Z:/Private/Calcul_Potentiels/Calcul_Potentiels_Purs.R')

# set back work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# 1 - create circular plots around NFI points
  # 1a - select points in the study area
ptIfn <- SpatialPointsDataFrame(bd[,c("xl93", "yl93")], data = data.frame(bd), proj4string = CRS(proj4string(pnr)))
baugesIfn <- over(ptIfn, pnr)
baugesIfn <- droplevels(baugesIfn[!is.na(baugesIfn$ID),])
bdBauges <- droplevels(bd[as.numeric(row.names(baugesIfn)),])
bdBauges <- bdBauges[, c("idp", "xl93", "yl93", "potentiel_03", "potentiel_09",
                        "potentiel_61", "potentiel_62", 'unknownPart03',
                        'unknownPart09', 'unknownPart61', 'unknownPart62')]
  # 1b - convert into spatial points
baugesIfnPts <- SpatialPointsDataFrame(bdBauges[,c("xl93", "yl93")], data = data.frame(bdBauges), proj4string = CRS(proj4string(pnr)))
  # 1c - convert into sf object
baugesIfnPtsSf <- st_as_sf(baugesIfnPts)
  # 1c - create circular plots around points and set the radius
ifnCircular <- st_buffer(baugesIfnPtsSf, dist = 15)

plot(elev, col=colorRampPalette(c("black", "white"))(255))
plot(ifnCircular, add = TRUE, col = 'red', border = 'red')

###############################################################
# geological data
###############################################################

geol <- readOGR(dsn = ".", layer = "geol", encoding = "UTF-8", use_iconv = TRUE)

# intersection NFI points / geol polygone
ifnGeol <- intersect(baugesIfnPts, geol)

###############################################################
# extract with velox package
###############################################################

# convert "Raster" into "VeloxRaster"
elevVr <- velox(elev)
sloVr <- velox(slo)
orienVr <- velox(orien)
grecoVr <- velox(grecoRaster)
ownershipVr <- velox(ownership)
nonHarvVr <- velox(nonHarv)
distVr <- velox(dist)
forProtecVr <- velox(forProtec)
niVr <- velox(ni)
niDgi2Vr <- velox(niDgi2)
gPredVr <- velox(gPred)
ggbPredVr <- velox(ggbPred)
nPredVr <- velox(nPred)
p100gfPredVr <- velox(p100gfPred)
phVr <- velox(ph)
rumVr <- velox(rum)
kVr <- velox(k)
lsVr <- velox(ls)
pVr <- velox(p)
rVr <- velox(r)

###############################################################
# extract values for NFI plots
###############################################################

# Create a function to calculate the mode
# Each plot is assigned the most represented value
getmode <- function(x) {
   uniqv <- unique(x)
   uniqv <- uniqv[!is.na(uniqv)]
   uniqv[which.max(tabulate(match(x, uniqv)))]
}

grecoExt <- grecoVr$extract(sp = ifnCircular, fun = getmode, small = TRUE)

# extract elevation
elevExt <- elevVr$extract(sp = ifnCircular, fun = mean, small = TRUE)

# extract slope
sloExt <- sloVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# extract orientation
orienExt <- orienVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# extract silvae data
phExt <- phVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rumExt <- rumVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# convert into df
grecoExtDf <- data.frame(grecoExt)
colnames(grecoExtDf) <- "greco"
grecoExtDf$greco <- greco$CODEGRECO[grecoExtDf$greco] # convert back factor numbers into GRECO letters
elevExtDf <- data.frame(elevExt)
colnames(elevExtDf) <- "alti"
sloExtDf <- data.frame(sloExt)
colnames(sloExtDf) <- "slope"
orienExtDf <- data.frame(orienExt)
colnames(orienExtDf) <- "orient"
phExtDf <- data.frame(phExt)
colnames(phExtDf) <- "ph"
rumExtDf <- data.frame(rumExt)
colnames(rumExtDf) <- "rum"

# convert slope degrees into percent
sloExtDf$slope <- tan(sloExtDf$slope*pi/180)*100

# convert orientation degrees into cos(radians)
orienExtDf$expoNS <- cos(orienExtDf$orient*pi/180)
orienExtDf$expoEW <- sin(orienExtDf$orient*pi/180)
orienExtDf <- orienExtDf[,c("expoNS", "expoEW")]

# add geol data
ifnCircular <- as_Spatial(ifnCircular)
ifnCircular@data <- cbind(ifnCircular@data, ifnGeol$NOTATION)

# create shp file
colnames(ifnCircular@data) <- c(c("idp", "X", "Y", "potentiel_03", "potentiel_09",
                                "potentiel_61", "potentiel_62", 'unknownPart03',
                                'unknownPart09', 'unknownPart61', 'unknownPart62', 'geolNotation'))
ifnCircular@data <- cbind(ifnCircular@data, grecoExtDf, elevExtDf, sloExtDf,
                      orienExtDf, phExtDf, rumExtDf)

# save
shapefile(ifnCircular, filename = 'ifnCircular', overwrite = TRUE)

###############################################################
# extract values for all forest plots
###############################################################

forestPlots <- readOGR(dsn = ".", layer = "BDid_1", encoding = "UTF-8", use_iconv = TRUE)
crs(forestPlots) <- crs(pnr)
# add unique id
forestPlots$WKTid <- c(1:nrow(forestPlots@data))
# remove useless columns
forestPlots$WKT <- NULL
forestPlots$BDID <- NULL

# extract GRECO
grecoExt <- grecoVr$extract(sp = forestPlots, fun = getmode, small = TRUE)

# extract ownership
ownershipExt <- ownershipVr$extract(sp = forestPlots, fun = getmode, small = TRUE)

# extract nonHarv
# Create a function to calculate the proportion of nonHarv and set a threshold
# to determine whether the plot will be harvestable or not (this is done because
# the non-harvestable surface is underestimated with our method using 3ha plots)
# ex with a threshold of 35: if more than 35% of a plot is non-harvestaable, it
# becomes entirely non-harvestable
threshold <- 35.34
getprop <- function(x) {
  uniqv <- sort(unique(x)) # 0, 1
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which(tabulate(match(x, uniqv)) * 100 / sum(tabulate(match(x, uniqv))) > threshold)][1]
}
nonHarvExt <- nonHarvVr$extract(sp = forestPlots, fun = getprop, small = TRUE)

# # extract dist
# # Create a function to calculate the proportion of dist 0, 1 and 2 on each plot
# # and set a threshold to determine wether the entire plot will become 0 or 1 or 2
# # ex with a threshold of 40: if more than 40% of a plot is 0 and the rest of it is 1
# # it becomes entirely 0 (favor 0 when 0,1 otherwise (0,2 - 1,2 - 1,2,3) take the mode)
# threshold01 <- 40
# getpropdist <- function(x) {
#   uniqv <- sort(unique(x)) # 0, 1
#   uniqv <- uniqv[!is.na(uniqv)]
#   if (length(uniqv)==2 & sum(uniqv) == 1){ # if dist is 0,1 --> favor 0
#     uniqv[which(tabulate(match(x, uniqv)) * 100 / sum(tabulate(match(x, uniqv))) > threshold01)][1]
#   } else {
#     uniqv[which.max(tabulate(match(x, uniqv)))] # mode
#   }
# }
# distExt <- distVr$extract(sp = forestPlots, fun = getpropdist, small = TRUE)

# extract dist
# if most of the plot is inaccessible (x = NA) -> inaccessible (i.e. plot dist = NA)
# if most of the plot is accessible (x != NA) -> dist = mean(dist != NA)
getdist <- function(x) {
  inaccess <- length(x[is.na(x)])
  access <- length(x[!is.na(x)])
  if (inaccess > access){
    NA
  } else {
    mean(x, na.rm = TRUE)
  }
}
distExt <- distVr$extract(sp = forestPlots, fun = getdist, small = TRUE)

# extract elevation
elevExt <- elevVr$extract(sp = forestPlots, fun = mean, small = TRUE)

# extract slope
sloExt <- sloVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# extract orientation
orienExt <- orienVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# extract silvae data
phExt <- phVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rumExt <- rumVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# extract dendro
forProtecExt <- forProtecVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
gPredExt <- gPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
ggbPredExt <- ggbPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
nPredExt <- nPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
p100gfPredExt <- p100gfPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# dg extraction
# dg = sqrt(sum(Ni*Dgi^2) / sum(Ni))
# calculate sum(Ni)
niExt <- niVr$extract(sp = forestPlots, fun = function(x) sum(x, na.rm = TRUE), small = TRUE)
# calculate sum(Ni*Dgi^2)
niDgi2Ext <- niDgi2Vr$extract(sp = forestPlots, fun = function(x) sum(x, na.rm = TRUE), small = TRUE)

# extract USLE parameters
kExt <- kVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
lsExt <- lsVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
pExt <- pVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rExt <- rVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# extract geol
# retrieve forest plots centroid coordinates
coordForest <- data.frame(coordinates(forestPlots))
colnames(coordForest) <- c("X", "Y")
# convert into a spatialPoint ob=object
coordForest <- SpatialPointsDataFrame(coordForest[,c("X", "Y")], data = data.frame(coordForest), proj4string = CRS(proj4string(pnr)))
# intersection forest plots centroid / geol polygone
coordForest <- intersect(coordForest, geol)

# convert into df
grecoExtDf <- data.frame(grecoExt)
colnames(grecoExtDf) <- "greco"
grecoExtDf$greco <- greco$CODEGRECO[grecoExtDf$greco] # convert back factor numbers into GRECO letters
ownershipExtDf <- data.frame(ownershipExt)
colnames(ownershipExtDf) <- "owner"
ownershipExtDf[ownershipExtDf$owner == 2 & !is.na(ownershipExtDf$owner), 'owner'] <- 'Pub'
ownershipExtDf[ownershipExtDf$owner == 1 & !is.na(ownershipExtDf$owner), 'owner'] <- 'Priv'
ownershipExtDf$owner <- as.factor(ownershipExtDf$owner)
nonHarvExtDf <- data.frame(nonHarvExt)
colnames(nonHarvExtDf) <- "nonHarv"
distExtDf <- data.frame(distExt)
colnames(distExtDf) <- "dist"
elevExtDf <- data.frame(elevExt)
colnames(elevExtDf) <- "alti"
sloExtDf <- data.frame(sloExt)
colnames(sloExtDf) <- "slope"
orienExtDf <- data.frame(orienExt)
colnames(orienExtDf) <- "orient"
forProtecExtDf <- data.frame(forProtecExt)
colnames(forProtecExtDf) <- "forProtec"
niExtDf <- data.frame(niExt)
colnames(niExtDf) <- "sumNi"
niDgi2ExtDf <- data.frame(niDgi2Ext)
colnames(niDgi2ExtDf) <- "sumNiDgi2"
dgPredExtDf <- sqrt(niDgi2ExtDf / niExtDf)
colnames(dgPredExtDf) <- "dgPred"
gPredExtDf <- data.frame(gPredExt)
colnames(gPredExtDf) <- "gPred"
ggbPredExtDf <- data.frame(ggbPredExt)
colnames(ggbPredExtDf) <- "ggbPred"
nPredExtDf <- data.frame(nPredExt)
colnames(nPredExtDf) <- "nPred"
p100gfPredExtDf <- data.frame(p100gfPredExt)
colnames(p100gfPredExtDf) <- "p100gfPred"
phExtDf <- data.frame(phExt)
colnames(phExtDf) <- "ph"
rumExtDf <- data.frame(rumExt)
colnames(rumExtDf) <- "rum"
kExtDf <- data.frame(kExt)
colnames(kExtDf) <- "k"
lsExtDf <- data.frame(lsExt)
colnames(lsExtDf) <- "ls"
pExtDf <- data.frame(pExt)
colnames(pExtDf) <- "p"
rExtDf <- data.frame(rExt)
colnames(rExtDf) <- "r"

# add geol data
forestPlots@data <- cbind(forestPlots@data, coordForest$NOTATION)
colnames(forestPlots@data)[colnames(forestPlots@data) == 'coordForest$NOTATION'] <- 'geolNotation'

# convert slope degrees into percent
sloExtDf$slope <- tan(sloExtDf$slope*pi/180)*100

# convert orientation degrees into cos(radians)
orienExtDf$expoNS <- cos(orienExtDf$orient*pi/180)
orienExtDf$expoEW <- sin(orienExtDf$orient*pi/180)
orienExtDf <- orienExtDf[,c("expoNS", "expoEW")]

# integrate into the shp file
forestPlots@data <- cbind(forestPlots@data, grecoExtDf, ownershipExtDf,
                      nonHarvExtDf, distExtDf, elevExtDf, sloExtDf,
                      orienExtDf, forProtecExtDf, dgPredExtDf, gPredExtDf,
                      ggbPredExtDf, p100gfPredExtDf, phExtDf,
                      rumExtDf, kExtDf, lsExtDf, pExtDf, rExtDf)

###############################################################
# load geol classification
###############################################################

classGeol <- read.csv("classificationGeol.csv", header = TRUE, sep = ";")
classGeol <- classGeol[, c('NOTATION', 'Code_carbonate', 'Code_hydro')]
classGeol$rocheCalc <- 0
classGeol[classGeol$Code_carbonate > 0, 'rocheCalc'] <- 1
classGeol$Code_carbonate <- as.factor(classGeol$Code_carbonate)
classGeol$Code_hydro <- as.factor(classGeol$Code_hydro)
classGeol$rocheCalc <- as.factor(classGeol$rocheCalc)

# insert geol classification in forestPlots
forestPlots <- merge(forestPlots, classGeol, by.x = "geolNotation", by.y = 'NOTATION', all.x = TRUE)

###############################################################
# save
###############################################################

shapefile(forestPlots, filename = 'forestPlots3Ha', overwrite = TRUE)
