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

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

###############################################################
# BD foret
###############################################################

forestPlots <- readOGR(dsn = ".", layer = "superID_1", encoding = "UTF-8", use_iconv = TRUE)
crs(forestPlots) <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000
+y_0=6600000 +ellps=GRS80 +units=m +no_defs"

# bdForet <- readOGR(dsn = ".", layer = "BD_Foret_V2_PNR_2014", encoding = "UTF-8", use_iconv = TRUE)
# plot(bdForet, col = bdForet$CODE_TFV, border = bdForet$CODE_TFV)

###############################################################
# elevation
###############################################################

elev <- raster("MNT_all_5m.tif")
# set projection
crs(elev) <- crs(forestPlots)
# plot(elev, col=colorRampPalette(c("black", "white"))(255))

# slope
slo <- terrain(elev, opt = 'slope', unit = 'degrees', neighbors = 8)

# orientation
orien <- terrain(elev, opt = 'aspect', unit = 'degrees', neighbors = 8)

###############################################################
# greco
###############################################################

greco <- readOGR(dsn = "Z:/Private/DonneesIFN/Shapes_SER_GRECO", layer = "greco_l93", encoding = "UTF-8", use_iconv = TRUE)
greco <- spTransform(greco, crs(forestPlots)) # change projection

# plot(greco, col = greco$CODEGRECO)

# convert into a Raster
ext <- floor(extent(elev))
r <- raster(ext, res=res(elev))
grecoRaster <- rasterize(greco, r, field="CODEGRECO")
crs(grecoRaster) <- crs(forestPlots)
# plot(grecoRaster)

###############################################################
# dendro
###############################################################

# forest length
gdalwarp('X:/ProjetsCommuns/PROTEST/T1/Donnees_SIG/Foret_protection.tif',
          dstfile="Z:/Private/rasterVanneck/forProtec.tif", t_srs = crs(elev),
          output_Raster = FALSE, overwrite = TRUE, verbose = TRUE) # change projection
forProtec <- raster('Z:/Private/rasterVanneck/forProtec.tif')
# forProtec <- projectRaster(from = forProtec, crs = crs(forestPlots), res = res(forProtec)) # change projection

# Dg_pred
dgPred <- raster('X:/ProjetsCommuns/PROTEST/T5/Livrables/T1/model_pnr_bauges_73_74/raster/Dg_pred.tif')
crs(dgPred) <- crs(forestPlots)

# G_pred
gPred <- raster('X:/ProjetsCommuns/PROTEST/T5/Livrables/T1/model_pnr_bauges_73_74/raster/G_pred.tif')
crs(gPred) <- crs(forestPlots)

# GGB_pred
ggbPred <- raster('X:/ProjetsCommuns/PROTEST/T5/Livrables/T1/model_pnr_bauges_73_74/raster/GGB_pred.tif')
crs(ggbPred) <- crs(forestPlots)

# N_pred
nPred <- raster('X:/ProjetsCommuns/PROTEST/T5/Livrables/T1/model_pnr_bauges_73_74/raster/N_pred.tif')
crs(nPred) <- crs(forestPlots)

# p100GF_pred
p100gfPred <- raster('X:/ProjetsCommuns/PROTEST/T5/Livrables/T1/model_pnr_bauges_73_74/raster/p100GF_pred.tif')
crs(p100gfPred) <- crs(forestPlots)

###############################################################
# geol
###############################################################


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
# extract
###############################################################

# gather all rasters into one rasterbrick
# brk <- brick(slo, orien, template = elev)
# brk.vx <- velox(brk)

###################################
# extract with velox package
# convert "Raster" into "VeloxRaster"
elevVr <- velox(elev)
sloVr <- velox(slo)
orienVr <- velox(orien)
grecoVr <- velox(grecoRaster)

forProtecVr <- velox(forProtec)
dgPredVr <- velox(dgPred)
gPredVr <- velox(gPred)
ggbPredVr <- velox(ggbPred)
nPredVr <- velox(nPred)
p100gfPredVr <- velox(p100gfPred)

kVr <- velox(k)
lsVr <- velox(ls)
pVr <- velox(p)
rVr <- velox(r)

# extract GRECO
# Create a function to calculate the mode.
# Each plot is assigned the most represented GRECO.
getmode <- function(x) {
   uniqv <- unique(x)
   uniqv[which.max(tabulate(match(x, uniqv)))]
}
grecoExt <- grecoVr$extract(sp = forestPlots, fun = getmode, small = TRUE) # works despite the error message

# extract elevation
start_time <- Sys.time()
# elevExt <- elevVr$extract(sp = forestPlots[forestPlots$CODE_TFV == "FF1-10-10",], fun = mean, small = TRUE)
elevExt <- elevVr$extract(sp = forestPlots, fun = mean, small = TRUE)
end_time <- Sys.time()
end_time - start_time

# extract slope
start_time <- Sys.time()
sloExt <- sloVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
end_time <- Sys.time()
end_time - start_time

# extract orientation
start_time <- Sys.time()
orienExt <- orienVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
end_time <- Sys.time()
end_time - start_time

# extract dendro
start_time <- Sys.time()
forProtecExt <- forProtecVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
dgPredExt <- dgPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
gPredExt <- gPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
ggbPredExt <- ggbPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
nPredExt <- nPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
p100gfPredExt <- p100gfPredVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
end_time <- Sys.time()
end_time - start_time

# extract USLE parameters
start_time <- Sys.time()
kExt <- kVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
lsExt <- lsVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
pExt <- pVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rExt <- rVr$extract(sp = forestPlots, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
end_time <- Sys.time()
end_time - start_time

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
forProtecExtDf <- data.frame(forProtecExt)
colnames(forProtecExtDf) <- "forProtec"
dgPredExtDf <- data.frame(dgPredExt)
colnames(dgPredExtDf) <- "dgPred"
gPredExtDf <- data.frame(gPredExt)
colnames(gPredExtDf) <- "gPred"
ggbPredExtDf <- data.frame(ggbPredExt)
colnames(ggbPredExtDf) <- "ggbPred"
nPredExtDf <- data.frame(nPredExt)
colnames(nPredExtDf) <- "nPred"
p100gfPredExtDf <- data.frame(p100gfPredExt)
colnames(p100gfPredExtDf) <- "p100gfPred"
kExtDf <- data.frame(kExt)
colnames(kExtDf) <- "k"
lsExtDf <- data.frame(lsExt)
colnames(lsExtDf) <- "ls"
pExtDf <- data.frame(pExt)
colnames(pExtDf) <- "p"
rExtDf <- data.frame(rExt)
colnames(rExtDf) <- "r"

# convert slope degrees into percent
sloExtDf$slope <- tan(sloExtDf$slope*pi/180)*100

# convert orientation degrees into cos(radians)
orienExtDf$expoNS <- cos(orienExtDf$orient*pi/180)
orienExtDf$expoEW <- sin(orienExtDf$orient*pi/180)
orienExtDf <- orienExtDf[,c("expoNS", "expoEW")]

# integrate into the shp file
forestPlots@data <- cbind(forestPlots@data, grecoExtDf, elevExtDf, sloExtDf, orienExtDf,
                      forProtecExtDf, dgPredExtDf, gPredExtDf, ggbPredExtDf,
                      nPredExtDf, p100gfPredExtDf, kExtDf, lsExtDf, pExtDf, rExtDf)
# forestPlots@data <- cbind(forestPlots@data, elevExtDf)
# forestPlots@data <- cbind(forestPlots@data, sloExtDf)
# forestPlots@data <- cbind(forestPlots@data, orienExtDf)

###############################################################
# save new shp with altitudes
###############################################################

shapefile(forestPlots, filename = 'forestPlots3Ha', overwrite = TRUE)
