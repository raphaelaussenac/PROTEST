###############################################################
# initialisation
###############################################################

# clean up environment except user variable with path values
rm(list = setdiff(ls(),"user"))
user$checkSurfaces <- FALSE
# process stands of Mihai (modelling)
# or extract whole Bauges stands
user$mihai <- FALSE

# load packages
library(rgdal)
library(raster)
library(velox)
library(gdalUtils)
library(sf)

# set work directory
setwd(user$WorkingDir)

###############################################################
# LOAD GEOGRAPHICAL DATA
###############################################################
#
###############################################################
# study area extent
#
pnr <- rgdal::readOGR(dsn = paste0(user$NetworkProtestDir, "T1/Donnees_SIG/PNR"), layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)
# make sure projection info is Lambert93
pnr@proj4string <- sp::CRS("+init=epsg:2154")
#
###############################################################
# land ownership
# 
# load ownership raster
ownership <- raster::raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/BDForetv2_foret_PNRfilled_propriete_5m.tif"))
# plot(ownership, col=colorRampPalette(c("green", "blue"))(255))
#
###############################################################
# elevation
#
elev <- raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/MNT/MNT_all_5m.tif"))
#
# set projection
elev@crs <- pnr@proj4string
# plot(elev, col=colorRampPalette(c("black", "white"))(255))
#
###############################################################
# greco
#
# load shapefile
greco <- sf::st_read(paste0(user$NetworkProtestDir,"T3/scriptIFNPatrick/DonneesIFN/Shapes_SER_GRECO/greco_l93.shp"), quiet=TRUE, , stringsAsFactors=TRUE)
sf::st_crs(greco) = 2154
# plot(greco)
# convert to raster
grecoRaster <- fasterize::fasterize(greco, elev, field="CODEGRECO")
grecoRaster@crs <- pnr@proj4string
# plot(grecoRaster)
# 
###############################################################
# geological data
#
# load shapefile
geol <- sf::st_read("./data/geol.shp", quiet=TRUE, options = "ENCODING=latin1", stringsAsFactors=TRUE)
sf::st_crs(geol) <- pnr@proj4string
sf::st_crs(geol) <- 2154
# plot(geolsf)
# convert to raster
geolRaster <- fasterize::fasterize(geol, elev, field="NOTATION")
geolRaster@crs <- pnr@proj4string
# plot(geolRaster)
#
###############################################################
# dendro
#
# Dg_pred
dgPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/rastDg75error.clean.tif"))
# for mihai stands, use estimation without residuals
if(user$mihai) {dgPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/rastDg75.clean.tif"))}
dgPred@crs <- pnr@proj4string
dgPred <- dgPred / 100 # cm to m
#
# G_pred
gPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/rastG75error.clean.tif"))
# for mihai stands, use estimation without residuals
if(user$mihai) {gPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/rastG75.clean.tif"))}
gPred@crs <- pnr@proj4string
#
# GGB_pred
# ggbPred <- raster::raster(paste0(user$NetworkProtestDir, "T5/Livrables/T1/model_pnr_bauges_73_74/raster/GGB_pred.tif"))
# ggbPred@crs <- pnr@proj4string
#
# N_pred
# nPred <- raster::raster(paste0(user$NetworkProtestDir, "T5/Livrables/T1/model_pnr_bauges_73_74/raster/N_pred.tif"))
# nPred@crs <- pnr@proj4string
#
# p100GF_pred --> deciduous percentage
p100gfPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/propGR_ONF_25filled.tif"))
# convert to coniferous percentage
p100gfPred <- 100 - p100gfPred
p100gfPred@crs <- pnr@proj4string

# calculate N from g and dg (more reliable than N_pred)
ni <- (4*gPred) / (pi*dgPred^2)
# calculate N from g and dg (more reliable than N_pred)
niDgi2 <- ni * dgPred^2

###############################################################
# SILVAE data
#
# pH
ph <- raster::raster(paste0(user$NetworkProtestDir, "T3/donneesSilvae/ph_2008.tif"))
ph@crs <- pnr@proj4string
#
# rum
rum <- raster::raster(paste0(user$NetworkProtestDir, "T3/donneesSilvae/rum_500_v2009.tif"))
rum@crs <- pnr@proj4string
#
###############################################################
# Universal Soil Loss Equation (USLE) parameters
#
if (0)
{
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
  # Protection forest
  
  # forest length
  # gdalwarp('X:/ProjetsCommuns/PROTEST/T1/Donnees_SIG/Foret_protection.tif',
  # dstfile="Z:/Private/rasterVanneck/forProtec.tif", t_srs = crs(elev),
  # output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
  # te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
  # forProtec <- raster::raster(paste0(user$NetworkProtestDir, "T3/rasterVanneck/forProtec.tif"))
  # CHECK THAT FORPROTECT EXTENT IS LARGE ENOUGh TO COVER PNR EXTENT
  forProtec <- raster::raster(paste0(user$NetworkProtestDir, "T3/rasterVanneck/Foret_protection.tif"))
  # round extent to resolution multiple
  # extentTemp <- elev@extent
  # extentTemp[c(1,3)] <- floor(extentTemp[c(1,3)]/raster::res(forProtec))*raster::res(forProtec)
  # extentTemp[c(2,4)] <- ceiling(extentTemp[c(2,4)]/raster::res(forProtec))*raster::res(forProtec)
  # crop extent to match elev raster
  # forProtec <- raster::crop(forProtec, extentTemp)
  forProtec@crs <- pnr@proj4string
}


###############################################################
# prepare NFI data for extraction
###############################################################

# list variables in environment
listVariables <- ls()

# load IFN points
source(paste0(user$NetworkProtestDir, "T3/scriptIFNPatrick/Calcul_Potentiels/Calcul_Potentiels_Purs.R"))

# remove variables from IFN script except bd
rm(list=setdiff(ls(), c(listVariables, "bd")))

# set back work directory
setwd(user$WorkingDir)

# 1 - create circular plots around NFI points
# 1a - select points in the study area
ptIfn <- SpatialPointsDataFrame(bd[,c("xl93", "yl93")], data = data.frame(bd), proj4string = CRS(proj4string(pnr)))
# intersect pnr polygon with points location
baugesIfn <- sp::over(ptIfn, pnr)
# extract from bd only points inside pnr
bdBauges <- droplevels(bd[!is.na(baugesIfn$ID),])
# keep only relevant fields
bdBauges <- bdBauges[, c("idp", "xl93", "yl93", "potentiel_03", "potentiel_09",
                         "potentiel_61", "potentiel_62", 'unknownPart03',
                         'unknownPart09', 'unknownPart61', 'unknownPart62')]
# 1b - convert into spatial points
baugesIfnPts <- sp::SpatialPointsDataFrame(bdBauges[,c("xl93", "yl93")], data = data.frame(bdBauges), proj4string = CRS(proj4string(pnr)))
# 1c - convert into sf object
baugesIfnPtsSf <- sf::st_as_sf(baugesIfnPts)
sf::st_crs(baugesIfnPtsSf) = 2154
# 1c - create circular plots around points and set the radius
ifnCircular <- sf::st_buffer(baugesIfnPtsSf, dist = 15)

# plot(elev, col=colorRampPalette(c("black", "white"))(255))
# plot(ifnCircular, add = TRUE, col = 'red', border = 'red')
# plot(pnr, add=TRUE, border="blue")

###############################################################
# extract values for IFN plots and forest stands
###############################################################

# load forest stands
if (!user$mihai)
{
  forestStands <- rgdal::readOGR(dsn = "./data", layer = "forestStands3HaPolyMerge", encoding = "UTF-8", use_iconv = TRUE)
  forestStands@proj4string <- pnr@proj4string
  # remove useless columns
  forestStands$WKT <- NULL
  forestStands$BDID <- NULL
  forestStands$id <- NULL
} else {
  # parcelles enquetees
  # if (user$local)
  # {
  # forestStands <-rgdal::readOGR("/media/reseau/jmm/Private/datadisk/travaux/projets/ouigef/Mihai", layer="EnqOG_Bauges", encoding="latin1")
  # }
  # else
  #{
  forestStands <-rgdal::readOGR("/media/data/travaux/projets/ouigef/Mihai/", layer="EnqOG_Bauges", encoding="latin1")
  # }
  forestStands@proj4string <- pnr@proj4string
  forestStands@data$AREA <- raster::area(forestStands)
  forestStands@data[,which(!is.element(names(forestStands@data), c("AREA", "id")))] <- NULL
  user$checkSurfaces <- FALSE
}

# add unique id
forestStands$WKTid <- c(1:nrow(forestStands@data))

# Create a function to calculate the mode # could use "names(sort(table(variable),decreasing=TRUE))[1])" but numeric values are converted to text
# Each plot is assigned the most represented value
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

# extract GRECO id at center of IFN plot location
plotGreco <- sf::st_intersects(baugesIfnPtsSf, greco)
#
# extract GRECO as mode of raster values inside forestStands
# GRECO surfaces in raster
if (user$checkSurfaces) {dummy <- as.data.frame(raster::freq(grecoRaster*(!is.na(ownership))));dummy$surface <- dummy$count * res(grecoRaster)[1]^2/10000;dummy}
grecoVr <- velox::velox(grecoRaster)
rm(grecoRaster);gc()
standGreco <- grecoVr$extract(sp = forestStands, fun = getmode, small = TRUE)
# GRECO surfaces in polygons
if (user$checkSurfaces) {aggregate(forestStands$area/10000, by=list(standGreco), FUN=sum)}
rm(grecoVr);gc()

# extract geology at center of IFN plot location
plotGeol <- sf::st_intersects(baugesIfnPtsSf, geol)
#
# extract GEOL as mode of raster values inside forestStands
if (user$checkSurfaces) {dummy <- as.data.frame(raster::freq(geolRaster*(!is.na(ownership))));dummy$surface <- dummy$count * res(geolRaster)[1]^2/10000;dummy}
geolVr <- velox::velox(geolRaster)
rm(geolRaster);gc()
standGeol <- geolVr$extract(sp = forestStands, fun = getmode, small = TRUE)
# Geology surfaces in polygons
if (user$checkSurfaces)
{
  dummy2 <-aggregate(forestStands$area/10000, by=list(standGeol), FUN=sum)
  dummy <-merge(dummy, dummy2, by.x="value", by.y="Group.1", all=TRUE)
  plot(dummy$surface, dummy$x, xlab="Raster geologie", ylab="Polygones", log="xy", main="Comparaison des surfaces par géologie (log)");abline(c(0,1))
  sum(dummy$surface)
  sum(dummy$x, na.rm=TRUE)
}
rm(geolVr);gc()

# extract mean NS and EW orientation as mean inside IFN plots and forest stands
# compute from elevation
orien <- raster::terrain(elev, opt = 'aspect', unit = 'degrees', neighbors = 8)
expoNS <- cos(orien*pi/180)
expoEW <- sin(orien*pi/180)
rm(orien)
if (user$checkSurfaces)
{
  par(mfrow=c(2,2))
  hist(raster::values(expoNS)[!is.na(raster::values(ownership))], main="Raster, expoNS")
  hist(raster::values(expoEW)[!is.na(raster::values(ownership))], main="Raster, expoEW")
}
# extract
expoNSVr <- velox(expoNS)
rm(expoNS)
plotExpoNS <- expoNSVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
gc()
standExpoNS <- expoNSVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rm(expoNSVr)
gc()
expoEWVr <- velox(expoEW)
rm(expoEW)
plotExpoEW <- expoEWVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
gc()
standExpoEW <- expoEWVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rm(expoEWVr)
gc()
if (user$checkSurfaces)
{
  plotrix::weighted.hist(standExpoNS, forestStands$area, main="Polygons, expoNS")
  plotrix::weighted.hist(standExpoEW, forestStands$area, main="Polygons, expoEW")
}

# extract mean elevation inside IFN plot and forest stands
elevVr <- velox::velox(elev)
plotElev <- elevVr$extract(sp = ifnCircular, fun = mean, small = TRUE)
gc()
standElev <- elevVr$extract(sp = forestStands, fun = mean, small = TRUE)
rm(elevVr)
if (user$checkSurfaces)
{
  par(mfrow=c(1,2))
  hist(raster::values(elev)[which(!is.na(raster::values(ownership)))], main="Raster, altitude", freq=TRUE, breaks=seq(from=0, to =2500, by=100))
  plotrix::weighted.hist(standElev, forestStands$area/raster::res(elev)[1]^2, main="Polygons, altitude", freq=TRUE, breaks=seq(from=0, to =2500, by=100))
}
gc()

# slope
# compute slope from elevation
slo <- raster::terrain(elev, opt = 'slope', unit = 'degrees', neighbors = 8)
sloVr <- velox::velox(slo)
if (user$checkSurfaces)
{
  par(mfrow=c(1,2))
  hist(raster::values(slo)[which(!is.na(raster::values(ownership)))], main="Raster, slope", freq=TRUE, breaks=seq(from=0,to=90,by=5))
}
rm(slo)
# extract mean slope inside IFN plots
plotSlo <- sloVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
# extract mean slope inside forest stand
standSlo <- sloVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
if (user$checkSurfaces)
{
  plotrix::weighted.hist(standSlo, forestStands$area/raster::res(elev)[1]^2, main="Polygons, slope", freq=TRUE, breaks=seq(from=0,to=90,by=5))
}
rm(sloVr)
gc()

# extract silvae data: mean inside IFN plots and forest stands
phVr <- velox(ph)
plotph <- phVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
standph <- phVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rm(phVr)
gc()
rumVr <- velox(rum)
plotrum <- rumVr$extract(sp = ifnCircular, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
standrum <- rumVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
rm(rumVr)
gc()
# ajouter verification pour surface forestiere

###############################################################
# BIND DATA FOR IFN PLOTS
######################################################

# CBIND values extracted for ifn plots into df
grecoExtDf <- data.frame(greco=unlist(plotGreco))
grecoExtDf$greco <- droplevels(greco$CODEGRECO[grecoExtDf$greco]) # convert back factor numbers into GRECO letters
geolExtDf <- data.frame(geolNotation=unlist(plotGeol))
geolExtDf$geolNotation <- droplevels(geol$NOTATION[geolExtDf$geolNotation]) # convert back factor numbers into Geol letters
#
elevExtDf <- data.frame(alti=plotElev)
sloExtDf <- data.frame(slope=plotSlo)
expoNSExtDf <- data.frame(expoNS=plotExpoNS)
expoEWExtDf <- data.frame(expoEW=plotExpoEW)
#
phExtDf <- data.frame(ph=plotph)
rumExtDf <- data.frame(rum=plotrum)

# convert slope degrees into percent
sloExtDf$slope <- tan(sloExtDf$slope*pi/180)*100

# convert to sf
ifnCircular <- sf::as_Spatial(ifnCircular)

# create shp file
colnames(ifnCircular@data) <- c(c("idp", "X", "Y", "potentiel_03", "potentiel_09",
                                  "potentiel_61", "potentiel_62", 'unknownPart03',
                                  'unknownPart09', 'unknownPart61', 'unknownPart62'))
ifnCircular@data <- cbind(ifnCircular@data, geolExtDf, grecoExtDf, elevExtDf, sloExtDf,
                          #                       orienExtDf, phExtDf, rumExtDf)
                          expoNSExtDf, expoEWExtDf, phExtDf, rumExtDf)
# remove temporary variables
rm(list=ls(pattern="*ExtDf"))
rm(list=ls(pattern="plot*"))
# save
shapefile(ifnCircular, filename = './data/ifnCircular', overwrite = TRUE)
# compare with previous result of raphael
ifnCircular1 <- rgdal::readOGR(dsn="./data/", layer="ifnCircularOLD")
par(mfrow=c(5,4))
for (i in 1:ncol(ifnCircular@data)) {plot(ifnCircular@data[,i], ifnCircular1@data[,i], main=names(ifnCircular@data)[i])}
table(ifnCircular@data[,12], ifnCircular1@data[,12])
table(ifnCircular@data[,13], ifnCircular1@data[,13])
rm(ifnCircular1)

##############################################################
# EXTRACT MISSING VALUES FOR FOREST STANDS
##############################################################

# extract dendro
gPredVr <- velox::velox(gPred)
gPredExt <- gPredVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
if (user$checkSurfaces)
{
  print(paste0("Surface terrière moyenne - raster : ", round(mean(raster::values(gPred), na.rm=TRUE),1), " ; polygones : ", round(sum(gPredExt*forestStands$area, na.rm=TRUE)/sum(forestStands$area),1)))
  print(paste0("Surface terrière totale - raster : ", round(sum(raster::values(gPred), na.rm=TRUE) * raster::res(gPred)[1]^2/10000), " ; polygones : ", round(sum(gPredExt*forestStands$area, na.rm=TRUE)/10000)))
  par(mfrow=c(1,2))
  hist(raster::values(gPred),breaks=seq(from=0, to=120, by=5), main="Raster, Basal area")
  dummy <- which(!is.na(gPredExt))
  plotrix::weighted.hist(gPredExt[dummy], forestStands$area[dummy]/raster::res(elev)[1]^2, main="Polygons, Basal area", freq=TRUE, breaks=seq(from=0, to=120, by=5))
}
gPredfVr <- velox::velox(p100gfPred * gPred/100)
#
# compute deciduous percentage by stand: use weighted mean
# first compute total deciduous basal area
gFtotVrExt <- gPredfVr$extract(sp = forestStands, fun = function(x) sum(x, na.rm = TRUE), small = TRUE)
# second compute total basal area
gtotfVrExt <- gPredVr$extract(sp = forestStands, fun = function(x) sum(x, na.rm = TRUE), small = TRUE)
# finaly compute deciduous percentage
p100gfPredExt <- gFtotVrExt/gtotfVrExt*100
if (user$checkSurfaces)
{
  print(paste0("Surface terrière feuillue totale - raster : ", round(sum(raster::values(gPred*p100gfPred/100), na.rm=TRUE) * raster::res(gPred)[1]^2/10000), " ; polygones : ", round(sum(gPredExt*p100gfPredExt*forestStands$area/100, na.rm=TRUE)/10000)))
  par(mfrow=c(1,2))
  hist(raster::values(p100gfPred),breaks=seq(from=0, to=100, by=5), main="Raster, %G feuillus")
  dummy <- which(!is.na(p100gfPredExt))
  plotrix::weighted.hist(p100gfPredExt[dummy], forestStands$area[dummy]/raster::res(p100gfPred)[1]^2, main="Polygons, %G feuillus", freq=TRUE, breaks=seq(from=0, to=100, by=5))
}
rm(p100gfPred)
rm(gFtotVrExt, gtotfVrExt)
# 
# Ni et NiDGi [DISTRIBUTIONS N'ONT PAS ETE VERIFIEES]
niVr <- velox::velox(ni)
niExt <- niVr$extract(sp = forestStands, fun = function(x) sum(x, na.rm = TRUE), small = TRUE)
niDgi2Vr <- velox::velox(niDgi2)
niDgi2Ext <- niDgi2Vr$extract(sp = forestStands, fun = function(x) sum(x, na.rm = TRUE), small = TRUE)
if (user$checkSurfaces)
{
  par(mfrow=c(1,2))
  hist(raster::values(niDgi2)/525, main="Raster, niDgi2")
  dummy <- which(!is.na(niDgi2Ext))
  plotrix::weighted.hist(niDgi2Ext[dummy]/forestStands$area[dummy], forestStands$area[dummy]/raster::res(niDgi2)[1]^2, main="Polygons, niDgi2", freq=TRUE)
}
# remove remaining velox rasters
rm(list=ls(pattern="*Vr"))
#
# ggbPredVr <- velox(ggbPred)
# ggbPredExt <- ggbPredVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
# nPredVr <- velox(nPred)
# nPredExt <- nPredVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
# forProtecVr <- velox(forProtec)
# forProtecExt <- forProtecVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
#
# extract USLE parameters
# kVr <- velox(k)
# lsVr <- velox(ls)
# pVr <- velox(p)
# rVr <- velox(r)
# kExt <- kVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
# lsExt <- lsVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
# pExt <- pVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)
# rExt <- rVr$extract(sp = forestStands, fun = function(x) mean(x, na.rm = TRUE), small = TRUE)

# convert into df
grecoExtDf <- data.frame(greco=standGreco)
grecoExtDf$greco <- droplevels(greco$CODEGRECO[grecoExtDf$greco]) # convert back factor numbers into GRECO letters
#
elevExtDf <- data.frame(alti=standElev)
sloExtDf <- data.frame(slope=standSlo)
expoNSExtDf <- data.frame(expoNS=standExpoNS)
expoEWExtDf <- data.frame(expoEW=standExpoEW)
# forProtecExtDf <- data.frame(forProtecExt)
# colnames(forProtecExtDf) <- "forProtec"
niExtDf <- data.frame(sumNi=niExt)
niDgi2ExtDf <- data.frame(sumNiDgi2=niDgi2Ext)
dgPredExtDf <- data.frame(dgPred=sqrt(niDgi2ExtDf / niExtDf))
names(dgPredExtDf) <- "dgPred"
gPredExtDf <- data.frame(gPred=gPredExt)
# ggbPredExtDf <- data.frame(ggbPredExt)
# colnames(ggbPredExtDf) <- "ggbPred" 
# nPredExtDf <- data.frame(nPredExt)
# colnames(nPredExtDf) <- "nPred"
p100gfPredExtDf <- data.frame(p100gfPred=p100gfPredExt)
#
phExtDf <- data.frame(ph=standph)
rumExtDf <- data.frame(rum=standrum)
#
# kExtDf <- data.frame(kExt)
# colnames(kExtDf) <- "k"
# lsExtDf <- data.frame(lsExt)
# colnames(lsExtDf) <- "ls"
# pExtDf <- data.frame(pExt)
# colnames(pExtDf) <- "p"
# rExtDf <- data.frame(rExt)
# colnames(rExtDf) <- "r"
geolExtDf <- data.frame(geolNotation=levels(geol$NOTATION)[standGeol])
geolExtDf <- droplevels(geolExtDf)
#
rm(list=ls(pattern="stand*"))
#
# convert slope degrees into percent
sloExtDf$slope <- tan(sloExtDf$slope*pi/180)*100

# integrate into the shp file
forestStands@data <- cbind(forestStands@data, grecoExtDf, elevExtDf, sloExtDf,
                           expoNSExtDf, expoEWExtDf,  dgPredExtDf, gPredExtDf,
                           phExtDf, rumExtDf, p100gfPredExtDf, geolExtDf)#, 
# ,ggbPredExtDf, forProtecExtDf, kExtDf, lsExtDf, pExtDf, rExtDf)
rm(list=ls(pattern="*Ext"))


###############################################################
# load geol classification
###############################################################

classGeol <- read.csv("./data/classificationGeol.csv", header = TRUE, sep = ";")
classGeol <- classGeol[, c('NOTATION', 'Code_carbonate', 'Code_hydro')]
classGeol$rocheCalc <- 0
classGeol[classGeol$Code_carbonate > 0, 'rocheCalc'] <- 1
classGeol$Code_carbonate <- as.factor(classGeol$Code_carbonate)
classGeol$Code_hydro <- as.factor(classGeol$Code_hydro)
classGeol$rocheCalc <- as.factor(classGeol$rocheCalc)

# insert geol classification in forestStands
forestStands <- merge(forestStands, classGeol, by.x = "geolNotation", by.y = 'NOTATION', all.x = TRUE)
#
##################################################################
# NETTOYAGE DES POLYGONES
#
if (!user$mihai)
{
  forestStands03aNotCleaned <- forestStands
  save(forestStands03aNotCleaned, file="./data/forestStands03aNotCleaned.rda")
  rm(forestStands03aNotCleaned)
  ###############################################################
  # filters
  ###############################################################
  dim(forestStands)
  # remove polygones with geol == 'hydro' (== lac, river)
  # 0 polygons
  sum(forestStands$geolNotation == 'hydro')
  forestStands <- forestStands[forestStands$geolNotation != 'hydro', ]
  #
  # remove forest plots where gPred missing
  # 76 very small polygons with NA values
  sum(is.na(forestStands$gPred))
  sum(forestStands$area[is.na(forestStands$gPred)])/10000
  forestStands <- forestStands[!is.na(forestStands$gPred), ]
  # no polygons with 0 value
  sum(forestStands$gPred==0)
  sum(forestStands$area[forestStands$gPred==0])/10000
  forestStands <- forestStands[forestStands$gPred > 0, ]

  # remove forest plots where dgPred == 0
  # a few polygons, mostly in lowlands
  sum(is.na(forestStands$dgPred))
  sum(forestStands$area[is.na(forestStands$dgPred)])/10000
  forestStands <- forestStands[!is.na(forestStands$dgPred), ]
  #
  sum(forestStands$dgPred==0, na.rm=TRUE)
  sum(forestStands$area[forestStands$dgPred==0])/10000
  forestStands <- forestStands[forestStands$dgPred > 0, ]

  # remove plot where p100gfP = NA (none)
  sum(is.na(forestStands$p100gfPred))
  forestStands <- forestStands[!is.na(forestStands$p100gfPred), ]
  
  # convert mean BA/ha --> BA (real stock associated to each plot)
  forestStands$area <- raster::area(forestStands) / 10000
  forestStands$gPred <- forestStands$gPred * forestStands$area
  #
  dim(forestStands)
  sum(forestStands$area)
  
  # remove useless columns
  forestStands$id <- NULL
  
  ###############################################################
  # save
  ###############################################################
  save(forestStands, file="./data/forestStands03a.rda")
  raster::shapefile(forestStands, filename = './data/forestStands03a', overwrite = TRUE)
} else {
  save(forestStands, file="./data/forestStandsMihai03a.rda")
  raster::shapefile(forestStands, filename = './data/forestStandsMihai03a', overwrite = TRUE)
}