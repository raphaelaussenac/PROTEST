###############################################################
# initialisation
###############################################################

# clean up environment except user variable with path values
rm(list = setdiff(ls(),"user"))
user$checkSurfaces <- TRUE

# load packages
library(rgdal)
library(raster)
library(velox)
library(gdalUtils)
library(sf)

# set work directory
setwd(user$WorkingDir)

# process stands of Mihai (modelling)
# or extract whole Bauges stands
user$mihai <- FALSE

###############################################################
# LOAD GEOGRAPHICAL DATA
###############################################################

###############################################################
# study area extent

pnr <- rgdal::readOGR(dsn = paste0(user$NetworkProtestDir, "T1/Donnees_SIG/PNR"), layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)
# make sure projection info is Lambert93
pnr@proj4string <- sp::CRS("+init=epsg:2154")

###############################################################
# elevation

if (user$confinement) {elev <- raster("./data/MNT_all_5m.tif")} else {elev <- raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/MNT/MNT_all_5m.tif"))}

# set projection
elev@crs <- pnr@proj4string
# plot(elev, col=colorRampPalette(c("black", "white"))(255))

###############################################################
# cadastre

# load cadastre in 73 and 74
# cadastreRaster <- list()
# for (i in c(73,74))
# {
#   # load vector layer of cadastre
#   cadastre <- sf::st_read(paste0("/media/reseau/infogeo/ign/BD_PARCELLAIRE/V1.2/BDPV_1-2_SHP_LAMB93_D0",i,"_2014/PARCELLE.SHP"), quiet=TRUE)
#   # add area field
#   cadastre$AREA <- sf::st_area(cadastre)
#   # rasterize area
#   cadastreRaster[[i]] <- fasterize::fasterize(cadastre, elev, field="AREA")
# }
# cadastreR <- raster::merge(cadastreRaster[[73]], cadastreRaster[[74]])
# cadastreR@crs <- pnr@proj4string
# raster::plot(cadastreR)
# raster::writeRaster(cadastreR, file=paste0(user$NetworkProtestDir, "T1/Donnees_SIG/Cadastre/cadastreAREA.tif"))
# 
if (user$confinement) {cadastreR <- raster("./data/cadastreAREA.tif")} else {cadastreR <- raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/Cadastre/cadastreAREA.tif"))}
cadastreR <- raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/Cadastre/cadastreAREA.tif"))

###############################################################
# greco

greco <- sf::st_read(paste0(user$NetworkProtestDir,"T3/scriptIFNPatrick/DonneesIFN/Shapes_SER_GRECO/greco_l93.shp"), quiet=TRUE)
sf::st_crs(greco) = 2154
# plot(greco)
grecoRaster <- fasterize::fasterize(greco, elev, field="CODEGRECO")
grecoRaster@crs <- pnr@proj4string
# plot(grecoRaster)

###############################################################
# geological data

# geol <- rgdal::readOGR(dsn = "./data", layer = "geol", encoding = "UTF-8", use_iconv = TRUE)
geol <- sf::st_read("./data/geol.shp", quiet=TRUE, options = "ENCODING=latin1")
sf::st_crs(geol) <- pnr@proj4string
sf::st_crs(geol) <- 2154
# 
# plot(geolsf)
geolRaster <- fasterize::fasterize(geol, elev, field="NOTATION")
geolRaster@crs <- pnr@proj4string
# plot(geolRaster)

###############################################################
# dendro

# Dg_pred
dgPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/rastDg75error.clean.tif"))
dgPred@crs <- pnr@proj4string
dgPred <- dgPred / 100 # cm to m

# G_pred
gPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/rastG75error.clean.tif"))
gPred@crs <- pnr@proj4string

# GGB_pred
# ggbPred <- raster::raster(paste0(user$NetworkProtestDir, "T5/Livrables/T1/model_pnr_bauges_73_74/raster/GGB_pred.tif"))
# ggbPred@crs <- pnr@proj4string

# N_pred
# nPred <- raster::raster(paste0(user$NetworkProtestDir, "T5/Livrables/T1/model_pnr_bauges_73_74/raster/N_pred.tif"))
# nPred@crs <- pnr@proj4string

# p100GF_pred --> deciduous proportion
p100gfPred <- raster::raster(paste0(user$NetworkProtestDir, "T1/Observatoire/Analyse/propGR_ONF_25filled.tif"))
p100gfPred <- 100 - p100gfPred
p100gfPred@crs <- pnr@proj4string

# calculate N from g and dg (more reliable than N_pred)
ni <- (4*gPred) / (pi*dgPred^2)
# calculate N from g and dg (more reliable than N_pred)
niDgi2 <- ni * dgPred^2

###############################################################
# SILVAE data

# pH
# gdalwarp('Z:/Private/donneesSilvae/ph_2008.tif',
#           dstfile="Z:/Private/donneesSilvae/ph.tif", t_srs = crs(elev),
#           output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
#           te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
ph <- raster::raster(paste0(user$NetworkProtestDir, "T3/donneesSilvae/ph_2008.tif"))
ph@crs <- pnr@proj4string
# ph <- raster::raster(paste0(user$NetworkProtestDir, "T3/donneesSilvae/ph.tif"))

# rum
# gdalwarp('Z:/Private/donneesSilvae/rum_500_v2009.tif',
#           dstfile="Z:/Private/donneesSilvae/rum.tif", t_srs = crs(elev),
#           output_Raster = FALSE, overwrite = TRUE, verbose = TRUE,
#           te = c(925930, 6489455, 968160, 6538375), te_srs = crs(elev)) # change projection
rum <- raster::raster(paste0(user$NetworkProtestDir, "T3/donneesSilvae/rum_500_v2009.tif"))
rum@crs <- pnr@proj4string
# rum <- raster('Z:/Private/donneesSilvae/rum.tif')

###############################################################
# Universal Soil Loss Equation (USLE) parameters

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
# land ownership

# load ownership raster
ownership <- raster::raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/BDForetv2_foret_PNRfilled_propriete_5m.tif"))
# plot(ownership, col=colorRampPalette(c("green", "blue"))(255))

###############################################################
# accessibility
# loaded just before extraction to avoid memory swap

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
  # forestStands <- rgdal::readOGR(dsn = "./data", layer = "BDid_1", encoding = "UTF-8", use_iconv = TRUE)
  forestStands <- rgdal::readOGR(dsn = "./data", layer = "forestStands3HaPolyMerge", encoding = "UTF-8", use_iconv = TRUE)
  forestStands@proj4string <- pnr@proj4string
  # remove useless columns
  forestStands$WKT <- NULL
  forestStands$BDID <- NULL
  forestStands$id <- NULL
} else {
  # parcelles enquetees
  if (user$confinement)
  {
    forestStands <-rgdal::readOGR("/media/reseau/jmm/Private/datadisk/travaux/projets/ouigef/Mihai", layer="EnqOG_Bauges", encoding="latin1")
  }
  else
  {
    forestStands <-rgdal::readOGR("/media/data/travaux/projets/ouigef/Mihai/", layer="EnqOG_Bauges", encoding="latin1")
  }
  forestStands@proj4string <- pnr@proj4string
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

# extract id of greco region at center of IFN plot location
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

##############################################################
# extract mean parcelle area from rasterized area of parcells

if (user$checkSurfaces)
{
  par(mfrow=c(1,2))
  hist(raster::values(cadastreR)[which(!is.na(raster::values(ownership)))], main="Raster, mean surface of parcels", freq=TRUE)
}
cadastreRVr <- velox::velox(cadastreR)
rm(cadastreR)
standMeanParcellAreaExt <- cadastreRVr$extract(sp = forestStands, function(x) mean(x[is.finite(x)]), small = TRUE)
#

rm(cadastreRVr);gc()
if (user$checkSurfaces)
{
  dummy <- which(!is.na(standMeanParcellAreaExt))
  plotrix::weighted.hist(standMeanParcellAreaExt[dummy], forestStands$area[dummy]/raster::res(elev)[1]^2, main="Polygons, mean surface of parcels", freq=TRUE)
}

###############################################################
# extract values for ownership accessibility dendro (stands)

if (user$checkSurfaces)
{
  par(mfrow=c(1,2))
  dummy <- as.data.frame(freq(ownership))
  dummy$surface <- dummy$count*raster::res(ownership)[1]^2/10000
}
ownershipVr <- velox::velox(ownership)
ownershipExt <- ownershipVr$extract(sp = forestStands, fun = getmode, small = TRUE)
rm(ownershipVr)
if(user$checkSurfaces)
{
  aggregate(forestStands$area/10000, by=list(ownershipExt), FUN=sum)
  dummy$surface
}
gc()

###############################################################
# extraction accessibility values 

# load non-harvestable forest
nonHarv <- raster::raster(paste0(user$NetworkProtestDir, "T1/Accessibilite/sylvaccess/sylvaccessv3.3/Skidder/PNRfilled_Foret_non_buch.tif"))
# set projection
nonHarv@crs <- pnr@proj4string
# plot(nonHarv, col=colorRampPalette(c("red", "green"))(255))

# remove useless attributes in nonHarv
isBecomes <- cbind(c(0, 1, NA),
                   c(1, 0, 1))
# 0 (harvestable forest) 1 (forest not harvestable) NA (outside forest)
# becomes 0 (forest not harvestable) 1 (other)
nonHarv <- raster::reclassify(nonHarv, rcl = isBecomes)
# non harvestable area from raster
if (user$checkSurfaces) 
{
  dummy <- (length(nonHarv)-sum(raster::values(nonHarv)))*raster::res(nonHarv)[1]^2/10000
}
# plot(nonHarv, col=colorRampPalette(c("red", "green"))(255))
#
# extract nonHarv
# Create a function to calculate the proportion of nonHarv and set a threshold
# to determine whether the plot will be harvestable or not (this is done because
# the non-harvestable surface is underestimated with our method using 3ha plots)
# ex with a threshold of 35: if more than 35% of a plot is non-harvestaable, it
# becomes entirely non-harvestable
threshold <- 35.15 # 35.34
getprop <- function(x) {
  uniqv <- sort(unique(x)) # 0, 1
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which(tabulate(match(x, uniqv)) * 100 / sum(tabulate(match(x, uniqv))) > threshold)][1]
}
nonHarvVr <- velox::velox(nonHarv)
rm(nonHarv)
nonHarvExt <- nonHarvVr$extract(sp = forestStands, fun = getprop, small = TRUE)
rm(nonHarvVr)
gc()
if (user$checkSurfaces) 
{
  print(paste0("Surface non bucheronnable - raster : ", round(dummy), " ; polygones : ", round(sum((nonHarvExt==0) * forestStands$area)/10000)))
}

# load skidding distance
dist  <- raster::raster(paste0(user$NetworkProtestDir, "T1/Accessibilite/sylvaccess/sylvaccessv3.3/Skidder/PNRfilled_F.distance.tif"))
# numeric value (transport distance between felling place and roadside) NA (forest not accessible OR not forest not harvestable OR outside forest)
# set projection
dist@crs <- pnr@proj4string
# remove extreme (error) value
dist[dist > 100000] <- NA
if (user$checkSurfaces)
{
  dummy <- sum(raster::values(is.na(dist))*raster::values(!is.na(ownership))) * raster::res(dist)[1]^2/10000
  par(mfrow=c(1,2))
  hist(raster::values(dist)[which(!is.na(raster::values(ownership)))], main="Raster, Skidding distance", freq=TRUE, breaks=seq(from=0, to=7000, by=250))
}

# extract dist
# if most of the plot is inaccessible (x = NA) -> inaccessible (i.e. plot dist = NA)
# if most of the plot is accessible (x != NA) -> dist = mean(dist != NA)
# a proportio nof 0.85 is applied so that surfaces are globally consistent between raster and polygons
getdist <- function(x) {
  inaccess <- length(x[is.na(x)])
  access <- length(x[!is.na(x)])
  if (inaccess > 0.85 *access){
    NA
  } else {
    mean(x, na.rm = TRUE)
  }
}
distVr <- velox::velox(dist)
rm(dist)
distExt <- distVr$extract(sp = forestStands, fun = getdist, small = TRUE)
rm(distVr)
if (user$checkSurfaces)
{
  print(paste0("Surface inaccessible - raster : ", round(dummy), " ; polygones : ", round(sum(forestStands$area[is.na(distExt)])/10000)))
  dummy <- which(!is.na(distExt))
  plotrix::weighted.hist(distExt[dummy], forestStands$area[dummy]/raster::res(elev)[1]^2, main="Polygons, Skidding distance", freq=TRUE, breaks=seq(from=0, to=7000, by=250))
}
gc()

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
ownershipExtDf <- data.frame(owner=ownershipExt)
ownershipExtDf[ownershipExtDf$owner == 2 & !is.na(ownershipExtDf$owner), 'owner'] <- 'Pub'
ownershipExtDf[ownershipExtDf$owner == 1 & !is.na(ownershipExtDf$owner), 'owner'] <- 'Priv'
ownershipExtDf$owner <- as.factor(ownershipExtDf$owner)
#
cadastreExtDf <- data.frame(meanParcelleArea=standMeanParcellAreaExt)
nonHarvExtDf <- data.frame(nonHarv=nonHarvExt)
distExtDf <- data.frame(dist=distExt)
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
forestStands@data <- cbind(forestStands@data, grecoExtDf, ownershipExtDf,
                      nonHarvExtDf, distExtDf, elevExtDf, sloExtDf,
                      expoNSExtDf, expoEWExtDf,  dgPredExtDf, gPredExtDf,
                      phExtDf, rumExtDf, p100gfPredExtDf, geolExtDf, cadastreExtDf)#, 
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

#########
# # COMPARAISON AVEC RESULTATS RAPHAEL A FAIRE ICI ????
# # AVANT SUPPRESSION DES POLYGONES
# # (avec donnees extraites par raphael : auparavant : polymerge fait apres extract)
# forestStands1 <- rgdal::readOGR(dsn="./data/", layer="forestPlots3HaPolyMergeOLD")
# #
# table(forestStands$geolNotation)
# table(forestStands$)
# sum(as.character(forestStands$geolNotation) == as.character(forestStands1$gelNttn))/nrow(forestStands) # mode plutôt que extraction au centroide
# sum(as.character(forestStands$CODE_TFV)== as.character(forestStands1$CODE_TF))/nrow(forestStands)
# table(forestStands$INSEE_DEP, forestStands1$INSEE_D)
# table(forestStands$greco, forestStands1$greco)
# table(forestStands$owner, forestStands1$owner)
# table(forestStands$nonHarv, forestStands1$nonHarv)
# #
# par(mfrow=c(3,4))
# for (i in c("dist", "alti", "slope", "expoNS", "expoEW", "gPred", "ph", "rum")) 
# {plot(forestStands@data[,i], forestStands1@data[,i], main=i)}
# plot(forestStands$p100gfPred, forestStands1$p100gfP, main="% feuillus")
# plot(forestStands$sumNiDgi2, forestStands1$dgPred, main="Dg")

forestStandsBck <- forestStands
#
##################################################################
#
if (!user$mihai)
{
  ###############################################################
  # filters
  ###############################################################
  dim(forestStands)
  # remove polygones with geol == 'hydro' (== lac, river)
  # 0 polygons
  forestStands <- forestStands[forestStands$geolNotation != 'hydro', ]
  
  # remove forest plots where gPred == 0 & NA
  sum(is.na(forestStands$gPred))
  sum(forestStands$area[is.na(forestStands$gPred)])/100
  forestStands <- forestStands[!is.na(forestStands$gPred), ]

  # sum(forestStands$area[forestStands$gPred == 0])
  # polygons in a area not covered by lidar flight (13 polygons / 1800 ha)
  sum(forestStands$gPred==0)
  sum(forestStands$area[forestStands$gPred==0])/100
  forestStands <- forestStands[forestStands$gPred > 0, ]
  
  # remove forest plots where dgPred == 0
  # a few polygons, mostly in lowlands
  sum(is.na(forestStands$dgPred))
  sum(forestStands$dgPred==0)
  sum(forestStands$area[forestStands$dgPred==0])/100
  forestStands <- forestStands[!is.na(forestStands$dgPred), ]
  forestStands <- forestStands[forestStands$dgPred > 0, ]
  
  dim(forestStands)
  
  # convert mean BA/ha --> BA (real stock associated to each plot)
  forestStands$area <- raster::area(forestStands) / 10000
  forestStands$gPred <- forestStands$gPred * forestStands$area
  # forestStands$area <- NULL
  
  # remove plot where p100gfP = NA
  sum(is.na(forestStands$p100gfPred))
  forestStands <- forestStands[!is.na(forestStands$p100gfPred), ]
  
  # remove forest plots where owner ==  NA
  sum(is.na(forestStands$owner))
  sum(forestStands$area[is.na(forestStands$owner)])/100
  forestStands <- forestStands[!is.na(forestStands$owner), ]
  
  # concatenate accessibility
  forestStands$access <- paste('dist', round(forestStands$dist), 'harv', forestStands$nonHarv)
  
  # for 17 stands located inside or along the riverbed of Isere, there are no parcells in the cadastre
  # stand are removed
  sum(is.na(forestStands$meanParcelleArea))
  forestStands <- forestStands[!is.na(forestStands$meanParcelleArea), ]
  
  # remove useless columns
  forestStands$id <- NULL
  
  ###############################################################
  # save
  ###############################################################
  raster::shapefile(forestStands, filename = './data/forestStands', overwrite = TRUE)
} else {
  mihaiStands <- forestStands[, c("geolNotation", "Q7", "greco", "nonHarv", "dist", "alti", "slope", "gPred", "p100gfPred", "surface", "alti_mn","alti_mx", "alti_my")]
  mihaiStands$Gestion <- mihaiStands$Q7!="(Vous n\u0092avez pas effectué de coupe sur cette parcelle)"
  # point de sauvegarde
  #save(list=ls(), file="analyseMihai.rda")
  rm(list=ls())
  load(file="analyseMihai.rda")
  model.gam <- mgcv::gam(Gestion ~ surface +s(dist), data=mihaiStands@data,binomial)
  dev.new();mgcv::plot.gam(model.gam)
  # model.gam <- mgcv::gam(Gestion ~ surface +s(slope), data=mihaiStands@data,binomial)
  # mgcv::plot.gam(model.gam)
  # replace NA values in distance
  mihaiStands$dist[is.na(mihaiStands$dist)] <- 2000
  # model.glm <- glm(Gestion ~ I(surface * (dist - 500) * (dist < 2000) * nonHarv), data=mihaiStands@data,binomial)
  # model.glm <- glm(Gestion ~ surface + I((dist - 1000) * (dist < 2000) * nonHarv) + I((slope-40)*(slope<80)), data=mihaiStands@data,binomial)
  model.glm <- glm(Gestion ~ log(surface) + I((dist - 1000) * (dist < 2000)), data=mihaiStands@data,binomial)
  # model.glm <- glm(Gestion ~ surface + I((slope - 40) * (slope < 80)), data=mihaiStands@data,binomial)
  summary(model.glm)
  step(model.glm)
  save(model.glm, file=paste0(user$WorkingDir, "/data/modelGestion.rda"))
}


