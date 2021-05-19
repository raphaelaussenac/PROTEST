###############################################################
# initialisation
###############################################################

# clean up environment except user variable with path values
rm(list = setdiff(ls(),"user"))
user$checkSurfaces <- FALSE

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

# load forestStands
if (!user$mihai)
{
  load(file="./data/forestStands03b.rda")
} else {
  load(file="./data/forestStandsMihai03b.rda")
}
# convert to sf object
forestStands$`WKT-GEOM` <- as.character(forestStands$`WKT-GEOM`)
forestStands <- sf::st_sf(forestStands[,-which(names(forestStands)=="WKT-GEOM")], geom=sf::st_as_sfc(forestStands$`WKT-GEOM`))
sf::st_crs(forestStands) <- 2154

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
# cadastre
#
# prepare raster file of cadastre (done once)
if (0)
{
  cadastreRaster <- list()
  # load cadastre in 73 and 74
  for (i in c(73,74))
  {
    # load vector layer of cadastre
    cadastre <- sf::st_read(paste0("/media/reseau/infogeo/ign/BD_PARCELLAIRE/V1.2/BDPV_1-2_SHP_LAMB93_D0",i,"_2014/PARCELLE.SHP"), quiet=TRUE)
    # add area field
    cadastre$AREA <- sf::st_area(cadastre)
    # rasterize area
    cadastreRaster[[i]] <- fasterize::fasterize(cadastre, elev, field="AREA")
  }
  cadastreR <- raster::merge(cadastreRaster[[73]], cadastreRaster[[74]])
  cadastreR@crs <- pnr@proj4string
  raster::plot(cadastreR)
  # export rasterized cadastre
  raster::writeRaster(cadastreR, file=paste0(user$NetworkProtestDir, "T1/Donnees_SIG/Cadastre/cadastreAREA.tif"))
}
#
# load rasterized cadastre
cadastreR <- raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/Cadastre/cadastreAREA.tif"))

###############################################################
# land ownership
# 
# load ownership raster
ownership <- raster::raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/BDForetv2_foret_PNRfilled_propriete_5m.tif"))
# plot(ownership, col=colorRampPalette(c("green", "blue"))(255))

###############################################################
# accessibility
# loaded just before extraction to avoid memory swap

###############################################################
# extract values for forest stands
###############################################################

# Create a function to calculate the mode # could use "names(sort(table(variable),decreasing=TRUE))[1])" but numeric values are converted to text
# Each plot is assigned the most represented value
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

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
#
#
ownershipExtDf <- data.frame(owner=ownershipExt)
ownershipExtDf[ownershipExtDf$owner == 2 & !is.na(ownershipExtDf$owner), 'owner'] <- 'Pub'
ownershipExtDf[ownershipExtDf$owner == 1 & !is.na(ownershipExtDf$owner), 'owner'] <- 'Priv'
forestStands$owner <- as.factor(ownershipExtDf$owner)
#
#cadastreExtDf <- data.frame(meanParcelleArea=standMeanParcellAreaExt)
#nonHarvExtDf <- data.frame(nonHarv=nonHarvExt)
#distExtDf <- data.frame(dist=distExt)
forestStands$meanParcelleArea <- as.vector(standMeanParcellAreaExt)
forestStands$nonHarv <- as.vector(nonHarvExt)
forestStands$dist <- as.vector(distExt)

# integrate into the shp file
# forestStands <- cbind(forestStands, ownershipExtDf,
#                            nonHarvExtDf, distExtDf, cadastreExtDf)#, 
# ,ggbPredExtDf, forProtecExtDf, kExtDf, lsExtDf, pExtDf, rExtDf)
rm(list=ls(pattern="*Ext"))

# concatenate accessibility
forestStands$access <- paste('dist', round(forestStands$dist), 'harv', forestStands$nonHarv)

# compute area
forestStands$area <- sf::st_area(forestStands)



if (!user$mihai)
{
  # remove forest plots where owner ==  NA (4, 26m2)
  sum(is.na(forestStands$owner))
  sum(forestStands$area[is.na(forestStands$owner)])
  forestStands <- forestStands[!is.na(forestStands$owner), ]
  
  # for 18 stands (16.9 ha) located inside or along the riverbed of Isere, there are no parcells in the cadastre
  # stand are removed
  sum(is.na(forestStands$meanParcelleArea))
  sum(forestStands$area[is.na(forestStands$meanParcelleArea)])
  forestStands <- forestStands[!is.na(forestStands$meanParcelleArea), ]
  #
  # save
  save(forestStands, file="./data/forestStands03c.rda")
} else {
  save(forestStands, file="./data/forestStandsMihai03c.rda")
}


# ############################
# # modelisation de la gestion
# if (user$mihai) {
#   mihaiStands <- forestStands[, c("geolNotation", "Q7", "greco", "nonHarv", "dist", "alti", "slope", "gPred", "p100gfPred", "surface", "alti_mn","alti_mx", "alti_my")]
#   mihaiStands$Gestion <- mihaiStands$Q7!="(Vous n\u0092avez pas effectué de coupe sur cette parcelle)"
#   # point de sauvegarde
#   #save(list=ls(), file="analyseMihai.rda")
#   rm(list=ls())
#   load(file="analyseMihai.rda")
#   model.gam <- mgcv::gam(Gestion ~ surface +s(dist), data=mihaiStands@data,binomial)
#   dev.new();mgcv::plot.gam(model.gam)
#   # model.gam <- mgcv::gam(Gestion ~ surface +s(slope), data=mihaiStands@data,binomial)
#   # mgcv::plot.gam(model.gam)
#   # replace NA values in distance
#   mihaiStands$dist[is.na(mihaiStands$dist)] <- 2000
#   # model.glm <- glm(Gestion ~ I(surface * (dist - 500) * (dist < 2000) * nonHarv), data=mihaiStands@data,binomial)
#   # model.glm <- glm(Gestion ~ surface + I((dist - 1000) * (dist < 2000) * nonHarv) + I((slope-40)*(slope<80)), data=mihaiStands@data,binomial)
#   model.glm <- glm(Gestion ~ log(surface) + I((dist - 1000) * (dist < 2000)), data=mihaiStands@data,binomial)
#   # model.glm <- glm(Gestion ~ surface + I((slope - 40) * (slope < 80)), data=mihaiStands@data,binomial)
#   summary(model.glm)
#   step(model.glm)
#   save(model.glm, file=paste0(user$WorkingDir, "/data/modelGestion.rda"))
# }

