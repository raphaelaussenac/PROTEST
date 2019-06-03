###############################################################
# initialisation
###############################################################
# clean up environment
rm(list = ls())

# load packages
library(raster)
library(rgdal)
library(ggplot2)
library(plyr)

###############################################################
# import IFN points & their potential index
###############################################################

# load IFN points
source('Z:/Private/Calcul_Potentiels/Calcul_Potentiels_Purs.R')

# load a mask of the study area
PNR <- readOGR(dsn = "Z:/Private/PNR Bauges/Sans_Trou", layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)
plot(PNR)
points(bd$xl93, bd$yl93, pch = 16, col = 'black')

# select points in the study area
ptIFN <- SpatialPointsDataFrame(bd[,c("xl93", "yl93")], data = data.frame(bd), proj4string = CRS(proj4string(PNR)))
proj4string(ptIFN)
proj4string(PNR)
baugesIFN <-over(ptIFN, PNR)
baugesIFN <- droplevels(baugesIFN[!is.na(baugesIFN$ID),])
bdBauges <- droplevels(bd[as.numeric(row.names(baugesIFN)),])
points(bdBauges$xl93, bdBauges$yl93, pch = 16, col = 'black', cex = 2)

# assign geological data to each point
geol <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "geol", encoding = "UTF-8", use_iconv = TRUE)
plot(geol, col = geol$CODE, border = geol$CODE, add = TRUE)
IFNgeol <- SpatialPointsDataFrame(bdBauges[,c("xl93", "yl93")], data = data.frame(bdBauges), proj4string = CRS(proj4string(PNR)))
IFNgeol <- intersect(IFNgeol, geol)
bdBauges <- IFNgeol@data

# import  BDforet?

###############################################################
# model unknown part of potential index
###############################################################

########################## abies alba (61)

# known variables:   - GRECO A
#                    - GRECO H
# unknown variables: - Intercept
#                    - ETP June
#                    - RUM 1st horizon
#                    - C/N
#
# I(TYPE_GEOL == "Couverture sédimentaire") + LITHOLOGIE + CODE + yl93 + greco + expoEW + expoNS
modUnPa61 <- lm(unknownPart61 ~ alti + xl93 + pent2 + LITHOLOGIE, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa61)


# # residuals
# summary(lm(residuals(modPot02)~bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"]))
# plot(residuals(modPot02)~bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"]); abline(h = 0, lty = 2)
# panel.smooth(bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"], residuals(modTmin2), span = 0.5)
#
# # effets sur la variable clim
# # x = continentalité
# (range(bdBauges$xl93)[2] - range(bdBauges$xl93)[1]) * modTmin2$coefficients["xl93"]
# # y = lattitude
# (range(bdBauges$yl93)[2] - range(bdBauges$yl93)[1]) * modTmin2$coefficients["yl93"]
# # alti
# (range(bdBauges$alti)[2] - range(bdBauges$alti)[1]) * modTmin2$coefficients["alti"]
# # expoEW
# (range(bdBauges$expoEW, na.rm = TRUE)[2] - range(bdBauges$expoEW, na.rm = TRUE)[1]) * modTmin2$coefficients["expoEW"]
# # expoNS
# (range(bdBauges$expoNS, na.rm = TRUE)[2] - range(bdBauges$expoNS, na.rm = TRUE)[1]) * modTmin2$coefficients["expoNS"]

###############################################################
# assign potential index to each plot (= centroid)
###############################################################

# load forest plots
topo_greco <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "topo_greco", encoding = "UTF-8", use_iconv = TRUE)
plot(topo_greco, col = topo_greco$CODE_TFV, border = topo_greco$CODE_TFV)
points(coordinates(topo_greco), pch = 16, cex = 0.5, col = "black")

# create df with variables used in the models
modDf <- data.frame(topo_greco[, c("ID", "alt", "slope", "GRECO")])

# add the coordinates of the polygons' centroids
coord <- data.frame(coordinates(topo_greco))
colnames(coord) <- c("X", "Y")
modDf <- cbind(modDf, coord)

########################## abies alba (61)
# calculate the unknown part
modDf$unPa61 <- modUnPa61$coef["(Intercept)"] + (modUnPa61$coef["alti"] * modDf$alt) +
                                              (modUnPa61$coef["xl93"] * modDf$X) +
                                              (modUnPa61$coef["pent2"] * modDf$slope)

# calculate the known part
modDf$knPa61 <- 0
modDf[modDf$GRECO =="H", "knPa61"] <- -11.56

# calculate the potential index
modDf$pot61 <- modDf$unPa61 + modDf$knPa61

###############################################################
# integrate climate variables into a shapefile
###############################################################

topo_greco@data <- cbind(topo_greco@data, modDf[, c("ID", "pot61")])

# convert to export to ggplot
topo_greco@data$id = rownames(topo_greco@data)
topo_greco.points = fortify(topo_greco, region="id")
topo_greco.df = join(topo_greco.points, topo_greco@data, by="id")

ggplot(topo_greco.df) +
  aes(long,lat,group=group,fill=pot61) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "white", mid = "blue", high = "green", aesthetics = "fill", midpoint = 25)
  # scale_fill_gradientn(colours = terrain.colors(10))

###############################################################
# compare modeled potential index and IFN potential index
###############################################################

aa <- intersect(ptIFN, topo_greco)
plot(aa$pot61, aa$potentiel_61)
abline(coef = c(0,1), col = "red")

hist(aa$pot61, col = "grey", breaks = seq(0, 60, 2.5))
hist(aa$potentiel_61, add = TRUE, border = 'red', breaks = seq(0, 60, 2.5))


###############################################################
# save new shp with climate data
###############################################################

# shapefile(topo_greco, filename = 'topo_greco_clim')
