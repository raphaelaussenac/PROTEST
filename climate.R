###############################################################
# initialisation
###############################################################
# clean up environment
rm(list = ls())

# load packages
library(raster)
library(rgdal)

###############################################################
# IFN points extraction
###############################################################

# load IFN points
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/Script_Lecture_IFN_2005_2016.R')

# load a mask of the study area
PNR <- readOGR(dsn = "Z:/Private/PNR Bauges/Sans_Trou", layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)
plot(PNR)
points(bd$xl93, bd$yl93, pch = 16, col = 'red')

# select points in the study area
spIFN <- SpatialPointsDataFrame(bd[,c("xl93", "yl93")], data = data.frame(id=bd$idp), proj4string = CRS(proj4string(PNR)))
proj4string(spIFN)
proj4string(PNR)
res <-over(spIFN, PNR)
res <- droplevels(res[!is.na(res$ID),])
bdBauges <- droplevels(bd[as.numeric(row.names(res)),])
points(bdBauges$xl93, bdBauges$yl93, pch = 16, col = 'green')

###############################################################
# modeling climate variables
###############################################################

bdBauges <- data.frame(bdBauges)

########################## T° min fév

modTmin2 <- lm(tmin_2 ~ expoEW + expoNS + alti + xl93 + yl93, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modTmin2)

# residuals
summary(lm(residuals(modTmin2)~bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"]))
plot(residuals(modTmin2)~bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"]); abline(h = 0, lty = 2)
panel.smooth(bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"], residuals(modTmin2), span = 0.5)

# effets sur la variable clim
# x = continentalité
(range(bdBauges$xl93)[2] - range(bdBauges$xl93)[1]) * modTmin2$coefficients["xl93"]
# y = lattitude
(range(bdBauges$yl93)[2] - range(bdBauges$yl93)[1]) * modTmin2$coefficients["yl93"]
# alti
(range(bdBauges$alti)[2] - range(bdBauges$alti)[1]) * modTmin2$coefficients["alti"]
# expoEW
(range(bdBauges$expoEW, na.rm = TRUE)[2] - range(bdBauges$expoEW, na.rm = TRUE)[1]) * modTmin2$coefficients["expoEW"]
# expoNS
(range(bdBauges$expoNS, na.rm = TRUE)[2] - range(bdBauges$expoNS, na.rm = TRUE)[1]) * modTmin2$coefficients["expoNS"]


########################## T° min déc

########################## ETP juin

########################## Bilan hydrique mai

########################## Deficit eau juil.


# 2 - faire tourner le modele et récupèrer les coefs
#  -- interactions + quadratique
# simple
# quadratique (si suggeré par les résidus)
# interaction
# 3 - récupèrer les coordonnées des centroides des polygones
# 4 - calculer les variables clim pour chaque parcelle


###############################################################
# assign climate value to each plot (= centroid)
###############################################################

# load forest plots
topo_greco <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "topo_greco", encoding = "UTF-8", use_iconv = TRUE)
plot(topo_greco, col = topo_greco$CODE_TFV, border = topo_greco$CODE_TFV)
points(coordinates(topo_greco), pch = 16, cex = 0.5, col = "red")

# create df with variables used in the models
modDf <- data.frame(topo_greco[, c("ID", "alt", "orient")])

# add the coordinates of the polygons' centroids
coord <- data.frame(coordinates(topo_greco))
colnames(coord) <- c("X", "Y")
modDf <- cbind(modDf, coord)

# add modeled variables
modDf$Tmin2 <- modTmin2$coef["(Intercept)"] + (modTmin2$coef["expoNS"] * modDf$orient) +
                                              (modTmin2$coef["alti"] * modDf$alt) +
                                              (modTmin2$coef["xl93"] * modDf$X) +
                                              (modTmin2$coef["yl93"] * modDf$Y)


###############################################################
# integrate climate variables into a shapefile
###############################################################

topo_greco@data <- cbind(topo_greco@data, modDf[, c("ID", "Tmin2")])

###############################################################
# save new shp with climate data
###############################################################

# shapefile(topo_greco, filename = 'topo_greco_clim')
