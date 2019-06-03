###############################################################
# initialisation
###############################################################
# clean up environment
rm(list = ls())

# load packages
library(raster)
library(rgdal)

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
# I(TYPE_GEOL == "Couverture sédimentaire") + LITHOLOGIE + CODE +
modUnPaAa <- lm(unknownPart61 ~ expoEW + expoNS + alti + xl93 + yl93 + greco + pent2, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPaAa)

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
points(coordinates(topo_greco), pch = 16, cex = 0.5, col = "red")


# assign geological data to each point
# ou dans extract?



# create df with variables used in the models
modDf <- data.frame(topo_greco[, c("ID", "alt", "expoNS")])

# add the coordinates of the polygons' centroids
coord <- data.frame(coordinates(topo_greco))
colnames(coord) <- c("X", "Y")
modDf <- cbind(modDf, coord)

# add modeled variables
modDf$Pot02 <- modPot02$coef["(Intercept)"] + (modPot02$coef["expoNS"] * modDf$expoNS) +
                                              (modPot02$coef["alti"] * modDf$alt) +
                                              (modPot02$coef["xl93"] * modDf$X) +
                                              (modPot02$coef["yl93"] * modDf$Y)


###############################################################
# integrate climate variables into a shapefile
###############################################################

topo_greco@data <- cbind(topo_greco@data, modDf[, c("ID", "Pot02")])

###############################################################
# save new shp with climate data
###############################################################

# shapefile(topo_greco, filename = 'topo_greco_clim')
