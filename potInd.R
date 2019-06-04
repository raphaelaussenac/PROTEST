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

##########################################
########################## quercus petraea (03)

# known variables:   - rocheCalc
#                    - GRECO D
# unknown variables: - Intercept
#                    - tmin_12
#                    - CN_decor
#                    - de_7
#                    - swhc
#
# I(TYPE_GEOL == "Couverture sédimentaire") + LITHOLOGIE + CODE + yl93 + greco + expoEW + expoNS
modUnPa03 <- lm(unknownPart03 ~ xl93 + greco + pent2, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa03)

##########################################
########################## fagus sylvatica (09)

# known variables:   - alti
#                    - slope
#                    - GRECO B
# unknown variables: - Intercept
#                    - C/N
#                    - swhc
#                    - solD
#
# I(TYPE_GEOL == "Couverture sédimentaire") + LITHOLOGIE + CODE + yl93 + greco + expoEW + expoNS
modUnPa09 <- lm(unknownPart09 ~ xl93 + expoEW + pent2, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa09)

##########################################
########################## abies alba (61)

# known variables:   - GRECO A
#                    - GRECO H
# unknown variables: - Intercept
#                    - ETP June
#                    - RUM 1st horizon
#                    - C/N
#
# I(TYPE_GEOL == "Couverture sédimentaire") + LITHOLOGIE + CODE + yl93 + greco + expoEW + expoNS
modUnPa61 <- lm(unknownPart61 ~ alti + xl93 + pent2, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa61)

##########################################
########################## picea abies (62)

# known variables:   - slope
#                    - expoNS
#                    - rocheCalc
# unknown variables: - Intercept
#                    - tmin_2
#                    - swhc_A
#                    - bhc_5
#                    - C/N
#
# I(TYPE_GEOL == "Couverture sédimentaire") + LITHOLOGIE + CODE + yl93 + greco + expoEW + expoNS
modUnPa62 <- lm(unknownPart62 ~ alti + xl93 + yl93 + pent2, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa62)

###############################################################
# assign potential index to each plot (= centroid)
###############################################################

# load forest plots
topo_greco <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "topo_greco", encoding = "UTF-8", use_iconv = TRUE)
plot(topo_greco, col = topo_greco$CODE_TFV, border = topo_greco$CODE_TFV)
points(coordinates(topo_greco), pch = 16, cex = 0.5, col = "black")

# create df with variables used in the models
modDf <- data.frame(topo_greco[, c("ID", "alti", "slope", "GRECO", "expoNS", "expoEW")])

# add the coordinates of the polygons' centroids
coord <- data.frame(coordinates(topo_greco))
colnames(coord) <- c("X", "Y")
modDf <- cbind(modDf, coord)

##########################################
########################## quercus petraea (03)
# calculate the unknown part
modDf$unPa03 <- modUnPa03$coef["(Intercept)"] + (modUnPa03$coef["xl93"] * modDf$X) +
                                                (modUnPa03$coef["pent2"] * modDf$slope)

modDf[modDf$GRECO == "H", "unPa03"] <- modDf[modDf$GRECO == "H", "unPa03"] + modUnPa03$coef["grecoH"]

# calculate the known part#                                             !!!!!!!!!!!!!!!! ajouter roche caalcaire
modDf$knPa03 <- 0                                                       #   -11.097 * modDF$rocheCalc

# calculate the potential index
modDf$pot03 <- modDf$unPa03 + modDf$knPa03

##########################################
########################## fagus sylvatica (09)
# calculate the unknown part
modDf$unPa09 <- modUnPa09$coef["(Intercept)"] + (modUnPa09$coef["expoEW"] * modDf$expoEW) +
                                                (modUnPa09$coef["xl93"] * modDf$X) +
                                                (modUnPa09$coef["pent2"] * modDf$slope)

# calculate the known part
modDf$knPa09 <- -0.0083 * modDf$alti + -0.086 * modDf$slope

# calculate the potential index
modDf$pot09 <- modDf$unPa09 + modDf$knPa09

##########################################
########################## abies alba (61)
# calculate the unknown part
modDf$unPa61 <- modUnPa61$coef["(Intercept)"] + (modUnPa61$coef["alti"] * modDf$alti) +
                                                (modUnPa61$coef["xl93"] * modDf$X) +
                                                (modUnPa61$coef["pent2"] * modDf$slope)

# calculate the known part
modDf$knPa61 <- 0
modDf[modDf$GRECO =="H", "knPa61"] <- -11.56

# calculate the potential index
modDf$pot61 <- modDf$unPa61 + modDf$knPa61

##########################################
########################## picea abies (62)
# calculate the unknown part
modDf$unPa62 <- modUnPa62$coef["(Intercept)"] + (modUnPa62$coef["alti"] * modDf$alti) +
                                                (modUnPa62$coef["xl93"] * modDf$X) +
                                                (modUnPa62$coef["yl93"] * modDf$Y) +
                                                (modUnPa62$coef["pent2"] * modDf$slope)

# calculate the known part                                      # !!!!!!!!!!!!!!!! ajouter roche caalcaire
modDf$knPa62 <- -0.255 * modDf$slope + -3.705 * modDf$expoNS    #   -8.100 * modDF$rocheCalc

# calculate the potential index
modDf$pot62 <- modDf$unPa62 + modDf$knPa62

###############################################################
# integrate potential index into a shapefile
###############################################################

topo_greco@data <- cbind(topo_greco@data, modDf[, c("ID", "pot61", "pot62", "pot09", "pot03")])

# convert to export to ggplot
topo_greco@data$id = rownames(topo_greco@data)
topo_greco.points = fortify(topo_greco, region="id")
topo_greco.df = join(topo_greco.points, topo_greco@data, by="id")

########################## quercus petraea (03)
ggplot(topo_greco.df) +
  aes(long,lat,group=group,fill=pot03) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot03), name = "potential\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Quercus petraea") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))

########################## fagus sylvatica (09)
ggplot(topo_greco.df) +
  aes(long,lat,group=group,fill=pot09) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot09), name = "potential\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Fagus sylvatica") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))

########################## abies alba (61)
ggplot(topo_greco.df) +
  aes(long,lat,group=group,fill=pot61) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot61), name = "potential\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Abies balsamea") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))

########################## picea abies (62)
ggplot(topo_greco.df) +
  aes(long,lat,group=group,fill=pot62) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot62), name = "potential\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Picea abies") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))


###############################################################
# compare modeled potential index and IFN potential index
###############################################################

compare <- intersect(ptIFN, topo_greco)

########################## quercus petraea (03)
plot(compare$pot03, compare$potentiel_03)
abline(coef = c(0,1), col = "red")

hist(compare$pot03, breaks = seq(0, 55, 0.5), col = "grey")
hist(compare$potentiel_03, add = TRUE, breaks = seq(0, 55, 0.5), border = 'red')

plot(density(compare$pot03), xlim = c(0, 55))
lines(density(compare$potentiel_03), col = "red")

########################## fagus sylvatica (09)
plot(compare$pot09, compare$potentiel_09)
abline(coef = c(0,1), col = "red")

hist(compare$pot09, breaks = seq(5, 55, 0.5), col = "grey")
hist(compare$potentiel_09, add = TRUE, breaks = seq(5, 55, 0.5), border = 'red')

plot(density(compare$pot09), xlim = c(5, 55))
lines(density(compare$potentiel_09), col = "red")
lines(density(compare$potentiel_09, na.rm = TRUE), col = "red")

########################## abies alba (61)
plot(compare$pot61, compare$potentiel_61)
abline(coef = c(0,1), col = "red")

hist(compare$pot61, breaks = seq(5, 55, 0.5), col = "grey")
hist(compare$potentiel_61, add = TRUE, breaks = seq(5, 55, 0.5), border = 'red')

plot(density(compare$pot61), xlim = c(5, 55))
lines(density(compare$potentiel_61), col = "red")

########################## picea abies (62)
plot(compare$pot62, compare$potentiel_62)
abline(coef = c(0,1), col = "red")

hist(compare$pot62, breaks = seq(0, 100, 0.5), col = "grey")
hist(compare$potentiel_62, add = TRUE, breaks = seq(0, 100, 0.5), border = 'red')

plot(density(compare$pot62), xlim = c(5, 110))
lines(density(compare$potentiel_62), col = "red")
lines(density(compare$potentiel_62, na.rm = TRUE), col = "red")

###############################################################
# save new shp with climate data
###############################################################

# shapefile(topo_greco, filename = 'topo_greco_clim')









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
