###############################################################
# initialisation
###############################################################
# clean up environment
rm(list = ls())

# load packages
library(ggplot2)
library(plyr)
library(sf)

source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/modfertInd.R')

###############################################################
# assign potential index to each plot (= centroid)
###############################################################

# load forest plots
forestPlots <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "forestPlots3Ha", encoding = "UTF-8", use_iconv = TRUE)
# plot(forestPlots, col = forestPlots$CODE_TFV, border = forestPlots$CODE_TFV)
plot(coordinates(forestPlots), asp = 1)

# create df with variables used in the models
modDf <- data.frame(forestPlots[, c("SUPERID", "alti", "slope", "greco", "expoNS", "expoEW", "ph", "rum")])

# add the coordinates of the polygons' centroids
coord <- data.frame(coordinates(forestPlots))
colnames(coord) <- c("X", "Y")
modDf <- cbind(modDf, coord)

# max slope = 200 --> remove cliff effect
modDf[modDf$slope > 200, "slope"] <- 200

# plot area histogram
hist(area(forestPlots), breaks = 1000)
hist(area(forestPlots)[area(forestPlots)<20], breaks = 10)

###############################################################
########################## quercus petraea (03)
# calculate the unknown part
modDf$unPa03 <- modUnPa03$coef["(Intercept)"] + (modUnPa03$coef["X"] * modDf$X) +
                                                (modUnPa03$coef["slope"] * modDf$slope)

modDf[modDf$greco == "H", "unPa03"] <- modDf[modDf$greco == "H", "unPa03"] + modUnPa03$coef["grecoH"]

# calculate the known part#                                             !!!!!!!!!!!!!!!! ajouter roche caalcaire
modDf$knPa03 <- 0                                                       #   -11.097 * modDF$rocheCalc

# calculate the potential index
modDf$pot03 <- modDf$unPa03 + modDf$knPa03

###############################################################
########################## fagus sylvatica (09)
# calculate the unknown part
modDf$unPa09 <- modUnPa09$coef["(Intercept)"] + (modUnPa09$coef["expoEW"] * modDf$expoEW) +
                                                (modUnPa09$coef["X"] * modDf$X) +
                                                (modUnPa09$coef["slope"] * modDf$slope)

# calculate the known part
modDf$knPa09 <- -0.0083 * modDf$alti + -0.086 * modDf$slope

# calculate the potential index
modDf$pot09 <- modDf$unPa09 + modDf$knPa09

###############################################################
########################## abies alba (61)
# calculate the unknown part
modDf$unPa61 <- predict(modUnPa61, newdata = modDf)

# calculate the known part
modDf$knPa61 <- 0
modDf[modDf$greco =="H", "knPa61"] <- -11.56

# calculate the potential index
modDf$pot61 <- modDf$unPa61 + modDf$knPa61

# calculate the potential index with a random variation
modDf$pot61Epsilon <- modDf$unPa61 + modDf$knPa61 + rnorm(nrow(modDf), 0, sd(residuals(modUnPa61)))

###############################################################
########################## picea abies (62)
# calculate the unknown part
modDf$unPa62 <- modUnPa62$coef["(Intercept)"] + (modUnPa62$coef["alti"] * modDf$alti) +
                                                (modUnPa62$coef["X"] * modDf$X) +
                                                (modUnPa62$coef["Y"] * modDf$Y) +
                                                (modUnPa62$coef["slope"] * modDf$slope)

# calculate the known part                                      # !!!!!!!!!!!!!!!! ajouter roche caalcaire
modDf$knPa62 <- -0.255 * modDf$slope + -3.705 * modDf$expoNS    #   -8.100 * modDF$rocheCalc

# calculate the potential index
modDf$pot62 <- modDf$unPa62 + modDf$knPa62

###############################################################
# integrate potential index into a shapefile
###############################################################

forestPlots@data <- cbind(forestPlots@data, modDf[, c("pot61", "pot61Epsilon", "pot62", "pot09", "pot03", "expoNS", "expoEW")])

# convert to export to ggplot
forestPlots@data$id <- rownames(forestPlots@data)
forestPlotsPts <- fortify(forestPlots, region="id")
forestPlotsDf <- join(forestPlotsPts, forestPlots@data, by="id")

########################## quercus petraea (03)
# fertility map
ggplot(forestPlotsDf) +
  aes(long,lat,group=group,fill=pot03) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot03), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Quercus petraea") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/fertilty03.pdf", plot = last_plot())

# residuals map
res <-data.frame(residuals(modUnPa03))
colnames(res) <- "residuals"
res$sign <- "positive"
res[res$res < 0, "sign"] <- "negative"
bdBauges03 <- cbind(bdBauges03, res)

ggplot() +
  geom_polygon(data = forestPlotsDf, aes(long,lat,group=group,fill=pot03), alpha = 0.08) +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot03), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Quercus petraea") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  geom_point(data = bdBauges03, aes(X, Y, size = sqrt(residuals^2), color = sign), alpha = 0.5) +
  geom_point(data = bdBauges, aes(X, Y, shape = ifnPt), size = 0.5) +
  guides(fill = FALSE, size = guide_legend(title = "residuals"), color = guide_legend(title="sign"), shape = guide_legend(title = ""))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/residuals03.pdf", plot = last_plot())

########################## fagus sylvatica (09)
# fertility map
ggplot(forestPlotsDf) +
  aes(long,lat,group=group,fill=pot09) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot09), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Fagus sylvatica") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/fertilty09.pdf", plot = last_plot())

# residuals map
res <-data.frame(residuals(modUnPa09))
colnames(res) <- "residuals"
res$sign <- "positive"
res[res$res < 0, "sign"] <- "negative"
bdBauges09 <- cbind(bdBauges09, res)

ggplot() +
  geom_polygon(data = forestPlotsDf, aes(long,lat,group=group,fill=pot09), alpha = 0.08) +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot09), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Fagus sylvatica") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  geom_point(data = bdBauges09, aes(X, Y, size = sqrt(residuals^2), color = sign), alpha = 0.5) +
  geom_point(data = bdBauges, aes(X, Y, shape = ifnPt), size = 0.5) +
  guides(fill = FALSE, size = guide_legend(title = "residuals"), color = guide_legend(title="sign"), shape = guide_legend(title = ""))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/residuals09.pdf", plot = last_plot())

########################## abies alba (61)
# fertility map
ggplot() +
  geom_polygon(data = forestPlotsDf, aes(long,lat,group=group,fill=pot61)) +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot61), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Abies alba") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/fertilty61.pdf", plot = last_plot())

# residuals map
res <-data.frame(residuals(modUnPa61))
colnames(res) <- "residuals"
res$sign <- "positive"
res[res$res < 0, "sign"] <- "negative"
bdBauges61 <- cbind(bdBauges61, res)

ggplot() +
  geom_polygon(data = forestPlotsDf, aes(long,lat,group=group,fill=pot61), alpha = 0.08) +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot61), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Abies alba") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  geom_point(data = bdBauges61, aes(X, Y, size = sqrt(residuals^2), color = sign), alpha = 0.5) +
  geom_point(data = bdBauges, aes(X, Y, shape = ifnPt), size = 0.5) +
  guides(fill = FALSE, size = guide_legend(title = "residuals"), color = guide_legend(title="sign"), shape = guide_legend(title = ""))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/residuals61.pdf", plot = last_plot())

########################## picea abies (62)
# fertility map
ggplot(forestPlotsDf) +
  aes(long,lat,group=group,fill=pot62) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot62), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Picea abies") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/fertilty62.pdf", plot = last_plot())

# residuals map
res <-data.frame(residuals(modUnPa62))
colnames(res) <- "residuals"
res$sign <- "positive"
res[res$res < 0, "sign"] <- "negative"
bdBauges62 <- cbind(bdBauges62, res)

ggplot() +
  geom_polygon(data = forestPlotsDf, aes(long,lat,group=group,fill=pot62), alpha = 0.08) +
  coord_equal() +
  scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot62), name = "fertility\nindex\n(modeled)") +
  theme_bw() +
  ggtitle("Picea abies") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  geom_point(data = bdBauges62, aes(X, Y, size = sqrt(residuals^2), color = sign), alpha = 0.5) +
  geom_point(data = bdBauges, aes(X, Y, shape = ifnPt), size = 0.5) +
  guides(fill = FALSE, size = guide_legend(title = "residuals"), color = guide_legend(title="sign"), shape = guide_legend(title = ""))

ggsave("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/residuals62.pdf", plot = last_plot())

###############################################################
# compare modeled potential index and IFN potential index
###############################################################

########################## density plots
# density observed vs predicted
d <- density(ifnCircular$ptnt_61)
plot(d, xlim = c(-15, 60), ylim = c(0, 0.1), lwd = 2, main = "density observed vs modeled vs modeled + noise")
polygon(d, col="grey", border="black")
lines(density(forestPlots$pot61), col = "red", lwd = 2)
lines(density(forestPlots$pot61Epsilon), col = "blue", lwd = 2)

########################## scatter plots
# create an intersect between ptIFN and forestPlots
compare <- intersect(forestPlots, ifnCircular)

# there is more lines in the compare table (288) than there is NFI points
# in the study area because some of the NFI circular plots overlap with
# several forest plots
# has no effect
# in that case there are 2 predicted fertility index (ex: pot61) for 1 fertility
# index coming from the NFI database --> there are therefore 2 points on the plot

plot(forestPlots[forestPlots$id == 4520,], col = 'orange')
plot(forestPlots[forestPlots$id == 2778,], add = TRUE, col = "green4")
plot(ifnCircular[ifnCircular$idp == 100733,], add = TRUE, col = 'black')

########################## quercus petraea (03)


########################## fagus sylvatica (09)


########################## abies alba (61)
# plot observed vs predicted
xymin <- round(min(range(compare$pot61), range(compare$ptnt_61), range(compare$pot61Epsilon)))-1
xymax <- round(max(range(compare$pot61), range(compare$ptnt_61), range(compare$pot61Epsilon)))+1
plot(compare$pot61, compare$ptnt_61, pch = 16, main = "observed vs predicted (without random noise)", xlim = c(xymin, xymax), ylim = c(xymin, xymax))
abline(coef = c(0,1), col = "red")
plot(compare$pot61Epsilon, compare$ptnt_61, pch = 16, main = "observed vs predicted (with random noise)", xlim = c(xymin, xymax), ylim = c(xymin, xymax), col = "green")
abline(coef = c(0,1), col = "red")
plot(compare$pot61, compare$ptnt_61, col = ifelse(compare$greco.1 == "H", "blue", "red"), pch = 16, main = "observed vs predicted (without random noise)", xlim = c(xymin, xymax), ylim = c(xymin, xymax))
abline(coef = c(0,1), col = "red")

########################## picea abies (62)







# # residuals
# summary(lm(residuals(modPot02)~bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"]))
# plot(residuals(modPot02)~bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"]); abline(h = 0, lty = 2)
# panel.smooth(bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),"alti"], residuals(modTmin2), span = 0.5)
#
# # effets sur la variable clim
# # x = continentalité
# (range(bdBauges$X)[2] - range(bdBauges$X)[1]) * modTmin2$coefficients["X"]
# # y = lattitude
# (range(bdBauges$Y)[2] - range(bdBauges$Y)[1]) * modTmin2$coefficients["Y"]
# # alti
# (range(bdBauges$alti)[2] - range(bdBauges$alti)[1]) * modTmin2$coefficients["alti"]
# # expoEW
# (range(bdBauges$expoEW, na.rm = TRUE)[2] - range(bdBauges$expoEW, na.rm = TRUE)[1]) * modTmin2$coefficients["expoEW"]
# # expoNS
# (range(bdBauges$expoNS, na.rm = TRUE)[2] - range(bdBauges$expoNS, na.rm = TRUE)[1]) * modTmin2$coefficients["expoNS"]
