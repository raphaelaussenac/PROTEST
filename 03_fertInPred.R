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
# assign potential index to each forest plot (= centroid)
###############################################################

# load forest plots
forestPlots <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "forestPlots3Ha", encoding = "UTF-8", use_iconv = TRUE)
# plot(forestPlots, col = forestPlots$CODE_TFV, border = forestPlots$CODE_TFV)
plot(coordinates(forestPlots), asp = 1)

# create df with variables used in the models
modDf <- data.frame(forestPlots[, c("SUPERID", "alti", "slope", "greco", "expoNS", "expoEW", "ph", "rum")])

# add the coordinates of the polygons' centroids (since X and Y may be used
# in the models)
coord <- data.frame(coordinates(forestPlots))
colnames(coord) <- c("X", "Y")
modDf <- cbind(modDf, coord)

# max slope = 200 --> remove cliff effect
modDf[modDf$slope > 200, "slope"] <- 200

# plot area histogram
hist(area(forestPlots), breaks = 1000)
# hist(area(forestPlots)[area(forestPlots)<20], breaks = 10)

# predict fertility index
modDf <- predFert(modUnPa61, modDf, "03")
modDf <- predFert(modUnPa61, modDf, "09")
modDf <- predFert(modUnPa61, modDf, "61")
modDf <- predFert(modUnPa61, modDf, "62")

###############################################################
# control model quality and create fertility maps
###############################################################

forestPlots@data <- cbind(forestPlots@data, modDf[,c('pot03', 'pot03Epsilon',
                                            'pot09', 'pot09Epsilon', 'pot61',
                                            'pot61Epsilon', 'pot62',
                                            'pot62Epsilon')])


# convert to export to ggplot
forestPlots@data$id <- rownames(forestPlots@data)
forestPlotsPts <- fortify(forestPlots, region="id")
forestPlotsDf <- join(forestPlotsPts, forestPlots@data, by="id")

########################## abies alba (61)
# density observed vs predicted
ggplot() +
  geom_density(data = ifnCircular@data, aes(ptnt_61), col = 'black', fill = 'grey') +
  geom_density(data = forestPlots@data, aes(pot61), col = 'red') +
  geom_density(data = forestPlots@data, aes(pot61Epsilon), col = 'blue') +
  theme_bw()

# obs vs pred
ggplot() +
  geom_point(data = bdBauges61, aes(pot61, ptnt_61, col = greco)) +
  geom_abline(slope = 1, col = 'red') +
  theme_bw()

ggplot() +
  geom_point(data = bdBauges61, aes(pot61Epsilon, ptnt_61, col = greco)) +
  geom_abline(slope = 1, col = 'red') +
  theme_bw()

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
