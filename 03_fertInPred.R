###############################################################
# initialisation
###############################################################
# clean up environment
rm(list = ls())

# load packages
library(ggplot2)
library(plyr)
library(sf)
library(gridExtra)

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

# plot
plotMap <- function(sp){
  # manage colnames
  ifn <- ifnCircular@data
  colnames(ifn)[colnames(ifn) == paste('ptnt_', sp, sep ='')] <- 'ptnt'
  forest <- forestPlots@data
  colnames(forest)[colnames(forest) == paste('pot', sp, sep ='')] <- 'pot'
  colnames(forest)[colnames(forest) == paste('pot', sp, 'Epsilon', sep ='')] <- 'potEpsilon'
  if (sp == "03"){
    mod <- modUnPa03
    bd <- bdBauges03
    ti <- "Quercus petraea"
  } else if(sp == "09"){
    mod <- modUnPa09
    bd <- bdBauges09
    ti <- "Fagus sylvatica"
  } else if(sp == "61"){
    mod <- modUnPa61
    bd <- bdBauges61
    ti <- "Abies alba"
  } else if(sp == "62"){
    mod <- modUnPa62
    bd <- bdBauges62
    ti <- "Picea abies"
  }
  colnames(bd)[colnames(bd) == paste('pot', sp, sep ='')] <- 'pot'
  colnames(bd)[colnames(bd) == paste('ptnt_', sp, sep ='')] <- 'ptnt'
  colnames(bd)[colnames(bd) == paste('pot', sp, 'Epsilon', sep ='')] <- 'potEpsilon'
  forestDf <- forestPlotsDf
  colnames(forestDf)[colnames(forestDf) == paste('pot', sp, sep ='')] <- 'pot'
  colnames(forestDf)[colnames(forestDf) == paste('pot', sp, 'Epsilon', sep ='')] <- 'potEpsilon'
  mDf <- modDf
  colnames(mDf)[colnames(mDf) == paste('pot', sp, sep ='')] <- 'pot'

  # density observed vs predicted
  dens <- ggplot() +
    geom_density(data = ifn, aes(ptnt), col = 'black', fill = 'grey') +
    geom_density(data = forest, aes(pot), col = 'red') +
    geom_density(data = forest, aes(potEpsilon), col = 'blue') +
    annotate("text", label = paste("R² =", round(summary(mod)$r.squared,3)), x = 5, y = 0.02, size = 5) +
    theme_bw()

  # obs vs pred
  mn <- min(bd$pot, bd$ptnt)
  mx <- max(bd$pot, bd$ptnt)
  ovsp <- ggplot() +
    geom_point(data = bd, aes(pot, ptnt)) + # , col = greco
    xlim(mn, mx) +
    ylim(mn, mx) +
    geom_abline(slope = 1, col = 'red') +
    theme_bw()

  # obs vs pred + E
  mn <- min(bd$potEpsilon, bd$ptnt)
  mx <- max(bd$potEpsilon, bd$ptnt)
  ovspE <- ggplot() +
    geom_point(data = bd, aes(potEpsilon, ptnt)) + # , col = greco
    xlim(mn, mx) +
    ylim(mn, mx) +
    geom_abline(slope = 1, col = 'red') +
    theme_bw()

  # fertility map
  fert <- ggplot() +
    geom_polygon(data = forestDf, aes(long,lat,group=group,fill=pot)) +
    coord_equal() +
    scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(mDf$pot), name = "fertility\nindex\n(modeled)") +
    ggtitle(ti) +
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position="right",
            axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())

  fertE <- ggplot() +
    geom_polygon(data = forestDf, aes(long,lat,group=group,fill=potEpsilon)) +
    coord_equal() +
    scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(mDf$pot), name = "fertility\nindex\n(modeled)") +
    ggtitle(paste(ti, " + E")) +
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position="right",
            axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())

  # residuals map
  res <-data.frame(residuals(mod))
  colnames(res) <- "residuals"
  res$sign <- "positive"
  res[res$res < 0, "sign"] <- "negative"
  bd <- cbind(bd, res)

  fertRes <- ggplot() +
    geom_polygon(data = forestPlotsDf, aes(long,lat,group=group,fill=pot61), alpha = 0.08) +
    coord_equal() +
    scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(modDf$pot61), name = "fertility\nindex\n(modeled)") +
    ggtitle(paste(ti, "res")) +
    geom_point(data = bd, aes(X, Y, size = sqrt(residuals^2), color = sign), alpha = 0.5) +
    geom_point(data = bdBauges, aes(X, Y, shape = ifnPt), size = 0.5) +
    guides(fill = FALSE, size = guide_legend(title = "residuals"), color = guide_legend(title="sign"), shape = guide_legend(title = "")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position="right",
            axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())

  # print(grid.arrange(dens, ovsp, ovspE, fert, fertE, fertRes, nrow = 2, ncol = 3))
  print(grid.arrange(dens, ovsp, ovspE, nrow = 1, ncol = 3))
  print(grid.arrange(fert, fertE, fertRes, nrow = 1, ncol = 3))
}

# model quercus petraea (03)
pdf(file="C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/map03.pdf", width = 20, height = 10)
plotMap("03")
dev.off()

# model fagus sylvatica  (09)
pdf(file="C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/map09.pdf", width = 20, height = 10)
plotMap("09")
dev.off()

# model abies alba (61)
pdf(file="C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/map61.pdf", width = 20, height = 10)
plotMap("61")
dev.off()

# model picea abies (62)
pdf(file="C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/map62.pdf", width = 20, height = 10)
plotMap("62")
dev.off()






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
