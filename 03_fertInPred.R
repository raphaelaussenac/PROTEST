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

# set model method
chosenMethod <- 'direct' # 'knPa-unPa' or 'direct'

# load models
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/modfertInd.R')

###############################################################
# prediction : assign potential index to each forest plot
###############################################################

# load forest plots
forestPlots <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "forestPlots3Ha", encoding = "UTF-8", use_iconv = TRUE)
# plot(forestPlots, col = forestPlots$CODE_TFV, border = forestPlots$CODE_TFV)
plot(coordinates(forestPlots), asp = 1)

# add the coordinates of the polygons' centroids (since X and Y may be used
# in the models)
coord <- data.frame(coordinates(forestPlots))
colnames(coord) <- c("X", "Y")
forestPlots@data <- cbind(forestPlots@data, coord)

# remove polygones with geol == 'hydro' (== lac, river)
forestPlots <- forestPlots[forestPlots$gelNttn != 'hydro', ]

# create df with variables used in the models
modDf <- forestPlots@data
modDf <- modDf[, c("alti", "slope", "greco", "expoNS", "expoEW", "ph", "rum", 'gelNttn' ,'Cd_crbn', 'Cd_hydr', 'rochClc', 'X', 'Y')]

# max slope = 200 --> remove cliff effect
modDf[modDf$slope > 200, "slope"] <- 200

# plot area histogram
hist(area(forestPlots), breaks = 1000)
# hist(area(forestPlots)[area(forestPlots)<20], breaks = 10)

# predict fertility index
modDf <- predFert(modUnPa03, modDf, "03", 'prediction', chosenMethod)
modDf <- predFert(modUnPa09, modDf, "09", 'prediction', chosenMethod)
modDf <- predFert(modUnPa61, modDf, "61", 'prediction', chosenMethod)
modDf <- predFert(modUnPa62, modDf, "62", 'prediction', chosenMethod)

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
  if (sp == "03"){
    mod <- modUnPa03
    bd <- bdBauges03
    ti <- "Q. petraea"
    ratEpsiPot <- varPar[ti, "ratEpsiPot"]
  } else if(sp == "09"){
    mod <- modUnPa09
    bd <- bdBauges09
    ti <- "F. sylvatica"
    ratEpsiPot <- varPar[2, "ratEpsiPot"]
  } else if(sp == "61"){
    mod <- modUnPa61
    bd <- bdBauges61
    ti <- "A. alba"
    ratEpsiPot <- varPar[3, "ratEpsiPot"]
  } else if(sp == "62"){
    mod <- modUnPa62
    bd <- bdBauges62
    ti <- "P. abies"
    ratEpsiPot <- varPar[4, "ratEpsiPot"]
  }
  colnames(bd)[colnames(bd) == paste('pot', sp, sep ='')] <- 'pot'
  colnames(bd)[colnames(bd) == paste('ptnt_', sp, sep ='')] <- 'ptnt'
  colnames(bd)[colnames(bd) == paste('pot', sp, 'Epsilon', sep ='')] <- 'potEpsilon'
  forestDf <- forestPlotsDf
  colnames(forestDf)[colnames(forestDf) == paste('pot', sp, sep ='')] <- 'pot'
  colnames(forestDf)[colnames(forestDf) == paste('pot', sp, 'Epsilon', sep ='')] <- 'potEpsilon'
  mDf <- modDf
  colnames(mDf)[colnames(mDf) == paste('pot', sp, sep ='')] <- 'pot'

  # add residuals tp bd
  res <-data.frame(residuals(mod))
  colnames(res) <- "residuals"
  res$sign <- "positive"
  res[res$res < 0, "sign"] <- "negative"
  bd <- cbind(bd, res)

  # residuals vs fitted
  rvsf <- ggplot() +
    geom_point(data = bd, aes(pot, residuals)) +
    geom_hline(yintercept = 0, col = 'red', linetype = "dashed") +
    theme_bw()

  # density observed vs predicted
  dens <- ggplot() +
    geom_density(data = bd, aes(ptnt), col = 'black', fill = 'grey', lwd = 1, , linetype = 'dashed') +
    geom_density(data = bd, aes(pot), col = 'red') +
    geom_density(data = bd, aes(potEpsilon), col = 'blue') +
    # geom_density(data = forestDf, aes(potEpsilon), col = 'green4') +
    # geom_density(data = forestDf, aes(pot), col = 'green1') +
    annotate("text", label = paste("R² =", round(summary(mod)$r.squared,2)), x = 20, y = 0.075, size = 5) +
    theme_bw()

  # model observed - predicted (voir article: How to evaluate models: Observed vs. predictedor predicted vs. observed?)
  pred <- lm(ptnt ~ pot, data = bd)
  predE <- lm(ptnt ~ potEpsilon, data = bd)

  # obs vs pred
  mn <- min(bd$pot, bd$ptnt)
  mx <- max(bd$pot, bd$ptnt)
  ovsp <- ggplot() +
    geom_point(data = bd, aes(pot, ptnt)) + # , col = greco
    xlim(mn, mx) +
    ylim(mn, mx) +
    geom_abline(slope = 1, col = 'red', linetype = "dashed") +
    geom_abline(slope = coef(pred)[[2]], intercept = coef(pred)[[1]], col = 'black') +
    annotate("text", label = paste("intercept:", round(coef(pred)[[1]],2), "slope:", round(coef(pred)[[2]],2),
              "R²:", round(summary(pred)$r.squared, 2)), x = mean(bd$pot)+10, y = mean(bd$ptnt)-5, size = 5) +
    theme_bw()

  # obs vs pred + E
  mn <- min(bd$potEpsilon, bd$ptnt)
  mx <- max(bd$potEpsilon, bd$ptnt)
  ovspE <- ggplot() +
    geom_point(data = bd, aes(potEpsilon, ptnt)) + # , col = greco
    xlim(mn, mx) +
    ylim(mn, mx) +
    geom_abline(slope = 1, col = 'red', linetype = "dashed") +
    geom_abline(slope = coef(predE)[[2]], intercept = coef(predE)[[1]], col = 'black') +
    annotate("text", label = paste("intercept:", round(coef(predE)[[1]],2), "slope:", round(coef(predE)[[2]],2),
              "R²:", round(summary(predE)$r.squared, 2)), x = mean(bd$pot)+10, y = mean(bd$ptnt)-5, size = 5) +
    theme_bw()

  # fertility map
  fert <- ggplot() +
    geom_polygon(data = forestDf, aes(long,lat,group=group,fill=pot)) +
    coord_equal() +
    scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(forestDf$pot), name = "fertility\nindex\n(modeled)") +
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
    scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(forestDf$pot), name = "fertility\nindex\n(modeled)") +
    ggtitle(paste(ti, " + E")) +
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position="right",
            axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())

  fertRes <- ggplot() +
    geom_polygon(data = forestDf, aes(long,lat,group=group,fill=pot), alpha = 0.08) +
    coord_equal() +
    scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(forestDf$pot), name = "fertility\nindex\n(modeled)") +
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
  print(grid.arrange(dens, rvsf, ovsp, ovspE, nrow = 2, ncol = 2))
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







#
# # summary(lm(residuals(modUnPa03) ~ bdBauges03[,"rum"]))
# plot(residuals(modUnPa03) ~ bdBauges03[,"rum"], pch = 16)
# abline(h = 0, lty = 2, col = 'red')
# panel.smooth(bdBauges03[, "rum"], residuals(modUnPa03), span = 0.5)
#
#
# plot(modUnPa03)
# plot(residuals(modUnPa03) ~ bdBauges03[,"pot03"], pch = 16, xlim = c(-10, 40), ylim = c(-10, 40))
#
# # observed vs predicted (voir article: How to evaluate models: Observed vs. predictedor predicted vs. observed?)
# testPred <- lm(ptnt_09 ~ pot09, data = bdBauges09)
# summary(testPred)
# plot(bdBauges09$ptnt_09 ~ bdBauges09$pot09)
# curve(1*x, add = TRUE, col = 'red', lty = 2)
# curve(coef(testPred)[[1]] +  coef(testPred)[[2]] *x, add = TRUE, col = 'black')
