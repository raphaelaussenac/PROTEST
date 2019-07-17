###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
library(ggplot2)
library(plyr)
library(reshape2)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# load NFI points within the study area (bdBauges)
# set model method
chosenMethod <- 'direct' # 'knPa-unPa' or 'direct'
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/modfertInd.R')

# load correspondence NFI point / code BDforet
correspond <- read.csv("Z:/Private/croisementIfn.csv", header = TRUE, sep = ";")

# load PROTEST points
protestPt <- load(file = "Z:/Private/protestPt.rda")

# load BDforet
forestPlots <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "forestPlots3Ha", encoding = "UTF-8", use_iconv = TRUE)

###############################################################
# retrieve code TFV for all NFI points within the study area
###############################################################

# select only NFI points within the study area
correspond <- correspond[correspond$idp %in% bdBauges$idp, ]

# assign codeTFVdist as TFV when TFV is missing
correspond[correspond$TFV == "", "TFV"] <- correspond[correspond$TFV == "", "codeTFVdist"]

###############################################################
# retrieve code TFV for all protest points within the study area
###############################################################

# convert placette.mes into a SpatialPointsDataFrame
xy <- placette.mes[, c('Xplac_recale', 'Yplac_recale')]
protestPlots <- SpatialPointsDataFrame(coords = xy, data = placette.mes, proj4string = crs(forestPlots))

# intersect
protestPlots <- intersect(protestPlots, forestPlots)

# plot
plot(forestPlots, col = forestPlots$CODE_TF, border = forestPlots$CODE_TF)
points(protestPlots, asp = 1, pch = 16, col = 'red')

###############################################################
# surface of each TFV types
###############################################################

# surface for each TFV type throughout the study area
surfaceTfv <- data.frame("tfv" = NA, "surface" = NA)
for (i in unique(forestPlots$CODE_TF)){
  surf <- sum(area(forestPlots[forestPlots$CODE_TF == i,]))
  aa <- c(i, surf)
  surfaceTfv <- rbind(surfaceTfv, aa)
}
surfaceTfv <- surfaceTfv[-1, ]
surfaceTfv$surface <- as.numeric(surfaceTfv$surface)
surfaceTfv$surface <- surfaceTfv$surface / 10000 # convert into hectars

# number of inventory points for each TFV type throughout the study area
protestPtSurfTfv <- data.frame(table(protestPlots$CODE_TF))
protestPtSurfTfv <- merge(surfaceTfv, protestPtSurfTfv, by.x = "tfv", by.y = "Var1")
colnames(protestPtSurfTfv)[colnames(protestPtSurfTfv) == "Freq"] <- "points"
protestPtSurfTfv <- protestPtSurfTfv[order(protestPtSurfTfv$surface, decreasing = T),]

# plot
bp <- barplot(protestPtSurfTfv$surface, names.arg = protestPtSurfTfv$tfv, las = 2, ylim = c(-1000, 20000), main = "Surface (ha) and number of\ninventory points for each TFV type")
barplot(protestPtSurfTfv$surface, names.arg = protestPtSurfTfv$tfv, las = 2, ylim = c(-1000, 20000), main = "Surface (ha) and number of\ninventory points for each TFV type")
text(bp, protestPtSurfTfv$surface + 500, label = protestPtSurfTfv$points, cex = 1, col = 'red')
text(bp, protestPtSurfTfv$surface - 500, label = round(protestPtSurfTfv$surface), cex = 1, col = 'black')
legend("topright", legend = c("nb protest pt", 'surface'), fill = c("red", 'black'), col = c('red'))

###############################################################
# group TFV types
###############################################################

# https://inventaire-forestier.ign.fr/IMG/pdf/dc_bdforet_2-0.pdf

todo -------> regroupement des types TFV

# protestPlots
protestPlotsDf <- protestPlots@data
protestPlotsDf$CODE_TF <- as.character(protestPlotsDf$CODE_TF)
minitfv <- protestPtSurfTfv[protestPtSurfTfv$surface < 1000, 'tfv'] # all small TFV in FF1-00-00
protestPlotsDf[protestPlotsDf$CODE_TF %in% minitfv, "CODE_TF"] <- 'FF1-00-00'

# forestPlotsDf
forestPlotsDf <- forestPlots@data
forestPlotsDf$CODE_TF <- as.character(forestPlotsDf$CODE_TF)
forestPlotsDf[forestPlotsDf$CODE_TF %in% minitfv, "CODE_TF"] <- 'FF1-00-00'

###############################################################
# Define whether the plot is composed of deciduous and / or conifers
# base on the basal area proportion of each (threshold = 75%)
###############################################################

# forestPlots
forestPlotsDf$compoDCM <- NA
forestPlotsDf[forestPlotsDf$p100gfP > 75 & !is.na(forestPlotsDf$p100gfP), 'compoDCM'] <- 'D' # deciduous
forestPlotsDf[forestPlotsDf$p100gfP < 25 & !is.na(forestPlotsDf$p100gfP), 'compoDCM'] <- 'C' # conifers
forestPlotsDf[forestPlotsDf$p100gfP >= 25 & forestPlotsDf$p100gfP <= 75 & !is.na(forestPlotsDf$p100gfP), 'compoDCM'] <- 'MDC' # mixte
# prepare columns to assign species to each forest plot
forestPlotsDf$compoSp <- NA

###############################################################
# Classsify protest plots in one of those composition
# hêtre
# chêne
# sapin
# epicea
# hêtre-sapin
# hêtre-épicéa
# sapin-épicea
###############################################################
arbres.vivant$Id_plac <- as.character(arbres.vivant$Id_plac)
arbres.vivant$Cod_ess <- as.character(arbres.vivant$Cod_ess)

# calculate basal area for each protest plot
protestG <- ddply(arbres.vivant, .(Id_plac), summarise, G = sum(g))
# calculate species basal area for each protest plot
protestGSp <- ddply(arbres.vivant, .(Id_plac, Cod_ess), summarise, Gsp = sum(g))
# calculate species proportion
protestGSpProp <- merge(protestGSp, protestG, by = "Id_plac")
protestGSpProp$prop <- protestGSpProp$Gsp * 100 / protestGSpProp$G
protestGSpProp <- protestGSpProp[order(protestGSpProp$Id_plac, protestGSpProp$prop, decreasing = TRUE),]

# list of pure stands
protestPure <- protestGSpProp[protestGSpProp$prop > 75,]
pureBeech <- protestPure[protestPure$Cod_ess == "HET", "Id_plac"]
pureOak <- protestPure[protestPure$Cod_ess == "CHE" | protestPure$Cod_ess == "CHS", "Id_plac"]
pureFir <- protestPure[protestPure$Cod_ess == "S.P", "Id_plac"]
pureSpruce <- protestPure[protestPure$Cod_ess == "EPC", "Id_plac"]

# list of mixed stands
protestMixed <- protestGSpProp[!(protestGSpProp$Id_plac %in% protestPure$Id_plac), ]
# keep only the two most abundant species on the plots
protestMixedProp <- data.frame(matrix(ncol = ncol(protestMixed), nrow = 0))
colnames(protestMixedProp) <- colnames(protestMixed)
for (i in unique(protestMixed$Id_plac)){
  plac <- protestMixed[protestMixed$Id_plac == i,]
  plac <- plac[c(1:2),]
  protestMixedProp <- rbind(protestMixedProp, plac)
}

#
# remove plots where the 2nd most abundant species represent less than 25% G ?
#

# change format long --> wide
protestMixed <- dcast(protestMixedProp, formula =  Id_plac ~ Cod_ess)
protestMixed$sumSp <- apply(protestMixed[,-1], 1, sum, na.rm = TRUE)
# keep only plots where sum(sp1 + sp1) > 75% G
protestMixed <- protestMixed[protestMixed$sumSp > 75, ]
mixedBeechFir <- protestMixed[!is.na(protestMixed$HET) & !is.na(protestMixed$S.P), "Id_plac"]
mixedBeechSpruce <- protestMixed[!is.na(protestMixed$HET) & !is.na(protestMixed$EPC), "Id_plac"]
mixedFirSpruce <- protestMixed[!is.na(protestMixed$S.P) & !is.na(protestMixed$EPC), "Id_plac"]

###############################################################
# assign a composition to each protest plot
###############################################################

protestPlotsDf$compoSp <- NA
protestPlotsDf[protestPlotsDf$Id_plac %in% pureBeech, "compoSp"] <- "beech"
protestPlotsDf[protestPlotsDf$Id_plac %in% pureOak, "compoSp"] <- "oak"
protestPlotsDf[protestPlotsDf$Id_plac %in% pureFir, "compoSp"] <- "fir"
protestPlotsDf[protestPlotsDf$Id_plac %in% pureSpruce, "compoSp"] <- "spruce"
protestPlotsDf[protestPlotsDf$Id_plac %in% mixedBeechFir, "compoSp"] <- "beech-fir"
protestPlotsDf[protestPlotsDf$Id_plac %in% mixedBeechSpruce, "compoSp"] <- "beech-spruce"
protestPlotsDf[protestPlotsDf$Id_plac %in% mixedFirSpruce, "compoSp"] <- "fir-spruce"

# remove plots for which we do not have any composition (compoSp)
protestPlotsDf <- protestPlotsDf[!is.na(protestPlotsDf$compoSp),]

###############################################################
# retrieve species composition within each TFV types
# and assign (draw) composition to each forest plot
###############################################################

# classify composition in deciduous or coniferous
deciduous <- c('beech', 'oak')
coniferous <- c('fir', 'spruce', 'fir-spruce')
mixed <- c('beech-fir', 'beech-spruce')

for (i in unique(forestPlotsDf$CODE_TF)){
  # retrieve distribution of composition in a specific TFV type
  distrib <- protestPlotsDf[protestPlotsDf$CODE_TF == i, "compoSp"]
  # split species distribution into a deciduous part a coniferous part
  # and a mied part
  distribD <- as.character(distrib[distrib %in% deciduous])
  distribC <- as.character(distrib[distrib %in% coniferous])
  distribM <- as.character(distrib[distrib %in% mixed])
  # assign species to each forest plot
  # deciduous forest plots
  nbD <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'D' & !is.na(forestPlotsDf$compoDCM)
                & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'D'  & !is.na(forestPlotsDf$compoDCM)
                & forestPlotsDf$CODE_TF == i, 'compoSp'] <- distribD[round(runif(nbD, min = 1, max = length(distribD)))]
  # coniferous forest plots
  nbC <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'C' & !is.na(forestPlotsDf$compoDCM)
                & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'C'  & !is.na(forestPlotsDf$compoDCM)
                & forestPlotsDf$CODE_TF == i, 'compoSp'] <- distribC[round(runif(nbC, min = 1, max = length(distribC)))]
  # mixte forest plots
  nbM <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$compoDCM)
                & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'MDC'  & !is.na(forestPlotsDf$compoDCM)
                & forestPlotsDf$CODE_TF == i, 'compoSp'] <- distribM[round(runif(nbM, min = 1, max = length(distribM)))]
}


# test
table(forestPlotsDf[forestPlotsDf$CODE_TF == "FF1-00-00","compoSp"], forestPlotsDf[forestPlotsDf$CODE_TF == "FF1-00-00","compoDCM"])








###############################################################
# Dg calculation
###############################################################

forestPlotsDf$dg <- NA

# 1 - First case: pure stands
# --> nothing to do, dgPred is available
forestPlotsDf[forestPlotsDf$compoSp == 'pure' & !is.na(forestPlotsDf$p100gfP), "Dg"] <-
                          forestPlotsDf[forestPlotsDf$compoSp == 'pure' & !is.na(forestPlotsDf$p100gfP), "dgPred"]

# 2 - Second case: deciduous - conifer mixture
    # 2a - G deciduous and G conifers calculation (with gPred and p100gfP)
forestPlotsDf$Gsp1 <- NA
forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$p100gfP), "Gsp1"] <-
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$p100gfP), "gPred"] *
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$p100gfP), "p100gfP"] / 100
forestPlotsDf$Gsp2 <- NA
forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$p100gfP), "Gsp2"] <-
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$p100gfP), "gPred"] -
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$p100gfP), "Gsp1"]

    # 2b - calculate Dg deciduous and Dg coniferous on each protest plot
protestPlotsDf$compoSp <- NA
protestPlotsDf$sp1 <- NA
protestPlotsDf$sp2 <- NA
protestPlotsDf$dgSp1 <- NA
protestPlotsDf$dgSp2 <- NA
protestPlotsDf$diffDg <- NA

    # retrieve protest points for each sp combination obtained on the study area
couples <- unique(paste(forestPlotsDf[forestPlotsDf$compoSp == 'mixte' & !is.na(forestPlotsDf$p100gfP), "sp1"],
                      forestPlotsDf[forestPlotsDf$compoSp == 'mixte' & !is.na(forestPlotsDf$p100gfP), "sp2"]))

plotListCouples <- data.frame(matrix(nrow = length(unique(arbres.vivant$Id_plac)), ncol = length(couples)))
colnames(plotListCouples) <- couples
plotListCouples$plot <- unique(arbres.vivant$Id_plac)
for (c in 1:length(couples)){
  for (i in unique(arbres.vivant$Id_plac)){
    plac <- arbres.vivant[arbres.vivant$Id_plac == i, ]
    sp1 <- substr(couples[c], 1, 3)
    sp2 <- substr(couples[c], 5, 7)
    # if both sp are present -> calculate sum of basal area
    if (sum(plac$Cod_ess == sp1) > 0 & sum(plac$Cod_ess == sp2) > 0){
      gTot <- sum(plac$g)
      gSp1Sp2 <- sum(plac[plac$Cod_ess == sp1 | plac$Cod_ess == sp2, "g"])
      # if both sp represent > 75% of G
      if (gSp1Sp2/gTot >= 0.75){
        # -> save plot ID
        plotListCouples[plotListCouples$plot == i, couples[c]] <- i
        # -> asign compoSp = mixte to protestPlotsDf
        protestPlotsDf[protestPlotsDf$Id_plac == i, "compoSp"] <- "mixte"
        # -> asign species name
        protestPlotsDf[protestPlotsDf$Id_plac == i, "sp1"] <- sp1
        protestPlotsDf[protestPlotsDf$Id_plac == i, "sp2"] <- sp2
        # -> calculate Dg deciduous and Dg coniferous
        dgSp1 <- sqrt(sum((plac[plac$Cod_ess == sp1, "Diam"])^2) / nrow(plac))
        dgSp2 <- sqrt(sum((plac[plac$Cod_ess == sp2, "Diam"])^2) / nrow(plac))
        diffDg <- dgSp1 - dgSp2
        protestPlotsDf[protestPlotsDf$Id_plac == i, "dgSp1"] <- dgSp1
        protestPlotsDf[protestPlotsDf$Id_plac == i, "dgSp2"] <- dgSp2
        protestPlotsDf[protestPlotsDf$Id_plac == i, "diffDg"] <- diffDg
      }
    }
  }
}

listTest <- plotListCouples[!is.na(plotListCouples[,1]),1]
length(listTest)
modTab <- protestPlotsDf[protestPlotsDf$Id_plac %in% listTest, ]
plot(modTab$dgSp1, modTab$dgSp2)
mod <- lm(diffDg ~ gPred + dgPred + alti + slope + expoNS + expoEW + ph + rum, data = modTab)
summary(mod)





# # 3 - Third case: deciduous - deciduous or conifer - conifer mixtures
# ----> ?????




nrow(plotListCouples[!is.na(plotListCouples[,1]),])

nrow(forestPlotsDf[forestPlotsDf$sp1 == "EPC" & forestPlotsDf$sp2 == "S.P", ])


# todo: add ifn points in the process (wherever protest points are used)
