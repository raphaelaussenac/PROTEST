###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
library(ggplot2)

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
# species distribution within each TFV types
# and assign species to each plot
###############################################################

arbres.vivant$Id_plac <- as.character(arbres.vivant$Id_plac)

# Define whether the plot is composed of deciduous and / or conifers
# base on the basal area proportion of each (threshold = 75%)
forestPlotsDf$compoDCM <- NA
forestPlotsDf[forestPlotsDf$p100gfP > 75 & !is.na(forestPlotsDf$p100gfP), 'compoDCM'] <- 'D' # deciduous
forestPlotsDf[forestPlotsDf$p100gfP < 25 & !is.na(forestPlotsDf$p100gfP), 'compoDCM'] <- 'C' # conifers
forestPlotsDf[forestPlotsDf$p100gfP >= 25 & forestPlotsDf$p100gfP <= 75 & !is.na(forestPlotsDf$p100gfP), 'compoDCM'] <- 'MDC' # mixte
# prepare columns to assign species to each forest plot
forestPlotsDf$sp1 <- NA
forestPlotsDf$sp2 <- NA

# group species (necessaire)????
-------> Boul -> hetre / frene -> chêne...

# specify whiwh species is deciduous and which one is coniferous
deciduous <- c('HET', 'CHE')
coniferous <- c('EPC', 'S.P')

for (i in unique(forestPlotsDf$CODE_TF)){
  print(i)
  # retrieve list of plots for each TFV type
  idPlotTfvType <- protestPlotsDf[protestPlotsDf$CODE_TF == i, "Id_plac"]
  # count trees per species
  treesTfvType <- arbres.vivant[arbres.vivant$Id_plac %in% idPlotTfvType, ]
  count <- data.frame(table(treesTfvType$Cod_ess))
  count <- count[count$Freq != 0,]
  count <- count[order(count$Freq, decreasing = TRUE),]
  barplot(count$Freq, names.arg = count$Var1, las = 2)
  # species distribution
  distrib <- rep(count$Var1, count$Freq)
  # split species distribution into a deciduous part and a coniferous part
  distribD <- as.character(distrib[distrib %in% deciduous])
  distribC <- as.character(distrib[distrib %in% coniferous])
  # assign species to each forest plot
  # deciduous forest plots
  nbD <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'D' & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'D'  & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i, 'sp1'] <- distribD[round(runif(nbD, min = 1, max = length(distribD)))]
  forestPlotsDf[forestPlotsDf$compoDCM == 'D'  & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i, 'sp2'] <- distribD[round(runif(nbD, min = 1, max = length(distribD)))]
  # coniferous forest plots
  nbC <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'C' & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'C'  & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i, 'sp1'] <- distribC[round(runif(nbC, min = 1, max = length(distribC)))]
  forestPlotsDf[forestPlotsDf$compoDCM == 'C'  & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i, 'sp2'] <- distribC[round(runif(nbC, min = 1, max = length(distribC)))]
  # mixte forest plots
  nbM <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'MDC' & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'MDC'  & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i, 'sp1'] <- distribD[round(runif(nbM, min = 1, max = length(distribD)))]
  forestPlotsDf[forestPlotsDf$compoDCM == 'MDC'  & !is.na(forestPlotsDf$compoDCM) & forestPlotsDf$CODE_TF == i, 'sp2'] <- distribC[round(runif(nbM, min = 1, max = length(distribC)))]
}

# pure or mixte (in terms of species)
forestPlotsDf$compoSp <- NA
forestPlotsDf[forestPlotsDf$sp1 == forestPlotsDf$sp2 & !is.na(forestPlotsDf$sp1), 'compoSp'] <- 'pure'
forestPlotsDf[forestPlotsDf$sp1 != forestPlotsDf$sp2 & !is.na(forestPlotsDf$sp1), 'compoSp'] <- 'mixte'



# "FF2G61-61" == forêt fermée de sapin ou épicéa
table(forestPlotsDf[forestPlotsDf$CODE_TF == "FF2G61-61","sp1"], forestPlotsDf[forestPlotsDf$CODE_TF == "FF2G61-61","sp2"])
