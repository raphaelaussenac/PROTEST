###############################################################
# initialisation
###############################################################

# clean up environment
# rm(list = ls())

# load packages
library(ggplot2)
library(plyr)
library(reshape2)
library(rgdal)
library(raster)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# load NFI points within the study area (bdBauges)
# set model method
# chosenMethod <- 'direct' # 'knPa-unPa' or 'direct'
# source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/modfertInd.R')

# load correspondence NFI point / code BDforet
correspond <- read.csv("Z:/Private/croisementIfn.csv", header = TRUE, sep = ";")

# load PROTEST points
protestPt <- load(file = "Z:/Private/protestPt.rda")

# load BDforet
forestPlots <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "forestPlots", encoding = "UTF-8", use_iconv = TRUE)

###############################################################
# retrieve code TFV for all NFI points within the study area
###############################################################

# select only NFI points within the study area
# correspond <- correspond[correspond$idp %in% bdBauges$idp, ]

# assign codeTFVdist as TFV when TFV is missing
# correspond[correspond$TFV == "", "TFV"] <- correspond[correspond$TFV == "", "codeTFVdist"]

###############################################################
# retrieve code TFV and geol data for all protest points
# within the study area
###############################################################

# convert placette.mes into a SpatialPointsDataFrame
xy <- placette.mes[, c('Xplac_recale', 'Yplac_recale')]
protestPlots <- SpatialPointsDataFrame(coords = xy, data = placette.mes, proj4string = crs(forestPlots))

# intersect
protestPlots <- intersect(protestPlots, forestPlots)

# plot
# plot(forestPlots, col = forestPlots$CODE_TF, border = forestPlots$CODE_TF)
# points(protestPlots, asp = 1, pch = 16, col = 'red')

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
text(bp, protestPtSurfTfv$surface + 500, label = protestPtSurfTfv$points, cex = 1, col = 'red')
text(bp, protestPtSurfTfv$surface - 500, label = round(protestPtSurfTfv$surface), cex = 1, col = 'black')
legend("topright", legend = c("nb protest pt", 'surface'), fill = c("red", 'black'), col = c('red'))

###############################################################
# group TFV types
###############################################################

# https://inventaire-forestier.ign.fr/IMG/pdf/dc_bdforet_2-0.pdf

# main types
A <- 'FF1-00-00' #: Forêt fermée à mélange de feuillus
B <- 'FF2G61-61' #: Forêt fermée de sapin ou épicéa
C <- 'FF31' #: Forêt fermée à mélange de feuillus prépondérants et conifères
D <- 'FF32' #: Forêt fermée à mélange de conifères prépondérants et feuillus
E <- 'FF1-09-09' #: Forêt fermée de hêtre pur

# under-represented types
# FO1: Forêt ouverte de feuillus purs ----------------------> A
# FF1-00: Forêt fermée de feuillus purs en îlots -----------> A
# FO2: Forêt ouverte de conifères purs ---------------------> B
# FO3: Forêt ouverte à mélange de feuillus et conifères ----> C
# FF1-49-49: Forêt fermée d’un autre feuillu pur -----------> A
# FF2-00-00: Forêt fermée à mélange de conifères -----------> B
# FF1G01-01: Forêt fermée de chênes décidus purs -----------> A
# FF2G53-53: Forêt fermée de pin laricio ou pin noir pur ---> B
# FF2-90-90: Forêt fermée à mélange d’autres conifères -----> B
# FF2-64-64: Forêt fermée de douglas pur -------------------> B
# FF2-00: Forêt fermée de conifères purs en îlots ----------> B
# FF1-10-10: Forêt fermée de châtaignier pur ---------------> A
# FO0: Forêt ouverte sans couvert arboré -------------------> remove
# FF2-52-52: Forêt fermée de pin sylvestre pur -------------> B
# FF0: Forêt fermée sans couvert arboré --------------------> remove

groupTfv <- function(df){
  df <- df[!(df$CODE_TF %in% c('FO0', 'FF0')),]
  df[df$CODE_TF == "FO1", "CODE_TF"] <- A
  df[df$CODE_TF == "FF1-00", "CODE_TF"] <- A
  df[df$CODE_TF == "FO2", "CODE_TF"] <- B
  df[df$CODE_TF == "FO3", "CODE_TF"] <- C
  df[df$CODE_TF == "FF1-49-49", "CODE_TF"] <- A
  df[df$CODE_TF == "FF2-00-00", "CODE_TF"] <- B
  df[df$CODE_TF == "FF1G01-01", "CODE_TF"] <- A
  df[df$CODE_TF == "FF2G53-53", "CODE_TF"] <- B
  df[df$CODE_TF == "FF2-90-90", "CODE_TF"] <- B
  df[df$CODE_TF == "FF2-64-64", "CODE_TF"] <- B
  df[df$CODE_TF == "FF2-00", "CODE_TF"] <- B
  df[df$CODE_TF == "FF1-10-10", "CODE_TF"] <- A
  df[df$CODE_TF == "FF2-52-52", "CODE_TF"] <- B
  return(df)
}

# protestPlots
protestPlotsDf <- protestPlots@data
protestPlotsDf$CODE_TF <- as.character(protestPlotsDf$CODE_TF)
protestPlotsDf <- groupTfv(protestPlotsDf)

# forestPlotsDf
# convert to be able to export to ggplot
forestPlots@data$area <- area(forestPlots)
forestPlotsDf <- forestPlots@data
forestPlotsDf$CODE_TF <- as.character(forestPlotsDf$CODE_TF)
forestPlotsDf <- groupTfv(forestPlotsDf)

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

# group oaks together (CHS CHE CHP)
arbres.vivant[arbres.vivant$Cod_ess %in% c('CHE', 'CHP'), 'Cod_ess'] <- 'CHS'

# transform NR (non-reconnu) into A.F
arbres.vivant[arbres.vivant$Cod_ess == 'NR', 'Cod_ess'] <- 'A.F'

# calculate basal area for each protest plot
# total BA of saplings
gPerches <- placette.mes[, c('Id_plac', 'Brin_taill', 'pch.cl15.F', 'pch.cl15.R', 'perch.nv')]
gPerches$Brin_taill <- as.numeric(as.character(gPerches$Brin_taill))
gPerches$pch.cl15.F <- as.numeric(as.character(gPerches$pch.cl15.F))
gPerches$pch.cl15.R <- as.numeric(as.character(gPerches$pch.cl15.R))
gPerches$perch.nv <- as.numeric(as.character(gPerches$perch.nv))
# saplings are considered all similar: diameter = 0.125m.
# they are multiplied by their number for each category (taill, cl15.F...)
# they are multiplied by 2.25 --> because they were measured on a plot
# of r = 10m (while trees were measured on a plot of r = 15 --> 2.25 greater)
gPerches$gBrin_taill <- ((pi*0.125^2)/4) * gPerches$Brin_taill * 2.25
gPerches$gpch.cl15.F <- ((pi*0.125^2)/4) * gPerches$pch.cl15.F * 2.25
gPerches$gpch.cl15.R <- ((pi*0.125^2)/4) * gPerches$pch.cl15.R * 2.25
gPerches$gperch.nv <- ((pi*0.125^2)/4) * gPerches$perch.nv * 2.25
gPerches$gPerches <- apply(gPerches[,c('gBrin_taill', 'gpch.cl15.F', 'gpch.cl15.R', 'gperch.nv')], 1, sum)
# total BA of trees
protestG <- ddply(arbres.vivant, .(Id_plac), summarise, Gtot = sum(g))
# merge
protestG <- merge(protestG, gPerches[, c('Id_plac', 'gPerches')], by = 'Id_plac')
protestG$G <- apply(protestG[,c('Gtot', 'gPerches')], 1, sum)
protestG[, c('Gtot', 'gPerches')] <- NULL

# calculate species basal area for each protest plot
# BA of deciduous saplings
gPerches$F <- apply(gPerches[,c('gBrin_taill', 'gpch.cl15.F')], 1, sum)
# proportion of deciduous saplings G
gPerches$pF <- gPerches$F / (gPerches$F + gPerches$gpch.cl15.R)
# assign "species" (i.e. deciduous or coniferous) to gperch.nv depending on
# the proportion of deciduous and coniferous sapling G
gPerches$Fnv <- gPerches$gperch.nv * gPerches$pF
gPerches$Rnv <- gPerches$gperch.nv * (1 - gPerches$pF)
# total BA of deciduous and coniferous saplings
gPerches$F <- gPerches$F + gPerches$Fnv
gPerches$R <- gPerches$gpch.cl15.R + gPerches$Rnv
# BA area of tree species
protestGSp <- ddply(arbres.vivant, .(Id_plac, Cod_ess), summarise, Gsp = sum(g))

# merge
for (i in unique(protestGSp$Id_plac)){
  if (!is.na(gPerches[gPerches$Id_plac == i, 'F'])){
    if (gPerches[gPerches$Id_plac == i, 'F'] > 0){
      protestGSp <- rbind(protestGSp, c(i, 'APF', gPerches[gPerches$Id_plac == i, 'F']))
    }
  }
  if (!is.na(gPerches[gPerches$Id_plac == i, 'R'])){
    if (gPerches[gPerches$Id_plac == i, 'R'] > 0){
      protestGSp <- rbind(protestGSp, c(i, 'APR', gPerches[gPerches$Id_plac == i, 'R']))
    }
  }
}
protestGSp <- protestGSp[order(protestGSp$Id_plac), ]

# calculate species proportion
protestGSpProp <- merge(protestGSp, protestG, by = "Id_plac")
protestGSpProp$Gsp <- as.numeric(protestGSpProp$Gsp)
protestGSpProp$prop <- protestGSpProp$Gsp * 100 / protestGSpProp$G
protestGSpProp <- protestGSpProp[order(protestGSpProp$Id_plac, protestGSpProp$prop, decreasing = TRUE),]

# transform all deciduous into Oak and Beech depending on the relative
# proportion of Oak and Beech in the plot
# same for coniferous with Fir and Spruce

# species not studied
deciduousSp <- c("CHA", "FRE", "CHT", "MER", "ERO", "TIL", "BOU", "TRE", "ERS",
                 "CHE", "ALB", "ERC", "SOR", "ROB", "ORM", "ERP", "SAU", "ALT",
                 "NOC", "NR", "CHP", "PEU", "SAM", "ERA", "A.F", 'APF') # [A]utre [P]erche [F]euillue
coniferousSp <- c("MEE", "P.S", "IFS","P.X", 'A.R', 'APR') # [A]utre [P]erche [R]ésineux

# species of interest
spInterestD <- c("HET", "CHS")
spInterestC <- c("S.P", "EPC")

protestNewSp <- data.frame(matrix(ncol = ncol(protestGSpProp), nrow = 0))
colnames(protestNewSp) <- colnames(protestGSpProp)
for (i in unique(protestGSpProp$Id_plac)){
  plac <- protestGSpProp[protestGSpProp$Id_plac == i,]

  # if there are some non-target deciduous species -------------------------------------------
  if (sum(plac$Cod_ess %in% c(deciduousSp)) > 0){
    # 1st case: there is 1 target deciduous species
    if (sum(plac$Cod_ess %in% c(spInterestD)) == 1){
      # -> assign the basal area of the non-target deciduous species
      # to the target deciduous species
      plac[plac$Cod_ess %in% spInterestD, "Gsp"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "Gsp"],
                                                        plac[plac$Cod_ess %in% spInterestD, "Gsp"])
      plac[plac$Cod_ess %in% spInterestD, "prop"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "prop"],
                                                        plac[plac$Cod_ess %in% spInterestD, "prop"])
      plac <- plac[!(plac$Cod_ess %in% deciduousSp),]
    }
    # 2nd case: there are 2 target deciduous species
    if (sum(plac$Cod_ess %in% c(spInterestD)) == 2){
      # 1 - calculate the relative proportion of each target species
      propCHS <- plac[plac$Cod_ess == "CHS", "Gsp"] / (plac[plac$Cod_ess == "CHS", "Gsp"] + plac[plac$Cod_ess == "HET", "Gsp"])
      propHET <- 1 - propCHS
      # 2 - assign the basal area of the non-target deciduous species
      # to the target deciduous species depending on the proportion of
      # each deciduous target species
      plac[plac$Cod_ess == "CHS", "Gsp"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "Gsp"]*propCHS,
                                                        plac[plac$Cod_ess == "CHS", "Gsp"])
      plac[plac$Cod_ess == "HET", "Gsp"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "Gsp"]*propHET,
                                                        plac[plac$Cod_ess == "HET", "Gsp"])
      #
      plac[plac$Cod_ess == "CHS", "prop"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "prop"]*propCHS,
                                                        plac[plac$Cod_ess == "CHS", "prop"])
      plac[plac$Cod_ess == "HET", "prop"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "prop"]*propHET,
                                                        plac[plac$Cod_ess == "HET", "prop"])
      plac <- plac[!(plac$Cod_ess %in% deciduousSp),]
    }
    # 3rd case: there are NO target deciduous species
    # --> next step
  }

  # if there are some non-target coniferous species -------------------------------------------
  if (sum(plac$Cod_ess %in% c(coniferousSp)) > 0){
    # 1st case: there is 1 target coniferous species
    if (sum(plac$Cod_ess %in% c(spInterestC)) == 1){
      # -> assign the basal area of the non-target coniferous species
      # to the target coniferous species
      plac[plac$Cod_ess %in% spInterestC, "Gsp"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "Gsp"],
                                                        plac[plac$Cod_ess %in% spInterestC, "Gsp"])
      plac[plac$Cod_ess %in% spInterestC, "prop"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "prop"],
                                                        plac[plac$Cod_ess %in% spInterestC, "prop"])
      plac <- plac[!(plac$Cod_ess %in% coniferousSp),]
    }
    # 2nd case: there are 2 target coniferous species
    if (sum(plac$Cod_ess %in% c(spInterestC)) == 2){
      # 1 - calculate the relative proportion of each target species
      propEPC <- plac[plac$Cod_ess == "EPC", "Gsp"] / (plac[plac$Cod_ess == "EPC", "Gsp"] + plac[plac$Cod_ess == "HET", "Gsp"])
      propSP <- 1 - propEPC
      # 2 - assign the basal area of the non-target coniferous species
      # to the target coniferous species depending on the relative proportion of
      # each target species
      plac[plac$Cod_ess == "EPC", "Gsp"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "Gsp"]*propEPC,
                                                        plac[plac$Cod_ess == "EPC", "Gsp"])
      plac[plac$Cod_ess == "S.P", "Gsp"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "Gsp"]*propSP,
                                                        plac[plac$Cod_ess == "S.P", "Gsp"])
      #
      plac[plac$Cod_ess == "EPC", "prop"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "prop"]*propEPC,
                                                        plac[plac$Cod_ess == "EPC", "prop"])
      plac[plac$Cod_ess == "S.P", "prop"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "prop"]*propSP,
                                                        plac[plac$Cod_ess == "S.P", "prop"])
      plac <- plac[!(plac$Cod_ess %in% coniferousSp),]
    }
    # 3rd case: there are NO target coniferous species
    # --> next step
  }
  protestNewSp <- rbind(protestNewSp, plac)
}

# list of pure stands
protestPure <- protestNewSp[protestNewSp$prop > 75,]
pureBeech <- protestPure[protestPure$Cod_ess == "HET", "Id_plac"]
pureOak <- protestPure[protestPure$Cod_ess == "CHS", "Id_plac"]
pureFir <- protestPure[protestPure$Cod_ess == "S.P", "Id_plac"]
pureSpruce <- protestPure[protestPure$Cod_ess == "EPC", "Id_plac"]

# list of mixed stands
protestMixed <- protestNewSp[!(protestNewSp$Id_plac %in% protestPure$Id_plac), ]
# keep only the two most abundant species on the plots
protestMixedProp <- data.frame(matrix(ncol = ncol(protestMixed), nrow = 0))
colnames(protestMixedProp) <- colnames(protestMixed)
for (i in unique(protestMixed$Id_plac)){
  plac <- protestMixed[protestMixed$Id_plac == i,]
  plac <- plac[c(1:2),]
  protestMixedProp <- rbind(protestMixedProp, plac)
}

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

# species composition
protestPlotsDf$compoSp <- NA
protestPlotsDf[protestPlotsDf$Id_plac %in% pureBeech, "compoSp"] <- "beech"
protestPlotsDf[protestPlotsDf$Id_plac %in% pureOak, "compoSp"] <- "oak"
protestPlotsDf[protestPlotsDf$Id_plac %in% pureFir, "compoSp"] <- "fir"
protestPlotsDf[protestPlotsDf$Id_plac %in% pureSpruce, "compoSp"] <- "spruce"
protestPlotsDf[protestPlotsDf$Id_plac %in% mixedBeechFir, "compoSp"] <- "beech-fir"
protestPlotsDf[protestPlotsDf$Id_plac %in% mixedBeechSpruce, "compoSp"] <- "beech-spruce"
protestPlotsDf[protestPlotsDf$Id_plac %in% mixedFirSpruce, "compoSp"] <- "fir-spruce"

###############################################################
# Define whether the forest plot is composed of deciduous and / or conifers
# base on the basal area proportion of each (threshold = 75%)
###############################################################

forestPlotsDf$compoDCM <- NA
forestPlotsDf[forestPlotsDf$p100gfP > 75, 'compoDCM'] <- 'D' # deciduous
forestPlotsDf[forestPlotsDf$p100gfP < 25, 'compoDCM'] <- 'C' # conifers
forestPlotsDf[forestPlotsDf$p100gfP >= 25 & forestPlotsDf$p100gfP <= 75, 'compoDCM'] <- 'MDC' # mixte
# prepare columns to assign species to each forest plot
forestPlotsDf$compoSp <- NA

###############################################################
# retrieve PROTEST plot species composition within each TFV types
# and assign (draw) composition to each forest plot
###############################################################

# classify composition in deciduous or coniferous
deciduous <- c('beech', 'oak')
coniferous <- c('fir', 'spruce', 'fir-spruce')
mixed <- c('beech-fir', 'beech-spruce')

for (i in unique(forestPlotsDf$CODE_TF)){
  # retrieve distribution of composition in a specific TFV type
  distrib <- protestPlotsDf[protestPlotsDf$CODE_TF == i & !is.na(protestPlotsDf$compoSp), "compoSp"]
  # split species distribution into a deciduous part a coniferous part
  # and a mied part
  distribD <- as.character(distrib[distrib %in% deciduous])
  distribC <- as.character(distrib[distrib %in% coniferous])
  distribM <- as.character(distrib[distrib %in% mixed])
  # assign species to each forest plot
  # deciduous forest plots
  nbD <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'D'
                & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'D'
                & forestPlotsDf$CODE_TF == i, 'compoSp'] <- distribD[round(runif(nbD, min = 1, max = length(distribD)))]
  # coniferous forest plots
  nbC <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'C'
                & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'C'
                & forestPlotsDf$CODE_TF == i, 'compoSp'] <- distribC[round(runif(nbC, min = 1, max = length(distribC)))]
  # mixte forest plots
  nbM <- nrow(forestPlotsDf[forestPlotsDf$compoDCM == 'MDC'
                & forestPlotsDf$CODE_TF == i,])
  forestPlotsDf[forestPlotsDf$compoDCM == 'MDC'
                & forestPlotsDf$CODE_TF == i, 'compoSp'] <- distribM[round(runif(nbM, min = 1, max = length(distribM)))]
}

# 'mixture composition' --> pure / mixed
forestPlotsDf$compo <- NA
forestPlotsDf[forestPlotsDf$compoSp %in% c('beech', 'oak', 'spruce', 'fir'), 'compo'] <- 'pure'
forestPlotsDf[forestPlotsDf$compoSp %in% c('beech-fir', 'beech-spruce', 'fir-spruce'), 'compo'] <- 'mixed'

###############################################################
# Plots and verifications
###############################################################

# verifications
table(forestPlotsDf[forestPlotsDf$CODE_TF == "FF1-00-00","compoSp"], forestPlotsDf[forestPlotsDf$CODE_TF == "FF1-00-00","compoDCM"])
table(forestPlotsDf[forestPlotsDf$compoDCM == "D", 'compoSp'])
table(forestPlotsDf[forestPlotsDf$compoDCM == "C", 'compoSp'])
table(forestPlotsDf[forestPlotsDf$compoDCM == "MDC", 'compoSp'])

# plot area for each type of composition
surfaceCompoSp <- ddply(forestPlotsDf, .(compoSp), summarise, area = sum(area))
surfaceCompoSp$area <- surfaceCompoSp$area / 10000
surfaceCompoSp <- surfaceCompoSp[order(surfaceCompoSp$area, decreasing = TRUE),]
bp <- barplot(surfaceCompoSp$area, names.arg = surfaceCompoSp$compoSp, las = 2, main = "Surface (ha) covered by each composition")
text(bp, surfaceCompoSp$area - 500, label = round(surfaceCompoSp$area,1), cex = 1, col = 'red')

# composition map
# put back compositions in the SpatialPolygonDataframe
forestPlots@data <- merge(forestPlots@data, forestPlotsDf[, c('WKTid', 'compoDCM', 'compoSp')], by = 'WKTid', all = TRUE)
forestPlots <- forestPlots[!is.na(forestPlots$compoSp), ]
# convert for ggplot
forestPlotsPts <- fortify(forestPlots, region="WKTid")
colnames(forestPlotsPts)[colnames(forestPlotsPts) == 'id'] <- 'WKTid'
forestNewSp <- join(forestPlotsPts, forestPlots@data, by="WKTid")

# plot
pnr <- readOGR(dsn = "Z:/Private/PNR Bauges/Sans_Trou", layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)
colnames(pnr@data)[colnames(pnr@data) == "ID"] <- 'id'
pnrPts <- fortify(pnr, region="id")
pnrNew <- join(pnrPts, pnr@data, by="id")

pdf(file="C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/mapCompo.pdf", width = 20, height = 10)
ggplot() +
  geom_polygon(data = forestNewSp, aes(long,lat,group=group,fill=compoSp)) +
  geom_polygon(data = pnrNew, aes(long,lat,group=group), alpha = 0.1) +
  # geom_polygon(data = forestNewSp, aes(long,lat,group=group,fill=greco), alpha = 0.3) +
  coord_equal() +
  # scale_fill_gradient2(low = "cyan", mid = "blue3", high = "purple", aesthetics = "fill", midpoint = mean(forestDf$pot), name = "fertility\nindex\n(modeled)") +
  ggtitle("stand composition") +
  # guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.75, barheight = 15)) +
  guides(fill = guide_legend(title="")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position ="right",
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

# -----> ajouter points TRUE chene / hetre / mélanges...???
