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
forestPlots@data$id <- c(1:nrow(forestPlots@data))
forestPlots@data$area <- area(forestPlots)
forestPlotsDf <- forestPlots@data
forestPlotsDf$CODE_TF <- as.character(forestPlotsDf$CODE_TF)
forestPlotsDf <- groupTfv(forestPlotsDf)

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

# group Oaks together (CHS CHE CHP)
arbres.vivant[arbres.vivant$Cod_ess %in% c('CHE', 'CHP'), 'Cod_ess'] <- 'CHS'

# transform NR (non-reconnu) into A.F
arbres.vivant[arbres.vivant$Cod_ess == 'NR', 'Cod_ess'] <- 'A.F'

# calculate basal area for each protest plot
protestG <- ddply(arbres.vivant, .(Id_plac), summarise, G = sum(g))
# calculate species basal area for each protest plot
protestGSp <- ddply(arbres.vivant, .(Id_plac, Cod_ess), summarise, Gsp = sum(g))
# calculate species proportion
protestGSpProp <- merge(protestGSp, protestG, by = "Id_plac")
protestGSpProp$prop <- protestGSpProp$Gsp * 100 / protestGSpProp$G
protestGSpProp <- protestGSpProp[order(protestGSpProp$Id_plac, protestGSpProp$prop, decreasing = TRUE),]

# transform all deciduous into Oak and Beech depending on the proportion of
# Oak and Beech in the plot
# same for coniferous with Fir and Spruce

# species not studied
deciduousSp <- c("CHA", "FRE", "CHT", "MER", "ERO", "TIL", "BOU", "TRE", "ERS",
                 "CHE", "ALB", "ERC", "SOR", "ROB", "ORM", "ERP", "SAU", "ALT",
                 "NOC", "NR", "CHP", "PEU", "SAM", "ERA", "A.F")
coniferousSp <- c("MEE", "P.S", "IFS","P.X")

# species of interest
spInterestD <- c("HET", "CHS")
spInterestC <- c("S.P", "EPC")

protestNewSp <- data.frame(matrix(ncol = ncol(protestGSpProp), nrow = 0))
colnames(protestNewSp) <- colnames(protestGSpProp)
for (i in unique(protestGSpProp$Id_plac)){
  plac <- protestGSpProp[protestGSpProp$Id_plac == i,]

  # if there are some deciduous species (not studied) -------------------------------------------
  if (sum(plac$Cod_ess %in% c(deciduousSp)) > 0){
    # 1st possibility: there is 1 deciduous species of interest
    if (sum(plac$Cod_ess %in% c(spInterestD)) == 1){
      # -> assign the basal area of the deciduous species (not of interest)
      # to the deciduous species of interest
      plac[plac$Cod_ess %in% spInterestD, "Gsp"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "Gsp"],
                                                        plac[plac$Cod_ess %in% spInterestD, "Gsp"])
      plac[plac$Cod_ess %in% spInterestD, "prop"] <- sum(plac[plac$Cod_ess %in% deciduousSp, "prop"],
                                                        plac[plac$Cod_ess %in% spInterestD, "prop"])
      plac <- plac[!(plac$Cod_ess %in% deciduousSp),]
    }
    # 2nd possibility: there are 2 deciduous species of interest
    if (sum(plac$Cod_ess %in% c(spInterestD)) == 2){
      # 1 - calculate the relative proportion of each species of interest
      propCHS <- plac[plac$Cod_ess == "CHS", "Gsp"] / (plac[plac$Cod_ess == "CHS", "Gsp"] + plac[plac$Cod_ess == "HET", "Gsp"])
      propHET <- 1 - propCHS
      # 2 - assign the basal area of the deciduous species (not of interest)
      # to the deciduous species of interest depending on the proportion of
      # each species of interest
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
    # 3rd possibility: there are NO deciduous species of interest
    # --> next step
  }

  # if there are some coniferous species (not studied) -------------------------------------------
  if (sum(plac$Cod_ess %in% c(coniferousSp)) > 0){
    # 1st possibility: there is 1 deciduous species of interest
    if (sum(plac$Cod_ess %in% c(spInterestC)) == 1){
      # -> assign the basal area of the deciduous species (not of interest)
      # to the deciduous species of interest
      plac[plac$Cod_ess %in% spInterestC, "Gsp"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "Gsp"],
                                                        plac[plac$Cod_ess %in% spInterestC, "Gsp"])
      plac[plac$Cod_ess %in% spInterestC, "prop"] <- sum(plac[plac$Cod_ess %in% coniferousSp, "prop"],
                                                        plac[plac$Cod_ess %in% spInterestC, "prop"])
      plac <- plac[!(plac$Cod_ess %in% coniferousSp),]
    }
    # 2nd possibility: there are 2 deciduous species of interest
    if (sum(plac$Cod_ess %in% c(spInterestC)) == 2){
      # 1 - calculate the relative proportion of each species of interest
      propEPC <- plac[plac$Cod_ess == "EPC", "Gsp"] / (plac[plac$Cod_ess == "EPC", "Gsp"] + plac[plac$Cod_ess == "HET", "Gsp"])
      propSP <- 1 - propEPC
      # 2 - assign the basal area of the deciduous species (not of interest)
      # to the deciduous species of interest depending on the proportion of
      # each species of interest
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
    # 3rd possibility: there are NO deciduous species of interest
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

# remove plots for which we do not have any composition (compoSp)
protestPlotsDf <- protestPlotsDf[!is.na(protestPlotsDf$compoSp),]

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

# remove those plots for which we do not have any composition
forestPlotsDf <- forestPlotsDf[!is.na(forestPlotsDf$compoSp), ]

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
forestPlots@data <- merge(forestPlots@data, forestPlotsDf[, c('id', 'compoDCM', 'compoSp')], by = 'id', all = TRUE)
forestPlots <- forestPlots[!is.na(forestPlots$compoSp), ]
# convert for ggplot
forestPlotsPts <- fortify(forestPlots, region="id")
forestNewSp <- join(forestPlotsPts, forestPlots@data, by="id")
# plot
pdf(file="C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/mapCompo.pdf", width = 20, height = 10)
ggplot() +
  geom_polygon(data = forestNewSp[forestNewSp$compoSp == 'spruce',], aes(long,lat,group=group,fill=compoSp)) +
  geom_polygon(data = forestNewSp, aes(long,lat,group=group,fill=greco), alpha = 0.3) +
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
