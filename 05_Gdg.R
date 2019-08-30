###############################################################
# import objects from compoTfv.R
###############################################################

source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/04_compoTfv.R')

###############################################################
# retrieve Gsp1, Gsp2, Dgsp1 and Dgsp2 of "TRUE" mixtures
# from PROTEST PLOTS
###############################################################

# retrieve list of TRUE mixed stands (i.e. where the two most abundant species
# represent > 75 % of the total basal area, and that, before the grouping of species)

# first remove TRUE (i.e. before the grouping of species) pure stands
listPure <- protestGSpProp[protestGSpProp$prop > 75, "Id_plac"]
protestGSpPropMixed <- protestGSpProp[!(protestGSpProp$Id_plac %in% listPure), ]

# keep only the two most abundant species on the PROTEST plots
tabMixed <- data.frame(matrix(ncol = ncol(protestGSpPropMixed), nrow = 0))
colnames(tabMixed) <- colnames(protestGSpPropMixed)
for (i in unique(protestGSpPropMixed$Id_plac)){
  plac <- protestGSpPropMixed[protestGSpPropMixed$Id_plac == i,]
  plac <- plac[c(1:2),]
  tabMixed <- rbind(tabMixed, plac)
}

# change format long --> wide
tabMixed <- dcast(tabMixed, formula =  Id_plac ~ Cod_ess)
tabMixed$sumSp <- apply(tabMixed[,-1], 1, sum, na.rm = TRUE)
# keep only plots where sum(sp1 + sp1) > 75% G (for the mixtures we are interested in)
tabMixed <- tabMixed[tabMixed$sumSp > 75, ]
trueMixedBeechFir <- tabMixed[!is.na(tabMixed$HET) & !is.na(tabMixed$S.P), "Id_plac"]
trueMixedBeechSpruce <- tabMixed[!is.na(tabMixed$HET) & !is.na(tabMixed$EPC), "Id_plac"]
trueMixedFirSpruce <- tabMixed[!is.na(tabMixed$S.P) & !is.na(tabMixed$EPC), "Id_plac"]

# calculate link dgsp1 / dgsp2 & gFir / gSpruce
ratGDg <- function(plotList, sp1, sp2){
  # for each mixture type
  tmbf <- arbres.vivant[arbres.vivant$Id_plac %in% plotList, ]
  plotDgbf <- data.frame(matrix(ncol = 3, nrow = length(plotList)))
  colnames(plotDgbf) <- c('Id_plac', 'dgSp1', 'dgSp2')
  plotDgbf$Id_plac <- plotList
  for (i in unique(tmbf$Id_plac)){
    # for each plot
    plac <- tmbf[tmbf$Id_plac == i,]
    # g calculation
    # assign the G of other species to the target species depending on the
    # proportion of the target species
    gTot <- unique(protestGSpProp[protestGSpProp$Id_plac == i, "G"])
    gSp1 <- protestGSpProp[protestGSpProp$Id_plac == i & protestGSpProp$Cod_ess == sp1, "Gsp"]
    gSp2 <- protestGSpProp[protestGSpProp$Id_plac == i & protestGSpProp$Cod_ess == sp2, "Gsp"]
    propSp1 <- gSp1 / (gSp1 + gSp2)
    propSp2 <- gSp2 / (gSp1 + gSp2)
    gRemain <- gTot - gSp1 - gSp2
    if (gRemain > 0){
      gSp1 <- gSp1 + gRemain * propSp1
      gSp2 <- gSp2 + gRemain * propSp2
    }
    plotDgbf[plotDgbf$Id_plac == i, 'gSp1'] <- gSp1
    plotDgbf[plotDgbf$Id_plac == i, 'gSp2'] <- gSp2
    # dg calculation
    plotDgbf[plotDgbf$Id_plac == i, 'dgSp1'] <- sqrt(sum((plac[plac$Cod_ess == sp1, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp1,]))
    plotDgbf[plotDgbf$Id_plac == i, 'dgSp2'] <- sqrt(sum((plac[plac$Cod_ess == sp2, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp2,]))
    # verify Gtot
    plotDgbf[plotDgbf$Id_plac == i, 'gTot'] <- gTot
  }
  # g1 / gtot
  plotDgbf$ratG <- plotDgbf$gSp1 / (plotDgbf$gSp1 + plotDgbf$gSp2)
  # dg1 / dg2
  plotDgbf$ratDg <- plotDgbf$dgSp1 / plotDgbf$dgSp2
  plotDgbf <- merge(plotDgbf, protestPlotsDf, by = 'Id_plac', all.x = TRUE, all.y = FALSE)
  return(plotDgbf)
}

###############################################################
# assign species G to all forest plots
###############################################################

# remove forest plots where gPred == 0
forestPlotsDf <- forestPlotsDf[forestPlotsDf$gPred > 0, ]

# remove forest plots where gelNttn == "hydro"
forestPlotsDf <- forestPlotsDf[forestPlotsDf$gelNttn != "hydro", ]

forestPlotsDf$gBeech <- NA
forestPlotsDf$gOak <- NA
forestPlotsDf$gFir <- NA
forestPlotsDf$gSpruce <- NA

################################### 1 - First case: pure stands
# --> nothing to calculate, gPred is available
# beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gPred"]
# oak
forestPlotsDf[forestPlotsDf$compoSp == 'oak', "gOak"] <- forestPlotsDf[forestPlotsDf$compoSp == 'oak', "gPred"]
# fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gPred"]
# spruce
forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gPred"]

################################### 2 - Second case: deciduous - coniferous mixtures
# g deciduous and g coniferous are available
# beech - fir
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gPred"] *
                                                                      forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "p100gfP"] / 100
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gPred"] *
                                                                      (100 - forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "p100gfP"]) / 100
# beech - spruce
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gPred"] *
                                                                      forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "p100gfP"] / 100
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gPred"] *
                                                                      (100 - forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "p100gfP"]) / 100

################################### 3 - Third case: coniferous - coniferous mixtures
# g deciduous and g coniferous are not available
# fir - spruce
# model Gfir / Gspruce ratio
ratGFirSpruce <- ratGDg(trueMixedFirSpruce, 'S.P', 'EPC')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") + I(Cd_hydr == "0") + I(Cd_hydr == "1") +
                  # I(Cd_hydr == "2") + I(Cd_hydr == "3") + I(Cd_crbn == "0") +
                  # I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")
modgFirSpruce <- lm(ratG ~ 1, data = ratGFirSpruce) # expoNS + I(dgPred^2) + I(Cd_crbn == "3")
summary(modgFirSpruce)

# b <- b + rnorm(sd(residuals))
# set the minimum proportion of G for a species in the mixture
minP <- 0.25
# minP < b < 1 - minP
forestPlotsDf$b <- -999
# keep generating b until all b values are in the range minP < b < 1 - minP
while(sum(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"] < minP | forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"] > (1 - minP)) > 0){
  forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$b < minP | forestPlotsDf$b > (1 - minP), "b"] <- predict(modgFirSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$b < minP | forestPlotsDf$b > (1 - minP),]) +
                                                              rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$b < minP | forestPlotsDf$b > (1 - minP),]), 0, sd(residuals(modgFirSpruce)))
  print(sum(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"] < minP | forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"] > (1 - (minP))))
}
hist(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"], breaks = 100)
range(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"])

# gSpruce
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"] * (1 - forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"])

# gFir
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"] - forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"]

###############################################################
# model Dg1/Dg2 ratio with available variables
###############################################################

# Beech - Fir
ratDgBeechFir <- ratGDg(trueMixedBeechFir, 'HET', 'S.P')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") + I(Cd_hydr == "0") + I(Cd_hydr == "1") +
                  # I(Cd_hydr == "2") + I(Cd_hydr == "3") + I(Cd_crbn == "0") +
                  # I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")
modBeechFir <- lm(ratDg ~ I(Cd_hydr == "1"), data = ratDgBeechFir) # I(Cd_hydr == "1")
summary(modBeechFir)

# Beech - Spruce
ratDgBeechSpruce <- ratGDg(trueMixedBeechSpruce, 'HET', 'EPC')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") + I(Cd_hydr == "0") + I(Cd_hydr == "1") +
                  # I(Cd_hydr == "2") + I(Cd_hydr == "3") + I(Cd_crbn == "0") +
                  # I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")
modBeechSpruce <- lm(ratDg ~ alti + slope + expoNS +
                  I(Cd_hydr == "2") + I(Cd_crbn == "0"), data = ratDgBeechSpruce) # alti + slope + expoNS + I(Cd_hydr == "2") + I(Cd_crbn == "0")
summary(modBeechSpruce)

# Fir - Spruce
ratDgFirSpruce <- ratGDg(trueMixedFirSpruce, 'S.P', 'EPC')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") + I(Cd_hydr == "0") + I(Cd_hydr == "1") +
                  # I(Cd_hydr == "2") + I(Cd_hydr == "3") + I(Cd_crbn == "0") +
                  # I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")
modFirSpruce <- lm(ratDg ~ expoNS + expoEW + I(Cd_hydr == "1"), data = ratDgFirSpruce) # expoNS + expoEW + I(Cd_hydr == "1")
summary(modFirSpruce)

###############################################################
# assign species Dg to all forest plots
###############################################################

forestPlotsDf$dgBeech <- NA
forestPlotsDf$dgOak <- NA
forestPlotsDf$dgFir <- NA
forestPlotsDf$dgSpruce <- NA

################################### 1 - First case: pure stands
# --> nothing to calculate, gPred is available
# beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech', "dgBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech', "dgPred"]
# oak
forestPlotsDf[forestPlotsDf$compoSp == 'oak', "dgOak"] <- forestPlotsDf[forestPlotsDf$compoSp == 'oak', "dgPred"]
# fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir', "dgFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir', "dgPred"]
# spruce
forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "dgSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "dgPred"]

################################### 2 - Second case: mixed stands

forestPlotsDf$a <- -999

# Beech - Fir
# condition: a values are in the range of a values obtained with protest plots
# # keep generating a until all a values are in the range of a value obtained with protest plots
minA <- min(ratDgBeechFir$dgSp1 / ratDgBeechFir$dgSp2)
maxA <- max(ratDgBeechFir$dgSp1 / ratDgBeechFir$dgSp2)
while(sum(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "a"] < minA | forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "a"] > maxA) > 0){
  forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' & forestPlotsDf$a < minA, 'a'] <- predict(modBeechFir, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' & forestPlotsDf$a < minA,]) +
                                                              rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' & forestPlotsDf$a < minA,]), 0, sd(residuals(modBeechFir)))
  forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' & forestPlotsDf$a > maxA, 'a'] <- predict(modBeechFir, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' & forestPlotsDf$a > maxA,]) +
                                                            rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' & forestPlotsDf$a > maxA,]), 0, sd(residuals(modBeechFir)))
  print(sum(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "a"] < minA | forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "a"] > maxA))
}

# Dg Beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgPred"] *
                                                              sqrt((forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gBeech"] +
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "a"]^2) *
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gFir"]) /
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gBeech"] +
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gFir"]))
# Dg Fir
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgBeech"] /
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "a"]

# Beech - Spruce
# condition: a values are in the range of a values obtained with protest plots
# # keep generating a until all a values are in the range of a value obtained with protest plots
minA <- min(ratDgBeechSpruce$dgSp1 / ratDgBeechSpruce$dgSp2)
maxA <- max(ratDgBeechSpruce$dgSp1 / ratDgBeechSpruce$dgSp2)
while(sum(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "a"] < minA | forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "a"] > maxA) > 0){
  forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' & forestPlotsDf$a < minA, 'a'] <- predict(modBeechSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' & forestPlotsDf$a < minA,]) +
                                                              rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' & forestPlotsDf$a < minA,]), 0, sd(residuals(modBeechSpruce)))
  forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' & forestPlotsDf$a > maxA, 'a'] <- predict(modBeechSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' & forestPlotsDf$a > maxA,]) +
                                                            rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' & forestPlotsDf$a > maxA,]), 0, sd(residuals(modBeechSpruce)))
  print(sum(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "a"] < minA | forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "a"] > maxA))
}

# Dg Beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgPred"] *
                                                              sqrt((forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gBeech"] +
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "a"]^2) *
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gSpruce"]) /
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gBeech"] +
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gSpruce"]))

# Dg Spruce
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgBeech"] /
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "a"]

# Fir - Spruce
# condition: a values are in the range of a values obtained with protest plots
# # keep generating a until all a values are in the range of a value obtained with protest plots
minA <- min(ratDgFirSpruce$dgSp1 / ratDgFirSpruce$dgSp2)
maxA <- max(ratDgFirSpruce$dgSp1 / ratDgFirSpruce$dgSp2)
while(sum(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "a"] < minA | forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "a"] > maxA) > 0){
  forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$a < minA, 'a'] <- predict(modFirSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$a < minA,]) +
                                                              rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$a < minA,]), 0, sd(residuals(modFirSpruce)))
  forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$a > maxA, 'a'] <- predict(modFirSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$a > maxA,]) +
                                                            rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' & forestPlotsDf$a > maxA,]), 0, sd(residuals(modFirSpruce)))
  print(sum(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "a"] < minA | forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "a"] > maxA))
}

# Dg Fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgPred"] *
                                                              sqrt((forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] +
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "a"]^2) *
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"]) /
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] +
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"]))
# Dg Spruce
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgFir"] /
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "a"]


###############################################################
# calculate nSp1 & nSp2 from g and dg
###############################################################

forestPlotsDf$nBeech <- NA
forestPlotsDf$nOak <- NA
forestPlotsDf$nFir <- NA
forestPlotsDf$nSpruce <- NA

# beech
forestPlotsDf[forestPlotsDf$compoSp == "beech", "nBeech"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "beech", "gBeech"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "beech", "dgBeech"]^2)

# oak
forestPlotsDf[forestPlotsDf$compoSp == "oak", "nOak"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "oak", "gOak"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "oak", "dgOak"]^2)

# fir
forestPlotsDf[forestPlotsDf$compoSp == "fir", "nFir"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "fir", "gFir"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "fir", "dgFir"]^2)

# spruce
forestPlotsDf[forestPlotsDf$compoSp == "spruce", "nSpruce"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "spruce", "gSpruce"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "spruce", "dgSpruce"]^2)

# beech-fir
forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "nBeech"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "gBeech"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "dgBeech"]^2)

forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "nFir"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "gFir"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "dgFir"]^2)

# beech-spruce
forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "nBeech"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "gBeech"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "dgBeech"]^2)

forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "nSpruce"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "gSpruce"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "dgSpruce"]^2)

# fir-spruce
forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "nFir"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "gFir"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "dgFir"]^2)

forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "nSpruce"] <- (4 * forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "gSpruce"]) /
                                                             (pi * forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "dgSpruce"]^2)
#

###############################################################
# test dg
###############################################################

forestPlotsDf$dTot <- NA

# Beech - Fir
a <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gBeech"]
b <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gFir"]
c <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgBeech"]^2
d <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgFir"]^2
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dTot"] <- (a + b) / ((a / c) + (b / d))
hist(sqrt(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dTot"]) - forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgPred"])

# Beech - Spruce
a <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gBeech"]
b <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gSpruce"]
c <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgBeech"]^2
d <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgSpruce"]^2
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dTot"] <- (a + b) / ((a / c) + (b / d))
hist(sqrt(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dTot"]) - forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgPred"])

# Fir - Spruce
a <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"]
b <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"]
c <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgFir"]^2
d <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgSpruce"]^2
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dTot"] <- (a + b) / ((a / c) + (b / d))
hist(sqrt(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dTot"]) - forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgPred"])

# density plot of prediction vs protest plots
# beech - spruce
plot(density(ratDgBeechSpruce$dgSp1), col = 'green', ylim = c(0, 10))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', 'dgBeech']))

plot(density(ratDgBeechSpruce$dgSp2, n = 100), col = 'green', ylim = c(0, 20))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', 'dgSpruce']))

# beech - fir
plot(density(ratDgBeechFir$dgSp1), col = 'green', ylim = c(0, 10))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', 'dgBeech']))

plot(density(ratDgBeechFir$dgSp2), col = 'green', xlim = c(0, 1), ylim = c(0, 10))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', 'dgFir']))

# fir - spruce
# dg
plot(density(ratDgFirSpruce$dgSp1), col = 'green', xlim = c(0, 1))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'dgFir']))

plot(density(ratDgFirSpruce$dgSp2), col = 'green', xlim = c(0, 1), ylim = c(0, 10))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'dgSpruce']))

###############################################################
# test g
###############################################################

# closing the balance of G (fir - spruce mixtures)
hist(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"] + forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] - forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"])

# G proportion of species (must be between 0.25 and 0.75)
range(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] / forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"])
range(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"] / forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"])

# g
# here we do not look at the density of protest Gsp1 vs forestPlots Gsp1
# because protest Gsp1 is measured on a small plot while forestPlots are
# 1 ha large
# we therefore look at G proportion here:

plot(density(ratGFirSpruce$gSp1 / (ratGFirSpruce$gSp1 + ratGFirSpruce$gSp2)), col = 'green', ylim = c(0, 2.6))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gFir'] / forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gPred']))

plot(density(ratGFirSpruce$gSp2 / (ratGFirSpruce$gSp1 + ratGFirSpruce$gSp2)), col = 'green', ylim = c(0, 2.6))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gSpruce'] / forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gPred']))

###############################################################
# test n --> ((n1*dg1²) + (n2*dg2²)) / n1 + n2   = dgt²
###############################################################

# beech - fir
dgPred <- forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "dgPred"]
n1  <- forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "nBeech"]
n2  <- forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "nFir"]
dg1 <- forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "dgBeech"]
dg2 <- forestPlotsDf[forestPlotsDf$compoSp == "beech-fir", "dgFir"]
hist((((n1*dg1^2) + (n2*dg2^2)) / (n1 + n2)) - dgPred^2)

# beech - spruce
dgPred <- forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "dgPred"]
n1  <- forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "nBeech"]
n2  <- forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "nSpruce"]
dg1 <- forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "dgBeech"]
dg2 <- forestPlotsDf[forestPlotsDf$compoSp == "beech-spruce", "dgSpruce"]
hist((((n1*dg1^2) + (n2*dg2^2)) / (n1 + n2)) - dgPred^2)

# fir - spruce
dgPred <- forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "dgPred"]
n1  <- forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "nFir"]
n2  <- forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "nSpruce"]
dg1 <- forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "dgFir"]
dg2 <- forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "dgSpruce"]
hist((((n1*dg1^2) + (n2*dg2^2)) / (n1 + n2)) - dgPred^2)

###############################################################
# test a --> minA < a < maxA
###############################################################

# beech - fir
# hist
hist(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' , 'a'], breaks = 100)
hist(ratDgBeechFir$dgSp1 / ratDgBeechFir$dgSp2, add = TRUE, col ='red')
# range
range(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' , 'a'])
range(ratDgBeechFir$dgSp1 / ratDgBeechFir$dgSp2)

# beech - spruce
# hist
hist(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' , 'a'], breaks = 100)
hist(ratDgBeechSpruce$dgSp1 / ratDgBeechSpruce$dgSp2, add = TRUE, col ='red')
# range
range(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' , 'a'])
range(ratDgBeechSpruce$dgSp1 / ratDgBeechSpruce$dgSp2)

# fir - spruce
# hist
hist(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' , 'a'], breaks = 100)
hist(ratDgFirSpruce$dgSp1 / ratDgFirSpruce$dgSp2, add = TRUE, col ='red')
# range
range(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' , 'a'])
range(ratDgFirSpruce$dgSp1 / ratDgFirSpruce$dgSp2)




# ---> G / Dg a 7.5 vec JMM
#
# ---> true mixture > 75
#     --> réaffecter G aux deux espèces
#     --> on ne reaffecte pas les dg des autres espèces dans les dg des deux
#         espèce cibles (on ne veut pas biaiser l'ontogénie des 2 sp). JMM --> calibration
#         des Dg seulement sur les sp cibles?


---> checker RDI
----> unit
G  -> m2
dg -> m
compo -> beech-spruce
fert ->
