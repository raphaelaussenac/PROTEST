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
    plotDgbf[plotDgbf$Id_plac == i, 'gSp1'] <- sum(plac[plac$Cod_ess == sp1, "g"])
    plotDgbf[plotDgbf$Id_plac == i, 'gSp2'] <- sum(plac[plac$Cod_ess == sp2, "g"])
    # dg calculation
    plotDgbf[plotDgbf$Id_plac == i, 'dgSp1'] <- sqrt(sum((plac[plac$Cod_ess == sp1, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp1,]))
    plotDgbf[plotDgbf$Id_plac == i, 'dgSp2'] <- sqrt(sum((plac[plac$Cod_ess == sp2, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp2,]))
  }
  # G ratio
  plotDgbf$ratG <- plotDgbf$gSp1 / plotDgbf$gSp2
  # dg ratio
  plotDgbf$ratDg <- plotDgbf$dgSp1 / plotDgbf$dgSp2
  plotDgbf <- merge(plotDgbf, protestPlotsDf, by = 'Id_plac', all.x = TRUE, all.y = FALSE)
  return(plotDgbf)
}

###############################################################
# assign species G to all forest plots
###############################################################

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
                  # I(greco == "H") + I(greco == "C") + I(Code_hydro == "0") + I(Code_hydro == "1") +
                  # I(Code_hydro == "2") + I(Code_hydro == "3") + I(Code_carbonate == "0") +
                  # I(Code_carbonate == "1") + I(Code_carbonate == "2") + I(Code_carbonate == "3")
modgFirSpruce <- lm(ratG ~ slope, data = ratGFirSpruce)
summary(modgFirSpruce)

# b + rnorm(sd(residuals))
forestPlotsDf$b <- NA
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"] <- predict(modgFirSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce',]) +
                                                            rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce',]), 0, sd(residuals(modgFirSpruce)))

# g
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"] /
                                                                   (1 + forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"])
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"] /
                                                                   (1 + (1 / forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"]) )

###############################################################
# model Dg1/Dg2 ratio with available variables
###############################################################

# Beech - Fir
ratDgBeechFir <- ratGDg(trueMixedBeechFir, 'HET', 'S.P')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") + I(Code_hydro == "0") + I(Code_hydro == "1") +
                  # I(Code_hydro == "2") + I(Code_hydro == "3") + I(Code_carbonate == "0") +
                  # I(Code_carbonate == "1") + I(Code_carbonate == "2") + I(Code_carbonate == "3")
modBeechFir <- lm(ratDg ~ I(Code_hydro == "1"), data = ratDgBeechFir)
summary(modBeechFir)

# Beech - Spruce
ratDgBeechSpruce <- ratGDg(trueMixedBeechSpruce, 'HET', 'EPC')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") + I(Code_hydro == "0") + I(Code_hydro == "1") +
                  # I(Code_hydro == "2") + I(Code_hydro == "3") + I(Code_carbonate == "0") +
                  # I(Code_carbonate == "1") + I(Code_carbonate == "2") + I(Code_carbonate == "3")
modBeechSpruce <- lm(ratDg ~ alti + slope + expoNS +
                  I(Code_hydro == "2") + I(Code_carbonate == "0"), data = ratDgBeechSpruce)
summary(modBeechSpruce)

# Fir - Spruce
ratDgFirSpruce <- ratGDg(trueMixedFirSpruce, 'S.P', 'EPC')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") + I(Code_hydro == "0") + I(Code_hydro == "1") +
                  # I(Code_hydro == "2") + I(Code_hydro == "3") + I(Code_carbonate == "0") +
                  # I(Code_carbonate == "1") + I(Code_carbonate == "2") + I(Code_carbonate == "3")
modFirSpruce <- lm(ratDg ~ expoNS + expoEW + I(Code_hydro == "1"), data = ratDgFirSpruce)
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

forestPlotsDf$a <- NA

# Beech - Fir
# a + rnorm(sd(residuals))
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "a"] <- predict(modBeechFir, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir',]) +
                                                            rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir',]), 0, sd(residuals(modBeechFir)))
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
#
# Beech - Spruce
# a + rnorm(sd(residuals))
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "a"] <- predict(modBeechSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce',]) +
                                                            rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce',]), 0, sd(residuals(modBeechSpruce)))
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
#
# Fir - Spruce
# a + rnorm(sd(residuals))
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "a"] <- predict(modFirSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce',]) +
                                                            rnorm(nrow(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce',]), 0, sd(residuals(modFirSpruce)))
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
#

###############################################################
# test
###############################################################

forestPlotsDf$dTot <- NA

# closing the balance of Dg
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

# closing the balance of G (fir - spruce mixtures)
hist(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"] + forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] - forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"])




---> G / Dg a 7.5 vec JMM
---> ajouter greco, code hydro / carbonate... dans les modèles complets


---> true mixture > 75
    --> réaffecter G aux deux espèces
    --> on ne reaffecte pas les dg des autres espèces dans les dg des deux
        espèce (on ne veut pas biaiser l'ontogénie des 2 sp). JMM --> calibration
        des Dg seulement sur les sp cibles.
