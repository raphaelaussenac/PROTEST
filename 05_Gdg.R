###############################################################
# assign species G to all forest plots
###############################################################

forestPlotsDf$gBeech <- NA
forestPlotsDf$gOak <- NA
forestPlotsDf$gFir <- NA
forestPlotsDf$gSpruce <- NA

# 1 - First case: pure stands
# --> nothing to calculate, gPred is available
# beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gPred"]
# oak
forestPlotsDf[forestPlotsDf$compoSp == 'oak', "gOak"] <- forestPlotsDf[forestPlotsDf$compoSp == 'oak', "gPred"]
# fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gPred"]
# spruce
forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gPred"]

# 2 - Second case: deciduous - coniferous mixtures
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

# 3 - Third case: coniferous - coniferous mixtures
# g deciduous and g coniferous are not available
# fir - spruce

--> ratio G1/G2


###############################################################
# retrieve Dg1 and Dg2 of "TRUE" mixtures from PROTEST PLOTS
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

# calculate link dgsp1 - dgsp2
diffDg <- function(plotList, sp1, sp2){
  # for each mixture type
  tmbf <- arbres.vivant[arbres.vivant$Id_plac %in% plotList, ]
  plotDgbf <- data.frame(matrix(ncol = 3, nrow = length(plotList)))
  colnames(plotDgbf) <- c('Id_plac', 'dgSp1', 'dgSp2')
  plotDgbf$Id_plac <- plotList
  for (i in unique(tmbf$Id_plac)){
    # for each plot
    plac <- tmbf[tmbf$Id_plac == i,]
    # dg calculation
    plotDgbf[plotDgbf$Id_plac == i, 'dgSp1'] <- sqrt(sum((plac[plac$Cod_ess == sp1, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp1,]))
    plotDgbf[plotDgbf$Id_plac == i, 'dgSp2'] <- sqrt(sum((plac[plac$Cod_ess == sp2, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp2,]))
  }
  # dg difference and ratio
  plotDgbf$diffDg <- plotDgbf$dgSp1 - plotDgbf$dgSp2
  plotDgbf$ratDg <- plotDgbf$dgSp1 / plotDgbf$dgSp2
  plotDgbf <- merge(plotDgbf, protestPlotsDf, by = 'Id_plac', all.x = TRUE, all.y = FALSE)
  return(plotDgbf)
}

###############################################################
# model Dg1/Dg2 ratio from available variables
###############################################################

# Beech - Fir
diffDgBeechFir <- diffDg(trueMixedBeechFir, 'HET', 'S.P')
# m0 --> diffDg ~ alti + slope + expoNS + expoEW + dgPred + gPred
                # + I(CODE_TF == 'FF31') + I(CODE_TF == "FF32") + I(CODE_TF == "FF1-09-09")
                # + I(CODE_TF == "FF2G61-61") + I(CODE_TF == "FF1-00-00")
modBeechFir <- lm(ratDg ~ slope + I(dgPred^2), data = diffDgBeechFir)
summary(modBeechFir)

# Beech - Spruce
diffDgBeechSpruce <- diffDg(trueMixedBeechSpruce, 'HET', 'EPC')
# m0 --> diffDg ~ alti + slope + expoNS + expoEW + dgPred + gPred
                # + I(CODE_TF == 'FF31') + I(CODE_TF == "FF32") + I(CODE_TF == "FF1-09-09")
                # + I(CODE_TF == "FF2G61-61") + I(CODE_TF == "FF1-00-00")
modBeechSpruce <- lm(ratDg ~ I(dgPred^2) + I(CODE_TF == 'FF31')
                    + I(CODE_TF == "FF32") + I(CODE_TF == "FF1-09-09")
                    + I(CODE_TF == "FF2G61-61"), data = diffDgBeechSpruce)
summary(modBeechSpruce)

# Fir - Spruce
diffDgFirSpruce <- diffDg(trueMixedFirSpruce, 'S.P', 'EPC')
# m0 --> diffDg ~ alti + slope + expoNS + expoEW + dgPred + gPred
                # + I(CODE_TF == 'FF31') + I(CODE_TF == "FF32") + I(CODE_TF == "FF1-09-09")
                # + I(CODE_TF == "FF2G61-61") + I(CODE_TF == "FF1-00-00")
modFirSpruce <- lm(ratDg ~ dgPred + I(CODE_TF == "FF32") + I(alti^2), data = diffDgFirSpruce)
summary(modFirSpruce)



















###############################################################
# Dg calculation
###############################################################

# --> objective: 1 G and 1 Dg per species and per plot
forestPlotsDf$dgBeech <- NA
forestPlotsDf$dgOak <- NA
forestPlotsDf$dgFir <- NA
forestPlotsDf$dgSpruce <- NA
forestPlotsDf$gBeech <- NA
forestPlotsDf$gOak <- NA
forestPlotsDf$gFir <- NA
forestPlotsDf$gSpruce <- NA

# 1 - First case: pure stands
# --> nothing to calculate, gPred and dgPred are available
# beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech', "dgBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech', "dgPred"]
forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gPred"]
# oak
forestPlotsDf[forestPlotsDf$compoSp == 'aok', "dgOak"] <- forestPlotsDf[forestPlotsDf$compoSp == 'aok', "dgPred"]
forestPlotsDf[forestPlotsDf$compoSp == 'aok', "gOak"] <- forestPlotsDf[forestPlotsDf$compoSp == 'aok', "gPred"]
# fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir', "dgFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir', "dgPred"]
forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gPred"]
# spruce
forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "dgSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "dgPred"]
forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gPred"]

# 2 - Second case: deciduous - conifer mixture (beech - fir, beech - spruce)

# g deciduous and g coniferous are available
# beech - fir
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gPred"] *
                                                                      forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "p100gfP"] / 100
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gPred"] *
# beech - spruce
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gPred"] *
                                                                      forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "p100gfP"] / 100
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gPred"] *
                                                                      (100 - forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "p100gfP"]) / 100

# dg deciduous and dg coniferous must be calculated
# beech - fir
aBeechFir <- predict(modBeechFir, newdata = forestPlotsDf)




























# 2 - Second case: conifer - conifer mixtures (fir - spruce)
# 2a - select a forest plot among those with the above composition
# 2b - randomly pick a PROTEST plot with the same composition in the same TFV type
# 2c - calculate Gsp1, Gsp2, dgsp1 and dgsp2 for the PROTEST plot
# 2d - assign these values to the forest plot selected

# list of forest plots with conifer - conifer mixtures
listId <- forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce", "id"]





# calculate Gsp1, Gsp2, dgsp1 and dgsp2 for the PROTEST plot --> TRUE (i.e.
# before the grouping of species) conifer - conifer mixtures


# remove TRUE (i.e. before the grouping of species) pure stands
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
# keep only TRUE (i.e. before the grouping of species) mixed stands of the
# specified composition
firSpruce <- tabMixed[tabMixed$Cod_ess %in% c('S.P', 'EPC'),]
firSpruce <- ddply(firSpruce, .(Id_plac), summarise, prop = sum(prop))
firSpruce <- firSpruce[firSpruce$prop > 75, 'Id_plac']
# Gsp1, Gsp2, dgsp1 and dgsp2
tab <- data.frame(matrix(ncol = 5, nrow = length(firSpruce)))
colnames(tab) <- c('Id_plac', 'gSp1', 'gSp2', 'dgSp1', 'dgSp2')
tab$Id_plac <- firSpruce
for (i in 1:length(firSpruce)){
  plac <- arbres.vivant[arbres.vivant$Id_plac == tab[i, 'Id_plac'], ]
  gSp <- ddply(plac, .(Cod_ess), summarise, G = sum(g))
  tab[i, 'gSp1'] <- gSp[gSp$Cod_ess == "S.P", "G"]
  tab[i, 'gSp2'] <- gSp[gSp$Cod_ess == "EPC", "G"]
  tab[i, 'dgSp1'] <- sqrt(sum((plac[plac$Cod_ess == 'S.P', "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == 'S.P',]))
  tab[i, 'dgSp2'] <- sqrt(sum((plac[plac$Cod_ess == 'EPC', "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == 'EPC',]))
}
tab$diffDg <- - tab$dgSp1 - tab$dgSp2
tab$ratDg <- - tab$dgSp1 / tab$dgSp2
tab <- merge(tab, protestPlotsDf, by = 'Id_plac', all.x = TRUE, all.y = FALSE)



plot(tab$gSp1, tab$dgSp1)



# m0 --> diffDg ~ alti + slope + expoNS + expoEW + dgPred
                # + I(CODE_TF == 'FF31') + I(CODE_TF == "FF32") + I(CODE_TF == "FF1-09-09")
                # + I(CODE_TF == "FF2G61-61") + I(CODE_TF == "FF1-00-00")
mod <- lm(diffDg ~  dgPred + I(CODE_TF == "FF32") + I(alti^2), data = tab)
summary(mod)






mod <- lm(ratDg ~ alti + slope + expoNS + expoEW + dgPred + gPred, data = tab)
summary(mod)

for (i in listId){
}




# randomly select a PROTEST plot
plac <- firSpruce[round(runif(1, min = 1, max = nrow(firSpruce))), "Id_plac"]
# calculate Gsp1, Gsp2, dgsp1 and dgsp2 for the PROTEST plot
gSp1 <- protestGSpProp[protestGSpProp$Id_plac == plac & protestGSpProp$Cod_ess, ]

nrow(forestPlotsDf[forestPlotsDf$compoSp == "fir-spruce",])




g <- n*(pi*dgSp1^2)/4




  dgSp1 <- sqrt(sum((plac[plac$Cod_ess == sp1, "Diam"])^2) / nrow(plac))
  dgSp2 <- sqrt(sum((plac[plac$Cod_ess == sp2, "Diam"])^2) / nrow(plac))


a <- protestGSpProp[protestGSpProp$Id_plac %in% firSpruce$Id_plac & protestGSpProp$Cod_ess == 'S.P', "prop"]
b <- protestGSpProp[protestGSpProp$Id_plac %in% firSpruce$Id_plac & protestGSpProp$Cod_ess == 'EPC', 'prop']
plot(a,b)


  # retrieve composition (unnecessary)
  # forestPlotsDf[forestPlotsDf$id %in% listId, "compoSp"]
  compoSp <- 'fir-spruce'
  # retrieve TFV type
  tfv <- forestPlotsDf[forestPlotsDf$id == i, "CODE_TF"]
  # select all PROTEST plot with the same composition in the same TFV type



  firSpruce <- protestGSpProp[protestGSpProp$Cod_ess %in% c('S.P', 'EPC'),]



  listProtest <- ddply(arbres.vivant, .(Id_plac), summarise, G = sum(g))






  listProtest <- protestPlotsDf[protestPlotsDf$compoSp == compoSp
                                & protestPlotsDf$CODE_TF == tfv, ]


  protestNewSp[protestNewSp$Id_plac == plac,]

  # calculate Gsp1, Gsp2, dgsp1 and dgsp2 for the PROTEST plot
  arbres.vivant























# 3 - Third case: deciduous - conifer mixture
    # 2a - G deciduous and G conifers calculation (with gPred and p100gfP)
forestPlotsDf$GspD <- NA
forestPlotsDf[forestPlotsDf$compoDCM == 'MDC', "GspD"] <-
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC', "gPred"] *
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC', "p100gfP"] / 100
forestPlotsDf$GspC <- NA
forestPlotsDf[forestPlotsDf$compoDCM == 'MDC', "GspC"] <-
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC', "gPred"] -
                          forestPlotsDf[forestPlotsDf$compoDCM == 'MDC', "GspD"]

    # 2b - calculate Dg deciduous and Dg coniferous on each protest plot
protestPlotsDf$compo <- NA
protestPlotsDf$sp1 <- NA
protestPlotsDf$sp2 <- NA
protestPlotsDf$dgSp1 <- NA
protestPlotsDf$dgSp2 <- NA
protestPlotsDf$diffDg <- NA

    # retrieve protest points for each sp combination obtained on the study area
couples <- unique(paste(forestPlotsDf[forestPlotsDf$compo == 'mixte', "sp1"],
                      forestPlotsDf[forestPlotsDf$compo == 'mixte', "sp2"]))

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
        # -> asign compo = mixte to protestPlotsDf
        protestPlotsDf[protestPlotsDf$Id_plac == i, "compo"] <- "mixte"
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
