library(betareg)

###############################################################
# import objects from compoTfv.R
###############################################################

source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/src/composition.R')

###############################################################
# retrieve Gsp1, Gsp2, Dgsp1 and Dgsp2 of "TRUE" mixtures
# from PROTEST PLOTS
###############################################################

# retrieve list of TRUE mixed stands (i.e. where the two most abundant species
# represent > 75 % of the basal area (d > 175), and that, before the grouping of species)

# First calculate sp proportion in the canopy
protestGSpCanopy <- ddply(arbres.vivant, .(Id_plac, Cod_ess), summarise, Gsp = sum(g))
protestGCanopy <- ddply(arbres.vivant, .(Id_plac), summarise, G = sum(g))
protestGSpPropCanopy <- merge(protestGSpCanopy, protestGCanopy, by = "Id_plac")
protestGSpPropCanopy$prop <- protestGSpPropCanopy$Gsp * 100 / protestGSpPropCanopy$G
protestGSpPropCanopy <- protestGSpPropCanopy[order(protestGSpPropCanopy$Id_plac, protestGSpPropCanopy$prop, decreasing = TRUE),]

# then remove TRUE pure stands (i.e. before the grouping of species)
listPure <- protestGSpPropCanopy[protestGSpPropCanopy$prop > 75, "Id_plac"]
protestGSpPropMixed <- protestGSpPropCanopy[!(protestGSpPropCanopy$Id_plac %in% listPure), ]

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
  tm <- arbres.vivant[arbres.vivant$Id_plac %in% plotList, ]        # [t]rue [m]ixture
  gDgDf <- data.frame(matrix(ncol = 5, nrow = length(plotList)))
  colnames(gDgDf) <- c('Id_plac', 'dgSp1', 'dgSp2', 'gSp1', 'gSp2')
  gDgDf$Id_plac <- plotList
  for (i in unique(tm$Id_plac)){
    # for each plot
    plac <- tm[tm$Id_plac == i,]
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
    gDgDf[gDgDf$Id_plac == i, 'gSp1'] <- gSp1
    gDgDf[gDgDf$Id_plac == i, 'gSp2'] <- gSp2
    # dg calculation
    gDgDf[gDgDf$Id_plac == i, 'dgSp1'] <- sqrt(sum((plac[plac$Cod_ess == sp1, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp1,]))
    gDgDf[gDgDf$Id_plac == i, 'dgSp2'] <- sqrt(sum((plac[plac$Cod_ess == sp2, "Diam"]/100)^2) / nrow(plac[plac$Cod_ess == sp2,]))
  }
  # g1 / gtot
  gDgDf$ratG <- gDgDf$gSp1 / (gDgDf$gSp1 + gDgDf$gSp2)
  # dg1 / dg2
  gDgDf$ratDg <- gDgDf$dgSp1 / gDgDf$dgSp2
  gDgDf <- merge(gDgDf, protestPlotsDf, by = 'Id_plac', all.x = TRUE, all.y = FALSE)
  return(gDgDf)
}

###############################################################
# assign species G to all forest plots
###############################################################

forestPlotsDf$gBeech <- NA
forestPlotsDf$gOak <- NA
forestPlotsDf$gFir <- NA
forestPlotsDf$gSpruce <- NA

# 1 - First case: pure stands --------------------------------------------------
# --> nothing to calculate, gPred is available
# beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech', "gPred"]
# oak
forestPlotsDf[forestPlotsDf$compoSp == 'oak', "gOak"] <- forestPlotsDf[forestPlotsDf$compoSp == 'oak', "gPred"]
# fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir', "gPred"]
# spruce
forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "gPred"]

# 2 - Second case: deciduous - coniferous mixtures -----------------------------
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

# 3 - Third case: coniferous - coniferous mixtures -----------------------------
# fir - spruce
# model Gfir / GTot ratio
ratGFirSpruce <- ratGDg(trueMixedFirSpruce, 'S.P', 'EPC')
# # complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
#                   # I(greco == "H") + I(greco == "C") +
#                   # I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") + I(Cd_hydr == "3") +
#                   # I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")

# lm
# modgFirSpruce <- lm(ratG ~ slope, data = ratGFirSpruce)

# betareg
modgFirSpruce <- betareg(ratG ~ slope,
                      link = 'logit', data = ratGFirSpruce)

summary(modgFirSpruce)

# b <- pred
# bNoise <- pred & beta noise
forestPlotsDf$b <- -999
forestPlotsDf$bNoise <- -999

pred <- predict(modgFirSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', ], type = 'response')
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "b"] <- pred
N <- nrow(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', ])
phi <- as.numeric(modgFirSpruce$coefficient$precision)
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "bNoise"] <- rbeta(N, pred * phi, (1 - pred) * phi)

# plot prediction and noise
plot(density(ratGFirSpruce$ratG), ylim = c(0,5))
mu <- predict(modgFirSpruce, type = 'response')
lines(density(mu), col = "blue3")
phi <- as.numeric(modgFirSpruce$coefficient$precision)
noise <- rbeta(N, mu * phi, (1 - mu) * phi)
lines(density(noise), col = 'cyan')
lines(density(pred), col = 'green4')
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "bNoise"]), col = 'green')

# gSpruce
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"] * (1 - forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "bNoise"])

# gFir
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gPred"] * forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "bNoise"]

###############################################################
# model Dg1/Dg2 ratio with available variables
###############################################################

# Beech - Fir ------------------------------------------------------------------
ratDgBeechFir <- ratGDg(trueMixedBeechFir, 'HET', 'S.P')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") +
                  # I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") + I(Cd_hydr == "3") +
                  # I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")

# lm
# modBeechFir <- lm(ratDg ~ I(Cd_hydr == "1"), data = ratDgBeechFir)

# glm Gamma
modBeechFir <- glm(ratDg ~ I(Cd_hydr == "1"),
                   family = Gamma(link = "identity"),
                   data = ratDgBeechFir)

summary(modBeechFir)

# Beech - Spruce ---------------------------------------------------------------
ratDgBeechSpruce <- ratGDg(trueMixedBeechSpruce, 'HET', 'EPC')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") +
                  # I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") + I(Cd_hydr == "3") +
                  # I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")

# lm
# modBeechSpruce <- lm(ratDg ~ alti + slope + expoNS +
#                   I(Cd_hydr == "2") +
#                   I(Cd_crbn == "0"), data = ratDgBeechSpruce)

# glm Gamma
modBeechSpruce <- glm(ratDg ~ alti + slope + expoNS +
                      I(Cd_hydr == "2") +
                      I(Cd_crbn == "0"),
                      family = Gamma(link = "identity"),
                      data = ratDgBeechSpruce)

summary(modBeechSpruce)

# Fir - Spruce -----------------------------------------------------------------
ratDgFirSpruce <- ratGDg(trueMixedFirSpruce, 'S.P', 'EPC')
# complete model: alti + slope + rum + ph + expoNS + expoEW + dgPred + gPred +
                  # I(greco == "H") + I(greco == "C") +
                  # I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") + I(Cd_hydr == "3") +
                  # I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2") + I(Cd_crbn == "3")

# lm
# modFirSpruce <- lm(ratDg ~ expoNS + expoEW +
#                   I(Cd_hydr == "1"), data = ratDgFirSpruce)

# glm Gamma
modFirSpruce <- glm(ratDg ~ expoNS + expoEW +
                    I(Cd_hydr == "1"),
                    family = Gamma(link = "identity"),
                    data = ratDgFirSpruce)

summary(modFirSpruce)

###############################################################
# assign species Dg to all forest plots
###############################################################

forestPlotsDf$dgBeech <- NA
forestPlotsDf$dgOak <- NA
forestPlotsDf$dgFir <- NA
forestPlotsDf$dgSpruce <- NA

# 1 - First case: pure stands --------------------------------------------------
# --> nothing to calculate, gPred is available
# beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech', "dgBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech', "dgPred"]
# oak
forestPlotsDf[forestPlotsDf$compoSp == 'oak', "dgOak"] <- forestPlotsDf[forestPlotsDf$compoSp == 'oak', "dgPred"]
# fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir', "dgFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir', "dgPred"]
# spruce
forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "dgSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'spruce', "dgPred"]

# 2 - Second case: mixed stands ------------------------------------------------

forestPlotsDf$a <- -999
forestPlotsDf$aNoise <- -999

# Beech - Fir ------------------------------------------------------------------
pred <- predict(modBeechFir, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', ], type = 'response')
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', 'a'] <- pred

N <- nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', ])
shape_estim  <- drop(MASS::gamma.shape(modBeechFir)[1]$alpha)
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', 'aNoise'] <- rgamma(N, rate = shape_estim / pred, shape = shape_estim)

plot(density(ratDgBeechFir$ratDg), lwd = 2, xlim = c(0, 2), ylim = c(0,3))
lines(density(predict(modBeechFir, type = 'response')), col = "blue")
lines(density(rgamma(nrow(ratDgBeechFir), rate = shape_estim / predict(modBeechFir, type = 'response'), shape = shape_estim)), col = "cyan")
lines(density(pred), col = 'green4')
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', 'aNoise']), col = 'green')

# Dg Beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgPred"] *
                                                              sqrt((forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gBeech"] +
                                                              ((forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "aNoise"]^2) *
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gFir"])) /
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gBeech"] +
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "gFir"]))
# Dg Fir
forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "dgBeech"] /
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir', "aNoise"]

# Beech - Spruce ---------------------------------------------------------------
pred <- predict(modBeechSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', ], type = 'response')
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', 'a'] <- pred

N <- nrow(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', ])
shape_estim  <- drop(MASS::gamma.shape(modBeechSpruce)[1]$alpha)
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', 'aNoise'] <- rgamma(N, rate = shape_estim / pred, shape = shape_estim)

plot(density(ratDgBeechSpruce$ratDg), lwd = 2, xlim = c(0, 3), ylim = c(0,2.5))
lines(density(predict(modBeechSpruce, type = 'response')), col = "blue")
lines(density(rgamma(nrow(ratDgBeechSpruce), rate = shape_estim / predict(modBeechSpruce, type = 'response'), shape = shape_estim)), col = "cyan")
lines(density(pred), col = 'green4')
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', 'aNoise']), col = 'green')

# Dg Beech
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgBeech"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgPred"] *
                                                              sqrt((forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gBeech"] +
                                                              ((forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "aNoise"]^2) *
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gSpruce"])) /
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gBeech"] +
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "gSpruce"]))

# Dg Spruce
forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "dgBeech"] /
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce', "aNoise"]

# Fir - Spruce -----------------------------------------------------------------
pred <- predict(modFirSpruce, newdata = forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', ], type = 'response')
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'a'] <- pred

N <- nrow(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', ])
shape_estim  <- drop(MASS::gamma.shape(modFirSpruce)[1]$alpha)
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'aNoise'] <- rgamma(N, rate = shape_estim / pred, shape = shape_estim)

plot(density(ratDgFirSpruce$ratDg), lwd = 2, xlim = c(0, 3), ylim = c(0,2.5))
lines(density(predict(modFirSpruce, type = 'response')), col = "blue")
lines(density(rgamma(nrow(ratDgFirSpruce), rate = shape_estim / predict(modFirSpruce, type = 'response'), shape = shape_estim)), col = "cyan")
lines(density(pred), col = 'green4')
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'aNoise']), col = 'green')

# Dg Fir
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgFir"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgPred"] *
                                                              sqrt((forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] +
                                                              ((forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "aNoise"]^2) *
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"])) /
                                                              (forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gFir"] +
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "gSpruce"]))
# Dg Spruce
forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgSpruce"] <- forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "dgFir"] /
                                                              forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', "aNoise"]

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
# save in forestPlots object
###############################################################

# put back g Dg and N in the SpatialPolygonDataframe
forestPlots@data <- merge(forestPlots@data, forestPlotsDf[, c('WKTid', "gBeech", "gOak", "gFir", "gSpruce",
                                          "dgBeech", "dgOak", "dgFir", "dgSpruce", "nBeech",
                                          "nOak", "nFir", "nSpruce")], by = 'WKTid', all = TRUE)

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

plot(density(ratGFirSpruce$gSp1 / (ratGFirSpruce$gSp1 + ratGFirSpruce$gSp2)), ylim = c(0, 2.6))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gFir'] / forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gPred']), col = 'green')

plot(density(ratGFirSpruce$gSp2 / (ratGFirSpruce$gSp1 + ratGFirSpruce$gSp2)), ylim = c(0, 2.6))
lines(density(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gSpruce'] / forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce', 'gPred']), col = 'green')

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
# test ratio range predicted vs observed
###############################################################

# beech - fir
# hist
hist(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' , 'aNoise'], breaks = 100)
hist(ratDgBeechFir$dgSp1 / ratDgBeechFir$dgSp2, add = TRUE, col ='red')
# range
range(forestPlotsDf[forestPlotsDf$compoSp == 'beech-fir' , 'aNoise'])
range(ratDgBeechFir$dgSp1 / ratDgBeechFir$dgSp2)

# beech - spruce
# hist
hist(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' , 'aNoise'], breaks = 100)
hist(ratDgBeechSpruce$dgSp1 / ratDgBeechSpruce$dgSp2, add = TRUE, col ='red')
# range
range(forestPlotsDf[forestPlotsDf$compoSp == 'beech-spruce' , 'aNoise'])
range(ratDgBeechSpruce$dgSp1 / ratDgBeechSpruce$dgSp2)

# fir - spruce
# hist
hist(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' , 'aNoise'], breaks = 100)
hist(ratDgFirSpruce$dgSp1 / ratDgFirSpruce$dgSp2, add = TRUE, col ='red')
# range
range(forestPlotsDf[forestPlotsDf$compoSp == 'fir-spruce' , 'aNoise'])
range(ratDgFirSpruce$dgSp1 / ratDgFirSpruce$dgSp2)

###############################################################
# compare Dg predicted to ifn data and LIDAR
###############################################################

# comparer dg ifn / protest /lidar et simulé
ifn <- read.csv("./data/forRaph.csv", sep = " ")       # --> ifn data (dg > 7.5)
load(file="./data/placette.protest.rda")               # --> protest data (dg > 7.5)
dgPred <- raster('X:/ProjetsCommuns/PROTEST/T1/Observatoire/Analyse/rastDg75error.clean.tif') # LIDAR data (dg > 7.5)

plot(density(dgPred), col = 'orange', ylim = c(0,0.08))
lines(density(ifn$DgTotFinal))
lines(density(forestPlots$dgPred * 100), col = 'green') # LIDAR after having create forestPlots +- 3 ha
lines(density(placette.mes$Dg75), col = 'blue3')
