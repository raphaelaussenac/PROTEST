# clean up environment
rm(list = ls())

# load packages
library(plyr)
library(reshape2)

###############################################################
# import
###############################################################

# Site index
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/siteIndex.R')

forestPlots
dim(forestPlots)
sum(area(forestPlots)) / 10000
length(forestPlots@polygons)
length(unique(forestPlots$WKTid))

forestPlotsSiteIndex <- forestPlots
rm(list=setdiff(ls(), c('forestPlotsSiteIndex', 'mod03', 'mod09', 'mod61', 'mod62', 'predFert')))

# compo g Dg and N
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/gDgN.R')

forestPlots
dim(forestPlots)
sum(area(forestPlots)) / 10000
length(forestPlots@polygons)
length(unique(forestPlots$WKTid))

forestPlotsCompogDgN <- forestPlots
rm(list=setdiff(ls(), c('forestPlotsCompogDgN', 'forestPlotsSiteIndex', 'mod03', 'mod09', 'mod61', 'mod62', 'predFert')))

# missing plots? ###############################################################
# check this --> might be those plots belonging to "unwanted" TFV types (FO0, FF0)

disapearedPlots <- forestPlotsSiteIndex$WKTid[!forestPlotsSiteIndex$WKTid %in% forestPlotsCompogDgN$WKTid]
forestPlotsSiteIndex@data[forestPlotsSiteIndex@data$WKTid %in% disapearedPlots,]

table(forestPlotsSiteIndex$CODE_TF)
table(forestPlotsCompogDgN$CODE_TF)

###############################################################
# merge
###############################################################

forestPlots <- merge(forestPlotsSiteIndex[, c('WKTid', 'pot03', 'pot03Epsilon',
                                                                'pot09', 'pot09Epsilon', 'pot61',
                                                                'pot61Epsilon', 'pot62',
                                                                'pot62Epsilon', 'INSEE_D', 'owner', 'nonHarv')],
                     forestPlotsCompogDgN[, c('WKTid', 'compoSp', 'area', "gBeech", "gOak", "gFir", "gSpruce",
                                                                "dgBeech", "dgOak", "dgFir", "dgSpruce", "nBeech",
                                                                "nOak", "nFir", "nSpruce")],
                      by = 'WKTid')

# retrieve spatial extent
xmin <- extent(forestPlots)@xmin
ymin <- extent(forestPlots)@ymin
xmax <- extent(forestPlots)@xmax
ymax <- extent(forestPlots)@ymax

# remove 'disapearedPlots'
forestPlots <- forestPlots[!forestPlots$WKTid %in% disapearedPlots, ]
forestPlots <- forestPlots@data

# import non-truncated WKT
wkt <- read.csv("BDid_1.csv", header = TRUE, sep = "\t")
wkt$WKTid <- c(1:nrow(wkt))
# replace wkt in forestPlots
forestPlots <- merge(forestPlots, wkt[, c('WKTid', 'WKT')], by = 'WKTid')

###############################################################
# format
###############################################################

# id
colnames(forestPlots)[colnames(forestPlots) == "WKTid"] <- 'STAND_ID'

# transform diversity
forestPlots[forestPlots$compoSp == 'beech', 'compoSp'] <- 'salem_beech'
forestPlots[forestPlots$compoSp == 'oak', 'compoSp'] <- 'salem_oak'
forestPlots[forestPlots$compoSp == 'fir', 'compoSp'] <- 'salem_fir'
forestPlots[forestPlots$compoSp == 'spruce', 'compoSp'] <- 'salem_spruce'
forestPlots[forestPlots$compoSp == 'beech-spruce', 'compoSp'] <- 'salem_beech_spruce'
forestPlots[forestPlots$compoSp == 'beech-fir', 'compoSp'] <- 'salem_beech_fir'
forestPlots[forestPlots$compoSp == 'fir-spruce', 'compoSp'] <- 'salem_fir_spruce'
colnames(forestPlots)[colnames(forestPlots) == "compoSp"] <- 'FOREST_TYPE_CODE'

# FOREST_TYPE_NAME
forestPlots$FOREST_TYPE_NAME <- 'RAS'

# area
colnames(forestPlots)[colnames(forestPlots) == "area"] <- 'AREA'

# ...
forestPlots$AGE_1 <- -1
forestPlots$HDOM_1 <- -1
forestPlots$DDOM_1 <- -1
forestPlots$HG_1 <- -1
forestPlots$AGE_2 <- -1
forestPlots$HDOM_2 <- -1
forestPlots$DDOM_2 <- -1
forestPlots$HG_2 <- -1
forestPlots$COMMENT <- -1

# wkt_geom
colnames(forestPlots)[colnames(forestPlots) == "WKT"] <- 'WKT-GEOM'

# departement
colnames(forestPlots)[colnames(forestPlots) == "INSEE_D"] <- 'DEPARTMENT'

# city
forestPlots$CITY <- 'city'

# forest
forestPlots$FOREST <- 'BAUGES'

# inventory
forestPlots$INVENTORY_DATE	 <- 2016

# DOMAINE_TYPE
colnames(forestPlots)[colnames(forestPlots) == "owner"] <- 'DOMAINE_TYPE'

# EXPLOITABILITY
colnames(forestPlots)[colnames(forestPlots) == "nonHarv"] <- 'EXPLOITABILITY'

######################################

# site index, n, dg
forestPlots$SITE_INDEX_1 <- -1
forestPlots$NHA_1 <- -1
forestPlots$DG_1 <- -1
forestPlots$SITE_INDEX_2 <- -1
forestPlots$NHA_2 <- -1
forestPlots$DG_2 <- -1

# sp1 = beech
forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "SITE_INDEX_1"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "pot09Epsilon"]
forestPlots[forestPlots$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "NHA_1"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "nBeech"] /
                                                  (forestPlots[forestPlots$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "AREA"] / 10000)
forestPlots[forestPlots$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "DG_1"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "dgBeech"]

# sp1 = oak
forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "SITE_INDEX_1"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "pot03Epsilon"]
forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "NHA_1"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "nOak"] /
                                                                     (forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "AREA"] / 10000)
forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "DG_1"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "dgOak"]

# sp1 = fir
forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "SITE_INDEX_1"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "pot61Epsilon"]
forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "NHA_1"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "nFir"] /
                                                  (forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "AREA"] / 10000)
forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "DG_1"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "dgFir"]

# sp1 = spruce
forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_spruce", "SITE_INDEX_1"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_spruce", "pot62Epsilon"]
forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_spruce", "NHA_1"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_spruce", "nSpruce"] /
                                                                        (forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_spruce", "AREA"] / 10000)
forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_spruce", "DG_1"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_spruce", "dgSpruce"]

# sp2 = fir
forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', "SITE_INDEX_2"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', "pot61Epsilon"]
forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', "NHA_2"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', "nFir"] /
                                                                           (forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', "AREA"] / 10000)
forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', "DG_2"] <- forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', "dgFir"]

# sp2 = spruce
forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "SITE_INDEX_2"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "pot62Epsilon"]
forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "NHA_2"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "nSpruce"] /
                                                  (forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "AREA"] / 10000)
forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "DG_2"] <-
                                                  forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "dgSpruce"]

# final table
forestPlots <- forestPlots[, c('STAND_ID',	'FOREST_TYPE_CODE',	'FOREST_TYPE_NAME',	'AREA',	'SITE_INDEX_1', 'NHA_1',
                              'AGE_1',	'HDOM_1',	'DDOM_1',	'HG_1',	'DG_1',	'SITE_INDEX_2', 'NHA_2',	'AGE_2',	'HDOM_2',
                              'DDOM_2',	'HG_2',	'DG_2',	'EXPLOITABILITY',	'DOMAINE_TYPE',	'FOREST',
                              'INVENTORY_DATE',	'DEPARTMENT',	'CITY',	'COMMENT',	'WKT-GEOM')]

###############################################################
# manage units
###############################################################

# convert dg m -> cm
forestPlots$DG_1 <- forestPlots$DG_1 * 100
forestPlots[forestPlots$DG_2 != -1, 'DG_2'] <- forestPlots[forestPlots$DG_2 != -1, 'DG_2']  * 100

# reduce file size
# forestPlots <- forestPlots[forestPlots$EXPLOITABILITY == 1,]
# forestPlots <- forestPlots[1:1000,]

# ###############################################################
# # surface area of each forest type
# ###############################################################
#
# AREA <- ddply(forestPlots, .(FOREST_TYPE_CODE, DOMAINE_TYPE), summarise, AREA = sum(AREA))
# AREAPub <- AREA[AREA$DOMAINE_TYPE == "Pub",]
# AREAPriv <-AREA[AREA$DOMAINE_TYPE == "Priv",]
#
# # beech
# beechPubPrivArea <- sum(AREA[AREA$FOREST_TYPE_CODE == "salem_beech", "AREA"])
#
# # oak
# oakPubPrivArea <- sum(AREA[AREA$FOREST_TYPE_CODE == "salem_oak", "AREA"])
#
# # fir + spruce + fir-spruce Public
# fsPubArea <- sum(AREAPub[AREAPub$FOREST_TYPE_CODE == "salem_fir" |
#                       AREAPub$FOREST_TYPE_CODE == "salem_spruce" |
#                       AREAPub$FOREST_TYPE_CODE == "salem_fir_spruce", "AREA"])
#
# # fir + spruce + fir-spruce Private
# fsPrivArea <- sum(AREAPriv[AREAPriv$FOREST_TYPE_CODE == "salem_fir" |
#                       AREAPriv$FOREST_TYPE_CODE == "salem_spruce" |
#                       AREAPriv$FOREST_TYPE_CODE == "salem_fir_spruce", "AREA"])
#
# # beech-spruce + beech-fir Public
# bfbsPubArea <- sum(AREAPub[AREAPub$FOREST_TYPE_CODE == "salem_beech_fir" |
#                       AREAPub$FOREST_TYPE_CODE == "salem_beech_spruce", "AREA"])
#
# # beech-spruce + beech-fir Private
# bfbsPrivArea <- sum(AREAPriv[AREAPriv$FOREST_TYPE_CODE == "salem_beech_fir" |
#                       AREAPriv$FOREST_TYPE_CODE == "salem_beech_spruce", "AREA"])
#
###############################################################
# list of plots and total area of each forest type
###############################################################

# fir + spruce + fir-spruce
firSpruceList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_fir" |
                             forestPlots$FOREST_TYPE_CODE == "salem_spruce" |
                             forestPlots$FOREST_TYPE_CODE == "salem_fir_spruce", "STAND_ID"]
firSprucePubList <- forestPlots[forestPlots$STAND_ID %in% firSpruceList & forestPlots$DOMAINE_TYPE == "Pub", "STAND_ID"]
firSprucePrivList <- forestPlots[forestPlots$STAND_ID %in% firSpruceList & forestPlots$DOMAINE_TYPE == "Priv", "STAND_ID"]
fsPubArea <- sum(forestPlots[forestPlots$STAND_ID %in% firSprucePubList, 'AREA'])
fsPrivArea <- sum(forestPlots[forestPlots$STAND_ID %in% firSprucePrivList, 'AREA'])

# beech
beechList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_beech", "STAND_ID"]
beechPubPrivArea <- sum(forestPlots[forestPlots$STAND_ID %in% beechList, 'AREA'])

# oak
oakList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "STAND_ID"]
oakPubPrivArea <- sum(forestPlots[forestPlots$STAND_ID %in% oakList, 'AREA'])

# beach-fir + beech-spruce Public
beechFirSpruceList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_beech_fir" |
                             forestPlots$FOREST_TYPE_CODE == "salem_beech_spruce", "STAND_ID"]

beechFirSprucePubList <- forestPlots[forestPlots$STAND_ID %in% beechFirSpruceList & forestPlots$DOMAINE_TYPE == "Pub", "STAND_ID"]
beechFirSprucePrivList <- forestPlots[forestPlots$STAND_ID %in% beechFirSpruceList & forestPlots$DOMAINE_TYPE == "Priv", "STAND_ID"]
bfbsPubArea <- sum(forestPlots[forestPlots$STAND_ID %in% beechFirSprucePubList, 'AREA'])
bfbsPrivArea <- sum(forestPlots[forestPlots$STAND_ID %in% beechFirSprucePrivList, 'AREA'])

###############################################################
# plot random selection function
###############################################################

rdmSelect <- function(plotList, threshold){
  # randomly pick first plot
  plots <- plotList[round(runif(1, min = 1, max = length(plotList)))]
  # add new plots till the threshold is reached
  while(sum(forestPlots[forestPlots$STAND_ID %in% plots, 'AREA']) < threshold){
    # list of remaining plots
    remaining <- plotList[!(plotList %in% plots)]
    # randomly select 1 more plot
    addplot <- remaining[round(runif(1, min = 1, max = length(remaining)))]
    plots <- c(plots, addplot)
  }
  return(plots)
}

###############################################################
# assign management to each plot
###############################################################

# Business as usual ------------------------------------------------------------
# fir + spruce + fir-spruce Public ---------------------------------------------
# conservation
conservationThresh <- fsPubArea * 0.02
plotCons <- rdmSelect(plotList = firSprucePubList, threshold = conservationThresh)
forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- 'fsPubCons'
# thinning and harvest
thinHarvThresh <- fsPubArea * 0.6
plotThinHarv <- rdmSelect(plotList = firSprucePubList[!(firSprucePubList %in% plotCons)], threshold = thinHarvThresh)
forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- 'fsPubThinHarv'
# irregular
plotIrr <- firSprucePubList[!(firSprucePubList %in% c(plotCons, plotThinHarv))]
forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- 'fsPubIrr'

# fir + spruce + fir-spruce Private --------------------------------------------
# conservation
conservationThresh <- fsPrivArea * 0.27
plotCons <- rdmSelect(plotList = firSprucePrivList, threshold = conservationThresh)
forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- 'fsPrivCons'
# final Harvest
HarvThresh <- fsPrivArea * 0.15
plotHarv <- rdmSelect(plotList = firSprucePrivList[!(firSprucePrivList %in% plotCons)], threshold = HarvThresh)
forestPlots[forestPlots$STAND_ID %in% plotHarv, "COMMENT"] <- 'fsPrivHarv'
# thinning and harvest
thinHarvThresh <- fsPrivArea * 0.39
plotThinHarv <- rdmSelect(plotList = firSprucePrivList[!(firSprucePrivList %in% c(plotCons, plotHarv))], threshold = thinHarvThresh)
forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- 'fsPrivThinHarv'
# irregular
plotIrr <- firSprucePrivList[!(firSprucePrivList %in% c(plotCons, plotHarv, plotThinHarv))]
forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- 'fsPrivIrr'

# beech Public + Private -------------------------------------------------------
# conservation
conservationThresh <- beechPubPrivArea * 0.24
plotCons <- rdmSelect(plotList = beechList, threshold = conservationThresh)
forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- 'bCons'
# final Harvest
HarvThresh <- beechPubPrivArea * 0.22
plotHarv <- rdmSelect(plotList = beechList[!(beechList %in% plotCons)], threshold = HarvThresh)
forestPlots[forestPlots$STAND_ID %in% plotHarv, "COMMENT"] <- 'bHarv'
# thinning and harvest
thinHarvThresh <- beechPubPrivArea * 0.21
plotThinHarv <- rdmSelect(plotList = beechList[!(beechList %in% c(plotCons, plotHarv))], threshold = thinHarvThresh)
forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- 'bThinHarv'
# irregular
plotIrr <- beechList[!(beechList %in% c(plotCons, plotHarv, plotThinHarv))]
forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- 'bIrr'

# oak Public + Private ---------------------------------------------------------
# conservation
conservationThresh <- oakPubPrivArea * 0.57
plotCons <- rdmSelect(plotList = oakList, threshold = conservationThresh)
forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- 'oCons'
# final Harvest
plotIrr <- oakList[!(oakList %in% c(plotCons))]
forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- 'oIrr'

# beech-spruce + beech-fir Public ----------------------------------------------
# thinning and harvest
thinHarvThresh <- bfbsPubArea * 0.34
plotThinHarv <- rdmSelect(plotList = beechFirSprucePubList, threshold = thinHarvThresh)
forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- 'bfsPubThinHarv'
# irregular
plotIrr <- beechFirSprucePubList[!(beechFirSprucePubList %in% plotThinHarv)]
forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- 'bfsPubIrr'

# beech-spruce + beech-fir Private ---------------------------------------------
# conservation
conservationThresh <- bfbsPrivArea * 0.29
plotCons <- rdmSelect(plotList = beechFirSprucePrivList, threshold = conservationThresh)
forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- 'bfsPrivCons'
# thinning and harvest
thinHarvThresh <- bfbsPrivArea * 0.14
plotThinHarv <- rdmSelect(plotList = beechFirSprucePrivList[!(beechFirSprucePrivList %in% plotCons)], threshold = thinHarvThresh)
forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- 'bfsPrivThinHarv'
# irregular
plotIrr <- beechFirSprucePrivList[!(beechFirSprucePrivList %in% c(plotCons, plotThinHarv))]
forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- 'bfsPrivIrr'






TODO: - faire une fonction assign avec les 4 types de gestion?
      - sortir itk dans un autre script (avec fonctions rdm et assign?)

# test
sum(forestPlots[forestPlots$STAND_ID %in% plotIrr, 'AREA']) / 10000
sum(forestPlots[forestPlots$COMMENT == "bfsPrivIrr", 'AREA']) / 10000
unique(forestPlots[forestPlots$STAND_ID %in% beechFirSprucePrivList, "COMMENT"])
length(plotCons) + length(plotHarv) + length(plotThinHarv) + length(plotIrr) - length(beechFirSprucePrivList)
unique(forestPlots$COMMENT)

###############################################################
# verification
###############################################################

# Site Index
hist(forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech', 'salem_beech-spruce', 'salem_beech-fir') , 'SITE_INDEX_1'], breaks = 100, border = 'green', ylim = c(0, 350), xlim = c(-10, 130), main = '', xlab = 'site index')
hist(c(forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_fir', 'salem_fir-spruce') , 'SITE_INDEX_1'], forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', 'SITE_INDEX_2']), breaks = 100, add = TRUE, border = 'blue')
hist(c(forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_spruce' , 'SITE_INDEX_1'], forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_fir-spruce', 'salem_beech-spruce') , 'SITE_INDEX_2']), breaks = 100, add = TRUE, border = 'orange')
hist(forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak" , 'SITE_INDEX_1'], breaks = 100, col = 'black', add = TRUE)

# Dg
hist(forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_beech', 'salem_beech-spruce', 'salem_beech-fir') , 'DG_1'], breaks = 200, border = 'green', ylim = c(0, 200), xlim = c(5, 90), main = '', xlab = 'Dg')
hist(c(forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_fir', 'salem_fir-spruce') , 'DG_1'], forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_beech_fir', 'DG_2']), breaks = 200, add = TRUE, border = 'blue')
hist(c(forestPlots[forestPlots$FOREST_TYPE_CODE == 'salem_spruce' , 'DG_1'], forestPlots[forestPlots$FOREST_TYPE_CODE %in% c('salem_fir-spruce', 'salem_beech-spruce') , 'DG_2']), breaks = 200, add = TRUE, border = 'orange')
hist(forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak" , 'DG_1'], breaks = 200, col = 'black', add = TRUE)

# n / ha
# pure plots
hist(forestPlots[forestPlots$NHA_2 == -1, 'NHA_1'] , breaks = 100, ylim = c(0,600), col = 'black')
# mixed plots
hist(forestPlots[forestPlots$NHA_2 != -1, 'NHA_1'] + forestPlots[forestPlots$NHA_2 != -1, 'NHA_2'], breaks = 100, border = 'blue3', add = TRUE)

###############################################################
# format
###############################################################

cat('# 1. Global Level\n', file="forestPlots.txt")
cat("DATE=2016\n", file="forestPlots.txt", append=TRUE)
cat('TOTAL_AREA=', sum(forestPlots$AREA), file="forestPlots.txt", append=TRUE)
cat('\nXMIN=', xmin, sep = '', file="forestPlots.txt", append=TRUE)
cat('\nYMIN=', ymin, sep = '', file="forestPlots.txt", append=TRUE)
cat('\nXMAX=', xmax, sep = '', file="forestPlots.txt", append=TRUE)
cat('\nYMAX=', ymax, sep = '', file="forestPlots.txt", append=TRUE)
cat('\n# 2. Forest Unit Level', file="forestPlots.txt", append=TRUE)
cat('\n#', file="forestPlots.txt", append=TRUE)
write.table(forestPlots, file="forestPlots.txt", row.names = FALSE, append=TRUE, quote = FALSE, sep = '\t')
