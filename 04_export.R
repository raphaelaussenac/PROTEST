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
                                                                'pot62Epsilon', 'INSEE_D', 'owner', 'access', 'nonHarv', 'dist')],
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
# define exploitability
###############################################################

forestPlots$access <- as.character(forestPlots$access)
# non-expoitable sites
forestPlots[!(forestPlots$access %in% c("dist 1 harv 1", "dist 2 harv 1")), "access"] <- 0
# exploitable sites
forestPlots[forestPlots$access %in% c("dist 1 harv 1", "dist 2 harv 1"), "access"] <- 1
forestPlots$access <- as.factor(forestPlots$access)

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
colnames(forestPlots)[colnames(forestPlots) == "access"] <- 'EXPLOITABILITY'

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
                              'INVENTORY_DATE',	'DEPARTMENT',	'CITY',	'COMMENT',	'WKT-GEOM', 'nonHarv', 'dist')]

###############################################################
# manage units
###############################################################

# convert dg m -> cm
forestPlots$DG_1 <- forestPlots$DG_1 * 100
forestPlots[forestPlots$DG_2 != -1, 'DG_2'] <- forestPlots[forestPlots$DG_2 != -1, 'DG_2']  * 100

# reduce file size
# forestPlots <- forestPlots[forestPlots$EXPLOITABILITY == 1,]
# forestPlots <- forestPlots[1:1000,]

###############################################################
# list of plots and total area of each forest type
###############################################################

# fir + spruce + fir-spruce
firSpruceList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_fir" |
                             forestPlots$FOREST_TYPE_CODE == "salem_spruce" |
                             forestPlots$FOREST_TYPE_CODE == "salem_fir_spruce", "STAND_ID"]
firSprucePubList <- forestPlots[forestPlots$STAND_ID %in% firSpruceList & forestPlots$DOMAINE_TYPE == "Pub", "STAND_ID"]
firSprucePrivList <- forestPlots[forestPlots$STAND_ID %in% firSpruceList & forestPlots$DOMAINE_TYPE == "Priv", "STAND_ID"]

# beech
beechList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_beech", "STAND_ID"]

# oak
oakList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_oak", "STAND_ID"]

# beach-fir + beech-spruce Public
beechFirSpruceList <- forestPlots[forestPlots$FOREST_TYPE_CODE == "salem_beech_fir" |
                             forestPlots$FOREST_TYPE_CODE == "salem_beech_spruce", "STAND_ID"]

beechFirSprucePubList <- forestPlots[forestPlots$STAND_ID %in% beechFirSpruceList & forestPlots$DOMAINE_TYPE == "Pub", "STAND_ID"]
beechFirSprucePrivList <- forestPlots[forestPlots$STAND_ID %in% beechFirSpruceList & forestPlots$DOMAINE_TYPE == "Priv", "STAND_ID"]

###############################################################
# management scenario
###############################################################

source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/sc1_BAU.R')

# TODO: arriver a ce stade -> executer tous les scenarios de gestion avec le même forestPLot
# (car attention -> processus rdm en amont)

forestPlots$nonHarv <- NULL
forestPlots$dist <- NULL

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

cat('# 1. Global Level\n', file="./output/forestPlots.txt")
cat("DATE=2016\n", file="./output/forestPlots.txt", append=TRUE)
cat('TOTAL_AREA=', sum(forestPlots$AREA), file="./output/forestPlots.txt", append=TRUE)
cat('\nXMIN=', xmin, sep = '', file="./output/forestPlots.txt", append=TRUE)
cat('\nYMIN=', ymin, sep = '', file="./output/forestPlots.txt", append=TRUE)
cat('\nXMAX=', xmax, sep = '', file="./output/forestPlots.txt", append=TRUE)
cat('\nYMAX=', ymax, sep = '', file="./output/forestPlots.txt", append=TRUE)
cat('\n# 2. Forest Unit Level', file="./output/forestPlots.txt", append=TRUE)
cat('\n#', file="./output/forestPlots.txt", append=TRUE)
write.table(forestPlots, file="./output/forestPlots.txt", row.names = FALSE, append=TRUE, quote = FALSE, sep = '\t')
