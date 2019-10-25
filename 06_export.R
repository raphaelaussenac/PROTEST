# clean up environment
rm(list = ls())

# load packages
library(plyr)
library(reshape2)

###############################################################
# import
###############################################################

# Site index
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/03_siteIndex.R')

forestPlots
dim(forestPlots)
sum(area(forestPlots))
length(forestPlots@polygons)
length(unique(forestPlots$id))

forestPlotsSiteIndex <- forestPlots
rm(list=setdiff(ls(), "forestPlotsSiteIndex"))

# compo g Dg and N
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/05_gDgN.R')

forestPlots
dim(forestPlots)
sum(area(forestPlots))
length(forestPlots@polygons)
length(unique(forestPlots$id))

forestPlotsCompogDgN <- forestPlots
rm(list=setdiff(ls(), c("forestPlotsCompogDgN", "forestPlotsSiteIndex")))

# missing plots? ###############################################################
# check this --> might be those plots belonging to "unwanted" TFV types (FO0, FF0)

disapearedPlots <- forestPlotsSiteIndex$id[!forestPlotsSiteIndex$id %in% forestPlotsCompogDgN$id]
forestPlotsSiteIndex@data[forestPlotsSiteIndex@data$id %in% disapearedPlots,]

table(forestPlotsSiteIndex$CODE_TF)
table(forestPlotsCompogDgN$CODE_TF)

###############################################################
# merge
###############################################################

forestPlots <- merge(forestPlotsSiteIndex[, c('id', 'pot03', 'pot03Epsilon',
                                                                'pot09', 'pot09Epsilon', 'pot61',
                                                                'pot61Epsilon', 'pot62',
                                                                'pot62Epsilon', 'INSEE_D', 'owner')],
                     forestPlotsCompogDgN[, c('id', 'compoSp', 'area', "gBeech", "gOak", "gFir", "gSpruce",
                                                                "dgBeech", "dgOak", "dgFir", "dgSpruce", "nBeech",
                                                                "nOak", "nFir", "nSpruce", "WKT")],
                      by = 'id')

# retrieve spatial extent
xmin <- extent(forestPlots)@xmin
ymin <- extent(forestPlots)@ymin
xmax <- extent(forestPlots)@xmax
ymax <- extent(forestPlots)@ymax

# remove 'disapearedPlots'
forestPlots <- forestPlots[!forestPlots$id %in% disapearedPlots, ]
forestPlots <- forestPlots@data

# import non-truncated WKT
wkt <- read.csv("superID_1.csv", header = TRUE, sep = "\t")
wkt$id <- c(1:nrow(wkt))
# replace wkt in forestPlots
forestPlots$WKT <- NULL
forestPlots <- merge(forestPlots, wkt[, c('id', 'WKT')], by = 'id')

###############################################################
# format
###############################################################

# id
colnames(forestPlots)[colnames(forestPlots) == "id"] <- 'STAND_ID'

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
forestPlots$EXPLOITABILITY <- 1


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

# remove fertility index < 0
forestPlots <- forestPlots[forestPlots$SITE_INDEX_1 > 0, ]
forestPlots <- forestPlots[forestPlots$SITE_INDEX_2 > 0 | forestPlots$SITE_INDEX_2 == -1, ]

# convert dg m -> cm
forestPlots$DG_1 <- forestPlots$DG_1 * 100
forestPlots[forestPlots$DG_2 != -1, 'DG_2'] <- forestPlots[forestPlots$DG_2 != -1, 'DG_2']  * 100

# reduce file size
# forestPlots <- forestPlots[20000:nrow(forestPlots),]

###############################################################
# format
###############################################################

cat('# 1. Global Level\n', file="forestPlots.txt")
cat("DATE=2016\n", file="forestPlots.txt", append=TRUE)
cat('TOTAL_AREA=', sum(forestPlots$AREA)/10000, file="forestPlots.txt", append=TRUE)
cat('\nXMIN=', xmin, sep = '', file="forestPlots.txt", append=TRUE)
cat('\nYMIN=', ymin, sep = '', file="forestPlots.txt", append=TRUE)
cat('\nXMAX=', xmax, sep = '', file="forestPlots.txt", append=TRUE)
cat('\nYMAX=', ymax, sep = '', file="forestPlots.txt", append=TRUE)
cat('\n# 2. Forest Unit Level', file="forestPlots.txt", append=TRUE)
cat('\n#', file="forestPlots.txt", append=TRUE)
write.table(forestPlots, file="forestPlots.txt", row.names = FALSE, append=TRUE, quote = FALSE, sep = '\t')






--> indice de fertilité négatifs !! due au predict + rnorm --> changer modele?

#
# verif merge forestPlots wkt by id
# a <- character()
# for (i in 1:nrow(wkt)){
#   for (j in c("CODE_TFV", "ESSENCE", "IDD", "ID_2", "ID_3", "ID_4", "INSEE_DEP", "INSEE_REG", "NOM_DEP", "SUPERID", "TFV", "TFV_G11")){
#     a <- c(a, wkt[wkt$id == i, c('IDD')] == forestPlots@data[forestPlots$id == i, c('IDD')])
#   }
# }

# sum(a == TRUE)
# sum(a == FALSE)
#
# nrow(wkt) * length(c("CODE_TFV", "ESSENCE", "IDD", "ID_2", "ID_3", "ID_4", "INSEE_DEP", "INSEE_REG", "NOM_DEP", "SUPERID", "TFV", "TFV_G11"))
