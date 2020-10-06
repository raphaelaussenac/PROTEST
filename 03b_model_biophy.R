# clean up environment
rm(list = setdiff(ls(), "user"))

# load packages
library(plyr)
library(reshape2)

###############################################################
# import
###############################################################

# Site index
# requires access to rastDg file
source(paste0(user$WorkingDir, "/src/siteIndex.R"))

names(forestStands)
# dim(forestStands)
sum(area(forestStands)) / 10000
# length(forestStands@polygons)
length(unique(forestStands$WKTid))

forestStandsSiteIndex <- forestStands
rm(list=setdiff(ls(), c('forestStandsSiteIndex', 'mod03', 'mod09', 'mod61', 'mod62', 'predFert', 'user')))

# compo g Dg and N
source(paste0(user$WorkingDir,"/src/gDgN.R"))

forestStands
dim(forestStands)
sum(area(forestStands)) / 10000
length(forestStands@polygons)
length(unique(forestStands$WKTid))

forestStandsCompogDgN <- forestStands
rm(list=setdiff(ls(), c('forestStandsCompogDgN', 'forestStandsSiteIndex', 'mod03', 'mod09', 'mod61', 'mod62', 'predFert')))

# check if polygons were removed during previous step -> no
disapearedPlots <- forestStandsSiteIndex$WKTid[!forestStandsSiteIndex$WKTid %in% forestStandsCompogDgN$WKTid]
forestStandsSiteIndex@data[forestStandsSiteIndex@data$WKTid %in% disapearedPlots,]
table(forestStandsSiteIndex$CODE_TF)
table(forestStandsCompogDgN$CODE_TF)

load(file="data/forestStands03a.rda")

# missing stands : 5 polygons, 8.6 ha
disapearedPlots <- forestStands$WKTid[!is.element(forestStands$WKTid, forestStandsCompogDgN$WKTid)]
disapearedPlots <- unique(disapearedPlots, forestStands$WKTid[!is.element(forestStands$WKTid, forestStandsSiteIndex$WKTid)])

###############################################################
# merge
###############################################################

forestStands <- merge(forestStandsSiteIndex[, c('WKTid', 'pot03', 'pot03Epsilon',
                                                'pot09', 'pot09Epsilon', 'pot61',
                                                'pot61Epsilon', 'pot62',
                                                'pot62Epsilon', 'INSEE_D')],
                      forestStandsCompogDgN[, c('WKTid', 'compoSp', 'area', "gBeech", "gOak", "gFir", "gSpruce",
                                                "dgBeech", "dgOak", "dgFir", "dgSpruce", "nBeech",
                                                "nOak", "nFir", "nSpruce")],
                      by = 'WKTid')

# remove 'disapearedPlots'
forestStands <- forestStands[!forestStands$WKTid %in% disapearedPlots, ]



# add geometry attribute and keep only data.frame
sf.forestStands <- sf::st_as_sf(forestStands)
forestStands$WKT <- sf::st_as_text(sf.forestStands$geometry)
forestStands <- forestStands@data

###############################################################
# format
###############################################################

# id
colnames(forestStands)[colnames(forestStands) == "WKTid"] <- 'STAND_ID'

# transform diversity
forestStands[forestStands$compoSp == 'beech', 'compoSp'] <- 'salem_beech'
forestStands[forestStands$compoSp == 'oak', 'compoSp'] <- 'salem_oak'
forestStands[forestStands$compoSp == 'fir', 'compoSp'] <- 'salem_fir'
forestStands[forestStands$compoSp == 'spruce', 'compoSp'] <- 'salem_spruce'
forestStands[forestStands$compoSp == 'beech-spruce', 'compoSp'] <- 'salem_beech_spruce'
forestStands[forestStands$compoSp == 'beech-fir', 'compoSp'] <- 'salem_beech_fir'
forestStands[forestStands$compoSp == 'fir-spruce', 'compoSp'] <- 'salem_fir_spruce'
colnames(forestStands)[colnames(forestStands) == "compoSp"] <- 'FOREST_TYPE_CODE'

# FOREST_TYPE_NAME
forestStands$FOREST_TYPE_NAME <- 'RAS'

# area
colnames(forestStands)[colnames(forestStands) == "area"] <- 'AREA'

# ...
forestStands$AGE_1 <- -1
forestStands$HDOM_1 <- -1
forestStands$DDOM_1 <- -1
forestStands$HG_1 <- -1
forestStands$AGE_2 <- -1
forestStands$HDOM_2 <- -1
forestStands$DDOM_2 <- -1
forestStands$HG_2 <- -1
forestStands$COMMENT <- -1

# wkt_geom
colnames(forestStands)[colnames(forestStands) == "WKT"] <- 'WKT-GEOM'

# departement
colnames(forestStands)[colnames(forestStands) == "INSEE_D"] <- 'DEPARTMENT'

# city
forestStands$CITY <- 'city'

# forest
forestStands$FOREST <- 'BAUGES'

# inventory
forestStands$INVENTORY_DATE	 <- 2016

######################################

# site index, n, dg
forestStands$SITE_INDEX_1 <- -1
forestStands$NHA_1 <- -1
forestStands$DG_1 <- -1
forestStands$SITE_INDEX_2 <- -1
forestStands$NHA_2 <- -1
forestStands$DG_2 <- -1

# sp1 = beech
forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "SITE_INDEX_1"] <-
  forestStands[forestStands$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "pot09Epsilon"]
forestStands[forestStands$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "NHA_1"] <-
  forestStands[forestStands$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "nBeech"] /
  (forestStands[forestStands$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "AREA"] / 10000)
forestStands[forestStands$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "DG_1"] <-
  forestStands[forestStands$FOREST_TYPE_CODE  %in% c("salem_beech", "salem_beech_fir", "salem_beech_spruce"), "dgBeech"]

# sp1 = oak
forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "SITE_INDEX_1"] <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "pot03Epsilon"]
forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "NHA_1"] <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "nOak"] /
  (forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "AREA"] / 10000)
forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "DG_1"] <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "dgOak"]

# sp1 = fir
forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "SITE_INDEX_1"] <-
  forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "pot61Epsilon"]
forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "NHA_1"] <-
  forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "nFir"] /
  (forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "AREA"] / 10000)
forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "DG_1"] <-
  forestStands[forestStands$FOREST_TYPE_CODE %in% c("salem_fir", "salem_fir_spruce"), "dgFir"]

# sp1 = spruce
forestStands[forestStands$FOREST_TYPE_CODE == "salem_spruce", "SITE_INDEX_1"] <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_spruce", "pot62Epsilon"]
forestStands[forestStands$FOREST_TYPE_CODE == "salem_spruce", "NHA_1"] <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_spruce", "nSpruce"] /
  (forestStands[forestStands$FOREST_TYPE_CODE == "salem_spruce", "AREA"] / 10000)
forestStands[forestStands$FOREST_TYPE_CODE == "salem_spruce", "DG_1"] <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_spruce", "dgSpruce"]

# sp2 = fir
forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', "SITE_INDEX_2"] <- forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', "pot61Epsilon"]
forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', "NHA_2"] <- forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', "nFir"] /
  (forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', "AREA"] / 10000)
forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', "DG_2"] <- forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', "dgFir"]

# sp2 = spruce
forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "SITE_INDEX_2"] <-
  forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "pot62Epsilon"]
forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "NHA_2"] <-
  forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "nSpruce"] /
  (forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "AREA"] / 10000)
forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "DG_2"] <-
  forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), "dgSpruce"]

############################################
# final table
forestStands <- forestStands[, c('STAND_ID',	'FOREST_TYPE_CODE',	'FOREST_TYPE_NAME',	'AREA',	'SITE_INDEX_1', 'NHA_1',
                                 'AGE_1',	'HDOM_1',	'DDOM_1',	'HG_1',	'DG_1',	'SITE_INDEX_2', 'NHA_2',	'AGE_2',	'HDOM_2',
                                 'DDOM_2',	'HG_2',	'DG_2',	'FOREST', 'INVENTORY_DATE',	'DEPARTMENT',	'CITY',	'COMMENT',	'WKT-GEOM')]

###############################################################
# manage units
###############################################################

# convert dg m -> cm
forestStands$DG_1 <- forestStands$DG_1 * 100
forestStands[forestStands$DG_2 != -1, 'DG_2'] <- forestStands[forestStands$DG_2 != -1, 'DG_2']  * 100

###############################################################
# calculate stand total basal area
forestStands$Gsp1 <- (forestStands$NHA_1 * pi * ((forestStands$DG_1/100)^2)) / 4
forestStands$Gsp2 <- 0
forestStands[forestStands$NHA_2 > 0, "Gsp2"] <- (forestStands[forestStands$NHA_2 > 0, "NHA_2"] * pi * ((forestStands[forestStands$NHA_2 > 0, "DG_2"]/100)^2)) / 4
forestStands$G <- forestStands$Gsp1 + forestStands$Gsp2
#
save(forestStands, file="./data/forestStands03b.rda")
