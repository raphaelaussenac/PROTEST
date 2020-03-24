# clean up environment
rm(list = setdiff(ls(), "user"))

# load packages
library(plyr)
library(reshape2)

###############################################################
# import
###############################################################

# Site index
source(paste0(user$WorkingDir, "/src/siteIndex.R"))

forestStands
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

# missing stands? ###############################################################
# check this --> might be those plots belonging to "unwanted" TFV types (FO0, FF0)

disapearedPlots <- forestStandsSiteIndex$WKTid[!forestStandsSiteIndex$WKTid %in% forestStandsCompogDgN$WKTid]
forestStandsSiteIndex@data[forestStandsSiteIndex@data$WKTid %in% disapearedPlots,]

table(forestStandsSiteIndex$CODE_TF)
table(forestStandsCompogDgN$CODE_TF)

###############################################################
# merge
###############################################################

forestStands <- merge(forestStandsSiteIndex[, c('WKTid', 'pot03', 'pot03Epsilon',
                                                'pot09', 'pot09Epsilon', 'pot61',
                                                'pot61Epsilon', 'pot62',
                                                'pot62Epsilon', 'INSEE_D', 'owner', 'access', 'nonHarv', 'dist', 'mnPrclA')],
                      forestStandsCompogDgN[, c('WKTid', 'compoSp', 'area', "gBeech", "gOak", "gFir", "gSpruce",
                                                "dgBeech", "dgOak", "dgFir", "dgSpruce", "nBeech",
                                                "nOak", "nFir", "nSpruce")],
                      by = 'WKTid')

# retrieve spatial extent
xmin <- extent(forestStands)@xmin
ymin <- extent(forestStands)@ymin
xmax <- extent(forestStands)@xmax
ymax <- extent(forestStands)@ymax

# remove 'disapearedPlots'
forestStands <- forestStands[!forestStands$WKTid %in% disapearedPlots, ]
forestStands <- forestStands@data

# import non-truncated WKT
wkt <- read.csv("./data/BDid_1.csv", header = TRUE, sep = "\t")
wkt$WKTid <- c(1:nrow(wkt))
# replace wkt in forestStands
forestStands <- merge(forestStands, wkt[, c('WKTid', 'WKT')], by = 'WKTid')

save(list=ls(), file="intermediaryExport0.rda")
rm(list=ls())
load(file="intermediaryExport0.rda")

###############################################################
# define exploitability
###############################################################

# forestStands$access <- as.character(forestStands$access)
# # non-expoitable sites
# forestStands[!(forestStands$access %in% c("dist 1 harv 1")), "access"] <- 0 # , "dist 2 harv 1"
# # exploitable sites
# forestStands[forestStands$access %in% c("dist 1 harv 1"), "access"] <- 1
# forestStands$access <- as.factor(forestStands$access)

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

# DOMAINE_TYPE
colnames(forestStands)[colnames(forestStands) == "owner"] <- 'DOMAINE_TYPE'

# EXPLOITABILITY
colnames(forestStands)[colnames(forestStands) == "access"] <- 'EXPLOITABILITY'

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
                                 'DDOM_2',	'HG_2',	'DG_2',	'EXPLOITABILITY',	'DOMAINE_TYPE',	'FOREST',
                                 'INVENTORY_DATE',	'DEPARTMENT',	'CITY',	'COMMENT',	'WKT-GEOM', 'nonHarv', 'dist', 'mnPrclA')]

###############################################################
# manage units
###############################################################

# convert dg m -> cm
forestStands$DG_1 <- forestStands$DG_1 * 100
forestStands[forestStands$DG_2 != -1, 'DG_2'] <- forestStands[forestStands$DG_2 != -1, 'DG_2']  * 100

# reduce file size
# forestStands <- forestStands[forestStands$EXPLOITABILITY == 1,]
# forestStands <- forestStands[1:1000,]

###############################################################
# list of stands and total area of each forest type
###############################################################

# fir + spruce + fir-spruce
firSpruceList <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_fir" |
                                forestStands$FOREST_TYPE_CODE == "salem_spruce" |
                                forestStands$FOREST_TYPE_CODE == "salem_fir_spruce", "STAND_ID"]
firSprucePubList <- forestStands[forestStands$STAND_ID %in% firSpruceList & forestStands$DOMAINE_TYPE == "Pub", "STAND_ID"]
firSprucePrivList <- forestStands[forestStands$STAND_ID %in% firSpruceList & forestStands$DOMAINE_TYPE == "Priv", "STAND_ID"]

# beech
beechList <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_beech", "STAND_ID"]

# oak
oakList <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak", "STAND_ID"]

# beach-fir + beech-spruce Public
beechFirSpruceList <- forestStands[forestStands$FOREST_TYPE_CODE == "salem_beech_fir" |
                                     forestStands$FOREST_TYPE_CODE == "salem_beech_spruce", "STAND_ID"]

beechFirSprucePubList <- forestStands[forestStands$STAND_ID %in% beechFirSpruceList & forestStands$DOMAINE_TYPE == "Pub", "STAND_ID"]
beechFirSprucePrivList <- forestStands[forestStands$STAND_ID %in% beechFirSpruceList & forestStands$DOMAINE_TYPE == "Priv", "STAND_ID"]

###############################################################
# assign management type to each stand
###############################################################

# calculate stand total basal area
forestStands$Gsp1 <- (forestStands$NHA_1 * pi * ((forestStands$DG_1/100)^2)) / 4
forestStands$Gsp2 <- 0
forestStands[forestStands$NHA_2 > 0, "Gsp2"] <- (forestStands[forestStands$NHA_2 > 0, "NHA_2"] * pi * ((forestStands[forestStands$NHA_2 > 0, "DG_2"]/100)^2)) / 4
forestStands$G <- forestStands$Gsp1 + forestStands$Gsp2

# HERE STOP POINT
save(list=ls(), file="intermediaryExport.rda")
rm(list=ls())
load(file="intermediaryExport.rda")
summary(forestStands[,-26])
#
# probability of management: 1 by default
# reminder
# nonHarv: 0-non bucheronnable / 1-bucheronnable
# management proportions proposed by FCBA refer to the 1-bucheronnable part
forestStands$proba <- 1
#
# set management probability of private forest
#
# load glm binomial model calibrated on Mihai dataset
# should the forest type (TFV) be added ?
load(file="./data/modelGestion.rda")
model.glm
# convert mean parcel surface in hectares
forestStands$surface <- forestStands$mnPrclA/10000
# replace distances by before application of model (was calibrated with NA distances replaced by 2000)
forestStands$dist[is.na(forestStands$dist)] <- 2000
# apply model only to private forests
dummy <- which(forestStands$DOMAINE_TYPE=="Priv")
# estimate probability of management in private forests
forestStands$proba[dummy] <- predict.glm(model.glm, forestStands[dummy,], type="response")
# histogram of probability
hist(forestStands$proba[dummy])
#
# too many managed stands : apply to private and public forest a multiplicative factor depending on log of distance (double effect on private stands ?)

# set management probability of public forests

dummy <- which(forestStands$DOMAINE_TYPE=="Pub")
# as log of distance -> by raphael
# forestStands$proba[dummy] <- 1 - (log(forestStands$dist[dummy]) / log(max(forestStands$dist[dummy])))
# forestStands$proba[dummy] <- forestStands$proba[dummy] * (1 - (log(forestStands$dist[dummy]) / log(10000)))
#
# public stands : proba decreases linearly from 1 at 0m and 0.75 at 2000m
forestStands$proba[dummy] <- 1 - 0.25/2000 * forestStands$dist[dummy]
plot(forestStands$dist, forestStands$proba, col=forestStands$DOMAINE_TYPE)

# for all forests
# nonHarv forest have a probability of 0 (although the model of private forests does not includes this variable)
forestStands$proba[forestStands$nonHarv==0] <- 0
# forests with NA distance have a probability of 0 (although the model of private forests considers them as tough their distance was 2000)
# set back to NA values previously set to 2000
forestStands$dist[forestStands$dist>=2000] <- NA
# set dist to NA of polygons not harvestable
forestStands$dist[forestStands$nonHarv == 0]<- NA
# forest with a NA distance (not accessible) is set to zero
forestStands$proba[is.na(forestStands$dist)] <- 0
#
plot(forestStands$dist, forestStands$proba, col=forestStands$DOMAINE_TYPE)
#
plot(log(forestStands$surface), forestStands$proba, col= forestStands$DOMAINE_TYPE)
summary(forestStands$proba)
# 
# sample managed and unmanaged forests
forestStands$EXPLOITABILITY <- as.numeric(forestStands$proba > runif(nrow(forestStands)))
round(table(forestStands$EXPLOITABILITY)/nrow(forestStands)*100)

# recap global des surfaces
# table ponderee
names(forestStands)
summary(forestStands[,-26])
#
# exploitabilite par type de peuplement salem
round(questionr::wtd.table(forestStands$FOREST_TYPE_CODE, forestStands$EXPLOITABILITY, weights = forestStands$AREA, digits=-4)/10000)

# stats surface totale - proportion gérée / non gérée
# par type de peuplement simulé
#
# ajout d'un variable avec la gestion / non gestion
forestStands$gestion <- ifelse(forestStands$nonHarv==0, "NonBuch", ifelse(is.na(forestStands$dist), "NonAcc", ifelse(forestStands$EXPLOITABILITY==0, "AccNonGere", "AccGere")))

# ajout d'un variable avec la classe de peuplement géré
# initialiser la variable
forestStands$typologie <- "test"
forestStands$typologie[forestStands$FOREST_TYPE_CODE=="salem_beech"] <- "Hêtre"
forestStands$typologie[forestStands$FOREST_TYPE_CODE=="salem_oak"] <- "Autres feuillus"
# pour les peuplements dépendant de public / privé
dummy <- which(forestStands$typologie=="test")
forestStands$typologie[dummy] <- forestStands$DOMAINE_TYPE[dummy]
# ajout du type forestier
dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech_spruce", "salem_beech_fir")))
forestStands$typologie[dummy] <- paste0(forestStands$DOMAINE_TYPE[dummy], "_Mixte")
dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_spruce", "salem_fir", "salem_fir_spruce")))
forestStands$typologie[dummy] <- paste0(forestStands$DOMAINE_TYPE[dummy], "_Sapin-épicéa")
#
# Type de peuplement en pourcentage
test <- questionr::wtd.table(forestStands$typologie, weights = forestStands$AREA/10000, digits=0)
round(test/sum(test)*100,1)
# Gestion ou non en pourcentage
test <- questionr::wtd.table(forestStands$gestion, weights = forestStands$AREA/10000, digits=0)
round(test/sum(test)*100,1)
# Tableau croise gestion / peuplement
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$gestion, forestStands$typologie, weights = forestStands$AREA/10000, digits=0))
# En pourcentage des surfaces
apply(test, 2, function(x){round(x/sum(x)*100,1)})
# Tableau croise gestion / peuplement en surface 
test$Total <- as.numeric(apply(test, 1, sum))
test["Total",] <- apply(test, 2, sum)
round(test)
# tableau des pourcentage en excluant les surfaces non bucheronnables
apply(test, 2, function(x){round(x[1:3]/sum(x[1:3])*100,1)})

# define extra plots as non accessible = non harvested
# -- exemple 1 plot sur 2 en chêne privé ne sera pas géré/exploité
# modfier exploitability et dist

# reduire la taille
facteur <- 4
forestStands <- forestStands[sample(1:nrow(forestStands), floor(nrow(forestStands)/facteur), replace=FALSE),]

source('./src/sc1_BAU.R')
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$COMMENT, forestStands$EXPLOITABILITY, weights = forestStands$AREA/10000, digits=0))
round(test,0)
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$gestion, forestStands$COMMENT, weights = forestStands$AREA/10000, digits=0))
round(test,0)
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$typologie, forestStands$COMMENT, weights = forestStands$AREA/10000, digits=0))
round(test,0)
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$DOMAINE_TYPE, forestStands$COMMENT, weights = forestStands$AREA/10000, digits=0))
round(test,0)

# TODO: arriver a ce stade -> executer tous les scenarios de gestion avec le même forestPLot
# (car attention -> processus rdm en amont)

forestStands$nonHarv <- NULL
forestStands$dist <- NULL
forestStands$Gsp1 <- NULL
forestStands$Gsp2 <- NULL
forestStands$G <- NULL
forestStands$proba <- NULL
forestStands$surface <- NULL
forestStands$mnPrclA	<- NULL
forestStands$gestion	<- NULL
forestStands$typologie <- NULL

###############################################################
# create management scenario java file to run SIMMEM
###############################################################

source("./simmem/simmemRules/simmemRulesGenerator.R")

###############################################################
# verification
###############################################################

# Site Index
hist(forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech', 'salem_beech-spruce', 'salem_beech-fir') , 'SITE_INDEX_1'], breaks = 100, border = 'green', ylim = c(0, 350), xlim = c(-10, 130), main = '', xlab = 'site index')
hist(c(forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_fir', 'salem_fir-spruce') , 'SITE_INDEX_1'], forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', 'SITE_INDEX_2']), breaks = 100, add = TRUE, border = 'blue')
hist(c(forestStands[forestStands$FOREST_TYPE_CODE == 'salem_spruce' , 'SITE_INDEX_1'], forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_fir-spruce', 'salem_beech-spruce') , 'SITE_INDEX_2']), breaks = 100, add = TRUE, border = 'orange')
hist(forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak" , 'SITE_INDEX_1'], breaks = 100, col = 'black', add = TRUE)

# Dg
hist(forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech', 'salem_beech-spruce', 'salem_beech-fir') , 'DG_1'], breaks = 200, border = 'green', xlim = c(5, 90), main = '', xlab = 'Dg')
hist(c(forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_fir', 'salem_fir-spruce') , 'DG_1'], forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', 'DG_2']), breaks = 200, add = TRUE, border = 'blue')
hist(c(forestStands[forestStands$FOREST_TYPE_CODE == 'salem_spruce' , 'DG_1'], forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_fir-spruce', 'salem_beech-spruce') , 'DG_2']), breaks = 200, add = TRUE, border = 'orange')
hist(forestStands[forestStands$FOREST_TYPE_CODE == "salem_oak" , 'DG_1'], breaks = 200, col = 'black', add = TRUE)

# n / ha
# pure plots
hist(forestStands[forestStands$NHA_2 == -1, 'NHA_1'] , breaks = 100, ylim = c(0,600), col = 'black')
# mixed plots
hist(forestStands[forestStands$NHA_2 != -1, 'NHA_1'] + forestStands[forestStands$NHA_2 != -1, 'NHA_2'], breaks = 100, border = 'blue3', add = TRUE)

###############################################################
# format
###############################################################

cat('# 1. Global Level\n', file="./output/forestStands.txt")
cat("DATE=2016\n", file="./output/forestStands.txt", append=TRUE)
cat('TOTAL_AREA=', sum(forestStands$AREA), file="./output/forestStands.txt", append=TRUE)
cat('\nXMIN=', xmin, sep = '', file="./output/forestStands.txt", append=TRUE)
cat('\nYMIN=', ymin, sep = '', file="./output/forestStands.txt", append=TRUE)
cat('\nXMAX=', xmax, sep = '', file="./output/forestStands.txt", append=TRUE)
cat('\nYMAX=', ymax, sep = '', file="./output/forestStands.txt", append=TRUE)
cat('\n# 2. Forest Unit Level', file="./output/forestStands.txt", append=TRUE)
cat('\n#', file="./output/forestStands.txt", append=TRUE)
write.table(forestStands, file="./output/forestStands.txt", row.names = FALSE, append=TRUE, quote = FALSE, sep = '\t')
