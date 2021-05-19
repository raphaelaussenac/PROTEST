# clean up environment
rm(list = setdiff(ls(), "user"))

# REORGANISER SCRIPTS : FAIRE MODELISATION COMPO ET DENDRO PUIS POINT DE SAUVEGARDE
# ENSUITE POSSIBILITE DE MODIFIER GESTION OU NON ET ITINERAIRE AVANT EXPORT

# load packages
library(plyr)
library(reshape2)

###############################################################
# import
###############################################################

# Site index
# requires access to rastDg file
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

# check if polygons were removed during previous step -> no
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

############################################
# COMPOSITION DONE - NOW MANAGEMENT
# 
save(list=ls(), file="intermediaryExport.rda")
rm(list=ls())
load(file="intermediaryExport.rda")
summary(forestStands[,-which(names(forestStands)=="WKT-GEOM")])
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
# replace distances by before application of model
# (was calibrated with NA distances replaced by 2000)
# first save data
#forestStands$dist <- forestStands$distWithNA
forestStands$distWithNA <- forestStands$dist
# forestStands$dist[is.na(forestStands$dist)] <- 2000
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
# public stands : proba decreases linearly from 1 at 0m and 0.75 at 2000m
forestStands$proba[dummy] <- 1 - 0.25/2000 * forestStands$dist[dummy]
plot(forestStands$dist, forestStands$proba, col=forestStands$DOMAINE_TYPE)

# for all forests
# forests with NA distance have a probability of 0 (although the model of private forests considers them as tough their distance was 2000)
forestStands$proba[is.na(forestStands$dist)] <- 0
# set proba to zero for forest further than 2000
forestStands$proba[forestStands$dist>2000] <- 0
#
# nonHarv forest have a probability of 0 (although the model of private forests does not includes this variable)
forestStands$proba[forestStands$nonHarv==0] <- 0
# set dist to NA of polygons not harvestable for coherence
forestStands$dist[forestStands$nonHarv == 0]<- NA
#
plot(forestStands$dist, forestStands$proba, col=forestStands$DOMAINE_TYPE, xlab="Distance moyenne de débardage (m)", ylab="Probabilité de gestion", cex=0.1)
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
summary(forestStands[,-which(names(forestStands)=="WKT-GEOM")])
#
# gestion par type de peuplement salem
round(questionr::wtd.table(forestStands$FOREST_TYPE_CODE, forestStands$EXPLOITABILITY, weights = forestStands$AREA, digits=-4)/10000)

# stats surface totale - proportion gérée / non gérée
# par type de peuplement simulé
#
# ajout d'un variable avec la gestion / non gestion
forestStands$gestion <- ifelse(forestStands$nonHarv==0, "NonBuch", ifelse(is.na(forestStands$dist), "NonAcc", ifelse(forestStands$EXPLOITABILITY==0, "AccNonGere", "AccGere")))

# ajout d'une variable avec la classe de peuplement géré
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
t(apply(test, 2, function(x){round(x[1:3]/sum(x[1:3])*100,1)}))


test <- as.data.frame.matrix(questionr::wtd.table(forestStands$typologie, forestStands$gestion, weights = forestStands$AREA/10000, digits=0))
round(test,0)
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$nonHarv, forestStands$gestion, weights = forestStands$AREA/10000, digits=0))
round(test,0)

source("./code1.R")
source('./src/sc1_BAU.R')
source("./code2.R")

save(list=ls(), file="intermediaryExport2.rda")
rm(list=ls())
load(file="intermediaryExport2.rda")

# reduire la taille
# facteur <- 4
# forestStands <- forestStands[sample(1:nrow(forestStands), floor(nrow(forestStands)/facteur), replace=FALSE),]
# forestStands <- forestStands[1:2,]

# forestStands <- forestStands[is.element(substr(forestStands$COMMENT,1,3), c("Har", "Th2")),]

#
# table EXPLOITABILITY / silviculture
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$COMMENT, forestStands$EXPLOITABILITY, weights = forestStands$AREA/10000, digits=0))
round(test,0)
# table Gestion / silviculture
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$gestion, forestStands$COMMENT, weights = forestStands$AREA/10000, digits=0))
round(test,0)
# table stand type / silviculture
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$typologie, forestStands$COMMENT, weights = forestStands$AREA/10000, digits=0))
round(test,0)
# table stand type / silviculture type
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$typologie, substr(forestStands$COMMENT,1,3), weights = forestStands$AREA/10000, digits=0))
round(test,0)
# table gestion / silviculture type
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$gestion, substr(forestStands$COMMENT,1,3), weights = forestStands$AREA/10000, digits=0))
round(test,0)
# table owner / silviculture
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$DOMAINE_TYPE, forestStands$COMMENT, weights = forestStands$AREA/10000, digits=0))
round(test,0)
# table gestion / silviculture
test <- as.data.frame.matrix(questionr::wtd.table(forestStands$gestion, forestStands$COMMENT, weights = forestStands$AREA/10000, digits=0))
round(test,0)

# TODO: arriver a ce stade -> executer tous les scenarios de gestion avec le même forestPLot
# (car attention -> processus rdm en amont)

# write full table
write.table(forestStands, file="./output/forestStandsFULL.txt", row.names = FALSE, quote = FALSE, sep = '\t')

forestStands[, c('nonHarv', 'dist', 'distWithNA', 'Gsp1', 'Gsp2', 'G', 'proba', 'surface', 'mnPrclA', 'gestion', 'typologie', 'RDI1', 'RDI2', 'RDI', 'Ntot', 'Dgtot', 'probaDg', 'probaRDI', 'jointProba')] <- NULL

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

