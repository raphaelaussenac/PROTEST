############################################
# COMPOSITION DONE - NOW MANAGEMENT
# 
rm(list = setdiff(ls(),"user"))
load(file="forestStands03c.rda")
#
# DOMAINE_TYPE
colnames(forestStands)[colnames(forestStands) == "owner"] <- 'DOMAINE_TYPE'

# EXPLOITABILITY
colnames(forestStands)[colnames(forestStands) == "access"] <- 'EXPLOITABILITY'


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
forestStands$surface <- forestStands$meanParcelleArea/10000
#
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
plot(log(forestStands$surface), forestStands$proba, col=forestStands$DOMAINE_TYPE)

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
summary(forestStands[,-which(names(forestStands)=="geom")])
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

# recuperer spatial extent
xmin <- raster::extent(forestStands)@xmin
ymin <- raster::extent(forestStands)@ymin
xmax <- raster::extent(forestStands)@xmax
ymax <- raster::extent(forestStands)@ymax

# enlever la geometrie (suite du code requiert un data.frame)
forestStands$`WKT-GEOM` <- sf::st_as_text(forestStands$geom)
forestStands <- as.data.frame(forestStands)
# forestStands <- forestStands[,-which(is.element(colnames(forestStands),c("area", "geom")))]
#
# calculate RDI and compute probability of being irregular stand
source("./code1.R")

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
# affecter les itineraires sylvicoles
source('./src/sc1_BAU.R')
# graphiques
source("./code2.R")

#####################################

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


# write full table
forestStands$geom <- NULL
write.table(forestStands, file="./output/forestStandsFULL.txt", row.names = FALSE, quote = FALSE, sep = '\t')


# forestStands[, c('nonHarv', 'dist', 'Gsp1', 'Gsp2', 'G', 'proba', 'surface', 'geom','meanParcelleArea', 'gestion', 'typologie', 'RDI1', 'RDI2', 'RDI', 'Dgtot', 'probaDg', 'probaRDI', 'jointProba', 'area')] <- NULL

forestStands <- forestStands[,c("STAND_ID", "FOREST_TYPE_CODE", "FOREST_TYPE_NAME", "AREA", "SITE_INDEX_1", "NHA_1", "AGE_1", "HDOM_1", "DDOM_1", "HG_1", "DG_1", "SITE_INDEX_2", "NHA_2", "AGE_2", "HDOM_2", "DDOM_2", "HG_2", "DG_2", "EXPLOITABILITY", "DOMAINE_TYPE", "FOREST", "INVENTORY_DATE", "DEPARTMENT", "CITY", "COMMENT", "WKT-GEOM")]

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

