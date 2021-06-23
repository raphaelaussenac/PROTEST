############################################
# COMPOSITION DONE - NOW MANAGEMENT
# 
rm(list = setdiff(ls(),"user"))
scenario <- "TRE"
load(file="./data/forestStands03c.rda")
if (0) # add protected forests info
{
  #############################################
  # protected forests
  protected.forests <- sf::st_read(dsn=paste0(user$NetworkProtestDir, "T1/Donnees_SIG/ZonesProtegees/Union_FRENE_RB_APB_PNRfilled.shp"))
  # rasterizeprotected forests
  elev <- raster::raster(paste0(user$NetworkProtestDir, "T1/Donnees_SIG/MNT/MNT_all_5m.tif"))
  protected.forests.raster <- fasterize::fasterize(protected.forests, elev, background = -1)
  protected.forests.raster <- protected.forests.raster < 0
  protected.forestsVr <- velox::velox(protected.forests.raster)
  rm(protected.forests.raster)
  protected.forestsExt <- protected.forestsVr$extract(sp = forestStands, fun = mean, small = TRUE)
  rm(protected.forestsVr)
  # 
  forestStands$PropNonProtected <- as.numeric(protected.forestsExt)
  save(forestStands, file="./data/forestStands03c.rda")
}
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
# model management probability
if (0)
{
  # load inquiry data
  inquiry <- sf::st_read("./data/mihai/EnqOG_Bauges.shp", stringsAsFactors=TRUE)
  summary(inquiry)
  inquiry$gestion <- inquiry$Q7 != "(Vous n\u0092avez pas effectué de coupe sur cette parcelle)"
  load(file = "./data/forestStandsMihai03c.rda")
  test <- merge(sf::st_drop_geometry(inquiry[,c("id", "gestion")]), forestStands)
  # set NA values in forestStands to 10000
  test$proximity <- 1/test$dist
  test$proximity[is.na(test$proximity)] <- 0
  # surface in 
  test$surface <- test$AREA/10000
  # model.glm <- glm(gestion ~ gelNttn + AREA + greco + alti + slope + expoNS + expoEW + dgPred + gPred + ph + rum + p100gfP + Cd_crbn + Cd_hydr + rochClc + pot03 + pot03Epsilon + pot09 + pot09Epsilon + pot61 + pot61Epsilon + pot62 + pot62Epsilon + nonHarv + dist, family = binomial, data = test)
  # model.glm <- glm(gestion ~ gelNttn + surface + greco + alti + slope + expoNS + expoEW + dgPred + gPred + ph + rum + p100gfP + Cd_crbn + Cd_hydr + rochClc + proximity + nonHarv + dist, family = binomial, data = test)
  # model.glm <- glm(gestion ~ surface + greco + alti + slope + expoNS + expoEW + dgPred + gPred + ph + rum + p100gfP + Cd_crbn + Cd_hydr + rochClc + nonHarv , family = binomial, data = test)
  # model.glm <- glm(gestion ~ surface + greco + alti + slope + expoNS + expoEW + dgPred + gPred + ph + rum + p100gfP + Cd_crbn + Cd_hydr + rochClc, family = binomial, data = test)
  # model.glm <- glm(gestion ~ surface + alti + slope + expoNS + expoEW + ph + rum + p100gfP + Cd_crbn + Cd_hydr + rochClc + dist, family = binomial, data = test)
  # #
  # gam.model <- mgcv::gam(gestion ~ surface + greco + alti + slope + expoNS + expoEW + dgPred + gPred + ph + rum + p100gfP + Cd_crbn + Cd_hydr + rochClc + s(dist), family = binomial, data = test)
  # # Summarize model
  # summary(gam.model )
  # mgcv::plot.gam(gam.model)
  # test$dist <- ifelse(is.na(test$dist), 2000, pmin(test$dist, 2000))
  # model.glm.2 <- glm(gestion ~ log(surface) + I((dist-1000) * (dist<2000)) + slope + alti + expoNS + expoEW , family = binomial, data = test[test$nonHarv==1,])
  # model.glm.2 <- glm(gestion ~ log(surface) + I((dist-1000) * (dist<2000)) + slope , family = binomial, data = test[test$nonHarv==1,])
  # model.glm.2 <- MASS::stepAIC(model.glm.2, direction = "both", trace  = FALSE)
  # summary(model.glm.2)
  # model.glm <- glm(gestion ~ log(surface) + I((dist-1000) * (dist<2000)) + slope , family = binomial, data = test[test$nonHarv==1,])
  model.glm <- glm(gestion ~ log(surface) + dist + slope , family = binomial, data = test[test$nonHarv==1 & test$dist<2000,])
  model.glm.step <- MASS::stepAIC(model.glm, direction = "both", trace = FALSE)
  summary(model.glm)
  summary(model.glm.step)
  model.glm <- model.glm.step
  save(model.glm, file = "./data/modelGestion2.rda")
}
# load glm binomial model calibrated on Mihai dataset
# should the forest type (TFV) be added ?
# load(file="./data/modelGestion.rda")
# new model
load(file="./data/modelGestion2.rda")

# convert mean parcel surface in hectares
forestStands$surface <- forestStands$meanParcelleArea/10000
#
# change distance for TRE to include accessibility due to additional roads & tracks
if (scenario =="TRE")
{
  # load skidding distance
  dist  <- raster::raster(paste0(user$NetworkProtestDir, "T1/Accessibilite/sylvaccess/sylvaccessv3.3/Skidder_TRE/PNRfilled_F.distance.tif"))
  # numeric value (transport distance between felling place and roadside) NA (forest not accessible OR not forest not harvestable OR outside forest)
  # set projection
  raster::crs(dist) <- 2154
  # remove extreme (error) value
  dist[dist > 100000] <- NA
  # extract dist
  # if most of the plot is inaccessible (x = NA) -> inaccessible (i.e. plot dist = NA)
  # if most of the plot is accessible (x != NA) -> dist = mean(dist != NA)
  # a proportio nof 0.85 is applied so that surfaces are globally consistent between raster and polygons
  getdist <- function(x) {
    inaccess <- length(x[is.na(x)])
    access <- length(x[!is.na(x)])
    if (inaccess > 0.85 *access){
      NA
    } else {
      mean(x, na.rm = TRUE)
    }
  }
  distVr <- velox::velox(dist)
  rm(dist)
  distExt <- distVr$extract(sp = forestStands, fun = getdist, small = TRUE)
  rm(distVr)
  gc()
  #
  forestStands$dist <- as.vector(distExt)
}
#
# save surface field
forestStands$surface.save <- forestStands$surface
# save dist field
forestStands$dist.save <- forestStands$dist
#
###########################################
# modificateur via surface
if (scenario == "PTR")
{
  # surface equivalente de base pour public : 300 ha
  forestStands$surface[forestStands$DOMAINE_TYPE=="Pub"] <- 300
}
if (scenario =="ADA")
{
  # surface equivalente de base pour public : 300 ha
  forestStands$surface[forestStands$DOMAINE_TYPE=="Pub"] <- 300
  # plutôt jouer sur la surface de la parcelle (regroupement de propriétaires) ?
  
  # diminuer le conservatoire en sapin / épicéa privé (~ -25%)
  # augmenter la probabilité de gestion
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_spruce", "salem_fir", "salem_fir_spruce")) & forestStands$DOMAINE_TYPE == "Priv")
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 10
  # forestStands$proba[dummy] <- pmin(forestStands$proba[dummy] * 1.2, 1) # multiply management probability
  #
  # diminuer le conservatoire en mixte privé (~ -25%)
  # augmenter la probabilité de gestion
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech_spruce", "salem_beech_fir")) & forestStands$DOMAINE_TYPE == "Priv")
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 10
  # forestStands$proba[dummy] <- pmin(forestStands$proba[dummy] * 1.2, 1) # multiply management probability
  #
  # diminuer le conservatoire en sapin / épicéa publique (non prévu)
  # augmenter la probabilité de gestion
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_spruce", "salem_fir", "salem_fir_spruce")) & forestStands$DOMAINE_TYPE == "Pub")
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 3
  # forestStands$proba[dummy] <- pmin(forestStands$proba[dummy] * 1.2, 1) # multiply management probability
  #
  # diminuer le conservatoire en mixte privé (non prévu)
  # augmenter la probabilité de gestion
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech_spruce", "salem_beech_fir")) & forestStands$DOMAINE_TYPE == "Pub")
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 3
  # forestStands$proba[dummy] <- pmin(forestStands$proba[dummy] * 1.2, 1) # multiply management probability
  #
}
#

if (scenario =="TRE")
{
  # surface equivalente de base pour public : 300 ha
  forestStands$surface[forestStands$DOMAINE_TYPE=="Pub"] <- 300
  #
  # diminuer le conservatoire en hetre et autres feuillus (~ -25%)
  # augmenter la probabilité de gestion
  # via augmentation de surface en privé (travail auprès des propriétaires)
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech", "salem_oak")) & forestStands$DOMAINE_TYPE == "Priv")
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 10
  # via diminution de la distance en prive et public (augmentation des prix)
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech", "salem_oak")))
  forestStands$dist[dummy] <- pmax(0, forestStands$dist[dummy] - 500)
  #
  # diminuer le conservatoire en mixte privé (~ -10%)
  # augmenter la probabilité de gestion
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech_spruce", "salem_beech_fir")) & forestStands$DOMAINE_TYPE == "Priv")
  # via augmentation de surface (travail auprès des propriétaires)
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 5
  # via diminution de la distance (augmentation des prix)
  forestStands$dist[dummy] <- pmax(0, forestStands$dist[dummy] - 200)
  #
}
#
if (scenario =="DYN")
{
  # surface equivalente de base pour public : 300 ha
  forestStands$surface[forestStands$DOMAINE_TYPE=="Pub"] <- 300
  #
  # diminuer le conservatoire en sapin / épicéa privé (~ -15%)
  # augmenter la probabilité de gestion
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_spruce", "salem_fir", "salem_fir_spruce")) & forestStands$DOMAINE_TYPE == "Priv")
  # via augmentation de surface en privé (travail auprès des propriétaires)
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 7
  # via distance
  forestStands$dist[dummy] <- pmax(0, forestStands$dist[dummy] -250)
  
  # diminuer le conservatoire en hetre et autres feuillus (~ -15%)
  # augmenter la probabilité de gestion
  # via augmentation de surface en privé (travail auprès des propriétaires)
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech", "salem_oak")) & forestStands$DOMAINE_TYPE == "Priv")
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 7
  # via diminution de la distance en prive et public (augmentation des prix)
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech", "salem_oak")))
  forestStands$dist[dummy] <- pmax(0, forestStands$dist[dummy] - 250)
  #
  # diminuer le conservatoire en mixte privé (~ -15%)
  # augmenter la probabilité de gestion
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_beech_spruce", "salem_beech_fir")) & forestStands$DOMAINE_TYPE == "Priv")
  # via augmentation de surface (travail auprès des propriétaires)
  forestStands$surface[dummy] <- forestStands$surface[dummy] * 7
  # via diminution de la distance (augmentation des prix)
  forestStands$dist[dummy] <- pmax(0, forestStands$dist[dummy] - 250)
  #
}
if (scenario =="SCT")
{
  # surface equivalente de base pour public : 300 ha
  forestStands$surface[forestStands$DOMAINE_TYPE=="Pub"] <- 300
}

#######################################
# APPLY MODEL
# estimate probability of management in private forests
forestStands$proba <- predict.glm(model.glm, forestStands, type="response")
# set back surface values
forestStands$surface <- forestStands$surface.save
# set back distance values
forestStands$dist <- forestStands$dist.save
forestStands$surface.save <- NULL
forestStands$dist.save <- NULL
#
if (scenario =="ADA")
{
  # augmenter encore plus proba de gestion sur zone deperissement
  zone.deperissement <- sf::st_read(dsn="/media/reseau/lessem/ProjetsCommuns/PROTEST/T1/Donnees_SIG/Deperissement/zone.deperissement.shp")
  # merge polygons
  zone.deperissement <- sf::st_union(zone.deperissement)
  # convert stands to spatial objects
  dummy <- sf::st_as_sf(forestStands, wkt = "WKT-GEOM")
  sf::st_crs(dummy) <- 2154
  # test if overlap
  test <- sf::st_intersects(dummy, zone.deperissement, sparse = FALSE)
  forestStands$deperissement <- test
  # augmenter ENCORE la probabilite de gestion dans ces zones, pour les peuplements epicea / sapin 
  dummy <- which(is.element(forestStands$FOREST_TYPE_CODE, c("salem_spruce", "salem_fir", "salem_fir_spruce", "salem_beech_spruce", "salem_beech_fir")) & forestStands$deperissement)
  forestStands$proba[dummy] <- pmin(forestStands$proba[dummy] * 1.1, 1)
}
#
if (scenario =="DYN")
{
  # augmenter encore plus proba de gestion sur zone d'animation
  zone.animation <- sf::st_read(dsn="/media/reseau/lessem/ProjetsCommuns/PROTEST/T1/Donnees_SIG/Admin/LaMotteLeChatelard.shp")
  # merge polygons
  zone.animation <- sf::st_union(zone.animation)
  # convert stands to spatial objects
  dummy <- sf::st_as_sf(forestStands, wkt = "WKT-GEOM")
  sf::st_crs(dummy) <- 2154
  # test if overlap
  test <- sf::st_intersects(dummy, zone.animation, sparse = FALSE)
  forestStands$animation <- test
  # augmenter ENCORE la probabilite de gestion dans ces zones, pour les forets prives
  dummy <- which(forestStands$DOMAINE_TYPE =="Priv" & forestStands$animation)
  forestStands$proba[dummy] <- pmin(forestStands$proba[dummy] * 1.1, 1)
}

if (scenario =="SCT")
{
  # diminuer la probabilité de gestion pour non "autres feuillus"
  dummy <- which(forestStands$FOREST_TYPE_CODE !="salem_oak")
  forestStands$proba[dummy] <- forestStands$proba[dummy] * 0.85
  # augmenter la probabilité de gestion pour "autres feuillus"
  dummy <- which(forestStands$FOREST_TYPE_CODE =="salem_oak")
  forestStands$proba[dummy] <- pmin(forestStands$proba[dummy] * 1.1, 1)
}

# histogram of probability
hist(forestStands$proba)
#
plot(forestStands$dist, forestStands$proba, col=forestStands$DOMAINE_TYPE)
plot(log(forestStands$surface), forestStands$proba, col=forestStands$DOMAINE_TYPE)

# for all forests
# forests with NA distance have a probability of 0 (although the model of private forests considers them as tough their distance was 2000)
forestStands$proba[is.na(forestStands$dist)] <- 0
# set proba to zero for forest further than 2000
# forestStands$proba[forestStands$dist>2000] <- 0
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

# decrease management probability according to proportion of protected areas
forestStands$proba <- forestStands$proba * forestStands$PropNonProtected

#############################################
# AUGMENTATION PROBA DE GESTION

#
# sample managed and unmanaged forests
forestStands$EXPLOITABILITY <- as.numeric(forestStands$proba > runif(nrow(forestStands)))
round(table(forestStands$EXPLOITABILITY)/nrow(forestStands)*100)


#################################################################################
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
#
# affecter les itineraires sylvicoles
# source('./src/sc1_BAU.R')
if(is.element(scenario, c("PTR", "TRE", "DYN")))
{
  dummyscenario <- "PTR"
} else {dummyscenario <- scenario}

source(paste0("./src/sc1_", dummyscenario, ".R"))
# graphiques
source("./code2.R")

#############################################
# TRANSFERT ENTRE ITINERAIRES
if (scenario == "ADA")
{
  # public forests 
  # spruce in fsp irregular to th1/2
  dummy <- which(forestStands$DOMAINE_TYPE == "Pub" & forestStands$FOREST_TYPE_CODE == "salem_spruce" & is.element(forestStands$COMMENT, c("Ir1fspPu", "Ir2fspPu", "Ir3fspPu")))
  forestStands$COMMENT[dummy] <- ifelse(forestStands$RDI[dummy] <= 0.7, "Th1fspPu", "Th2fspPu")
  # half of mixed fsp in irregular to th1/2
  dummy <- which(forestStands$DOMAINE_TYPE == "Pub" & forestStands$FOREST_TYPE_CODE == "salem_fir_spruce" & is.element(forestStands$COMMENT, c("Ir1fspPu", "Ir2fspPu", "Ir3fspPu")))
  dummy <- dummy[sample(1:length(dummy), floor(length(dummy)/2))]
  forestStands$COMMENT[dummy] <- ifelse(forestStands$RDI[dummy] <= 0.7, "Th1fspPu", "Th2fspPu")
  # spruce in bfs irregular to th1/2
  dummy <- which(forestStands$DOMAINE_TYPE == "Pub" & forestStands$FOREST_TYPE_CODE == "salem_beech_spruce" & is.element(forestStands$COMMENT, c("Ir1bfsPu", "Ir2bfsPu", "Ir3bfsPu")))
  forestStands$COMMENT[dummy] <- ifelse(forestStands$RDI[dummy] <= 0.7, "Th1bfsPu", "Th2bfsPu")
  #
  # private forests 
  # spruce in irregular to har
  dummy <- which(forestStands$DOMAINE_TYPE == "Priv" & forestStands$FOREST_TYPE_CODE == "salem_spruce" & is.element(forestStands$COMMENT, c("Ir1fspPr", "Ir2fspPr", "Ir3fspPr")))
  forestStands$COMMENT[dummy] <- "HarfspPr"
  # half of mixed fsp in irregular to har
  dummy <- which(forestStands$DOMAINE_TYPE == "Priv" & forestStands$FOREST_TYPE_CODE == "salem_fir_spruce" & is.element(forestStands$COMMENT, c("Ir1fspPr", "Ir2fspPr", "Ir3fspPr")))
  forestStands$COMMENT[dummy[sample(1:length(dummy), floor(length(dummy)/2))]] <- "HarfspPr"
  # spruce in bfs irregular to har
  dummy <- which(forestStands$DOMAINE_TYPE == "Priv" & forestStands$FOREST_TYPE_CODE == "salem_beech_spruce" & is.element(forestStands$COMMENT, c("Ir1bfsPr", "Ir2bfsPr", "Ir3bfsPr")))
  forestStands$COMMENT[dummy] <- "HarbfsPr"
}

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
write.table(forestStands, file=paste0("./output/forestStandsFULL",scenario,".txt"), row.names = FALSE, quote = FALSE, sep = '\t')


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
fichier <- paste0("./output/forestStands",scenario,".txt")
cat('# 1. Global Level\n', file=fichier)
cat("DATE=2016\n", file=fichier, append=TRUE)
cat('TOTAL_AREA=', sum(forestStands$AREA), file=fichier, append=TRUE)
cat('\nXMIN=', xmin, sep = '', file=fichier, append=TRUE)
cat('\nYMIN=', ymin, sep = '', file=fichier, append=TRUE)
cat('\nXMAX=', xmax, sep = '', file=fichier, append=TRUE)
cat('\nYMAX=', ymax, sep = '', file=fichier, append=TRUE)
cat('\n# 2. Forest Unit Level', file=fichier, append=TRUE)
cat('\n#', file=fichier, append=TRUE)
write.table(forestStands, file=fichier, row.names = FALSE, append=TRUE, quote = FALSE, sep = '\t')

