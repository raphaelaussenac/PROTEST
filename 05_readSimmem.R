###############################################################
# initialisation
###############################################################

# clean up environment
# rm(list = setdiff(ls(), "user"))

# load packages
library(ggplot2)
library(plyr)

# set work directory
setwd("/home/jean-matthieu/R/PROTEST")
# mean annual area
period <- 30

# load SIMMEM output file
df <- read.csv(file="./simmem/simmemOutput/exportAutoBAU.txt", sep = "\t", skip = 3)
colnames(df)[1] <- "standId"

# load SIMMEM input file to retrieve plot surface
forestStands <- read.csv(file="./output/forestStands.txt", sep = "\t", skip = 8)
colnames(forestStands)[1] <- "STAND_ID"

###############################################################
# manage format
###############################################################

# create columns for sp2
df$realSpecies2 <- -1
df$Ddom2 <- -1

# separate sp1 and sp2 data
df[df$realSpecies == 'Fagus sylvatica;Picea abies', 'realSpecies2'] <- 'Picea abies'
df[df$realSpecies == 'Fagus sylvatica;Abies alba', 'realSpecies2'] <- 'Abies alba'
df[df$realSpecies == 'Abies alba;Picea abies', 'realSpecies2'] <- 'Picea abies'

colnames(df)[colnames(df) == "realSpecies"] <- "realSpecies1"
df[df$realSpecies1 == 'Fagus sylvatica;Picea abies', 'realSpecies1'] <- 'Fagus sylvatica'
df[df$realSpecies1 == 'Fagus sylvatica;Abies alba', 'realSpecies1'] <- 'Fagus sylvatica'
df[df$realSpecies1 == 'Abies alba;Picea abies', 'realSpecies1'] <- 'Abies alba'

# TODO: separate Ddom1 and Ddom2

# retrieve area
df <- merge(df, forestStands[,c("STAND_ID", "AREA")], by.x = "standId", by.y = "STAND_ID")

# managType
df$managType <- substr(df$comment, 1, 3)

# compo
df$compo <- substr(df$comment, 4, 6)

# owner
df$owner <- substr(df$comment, 7, 8)

# order by standId and then by date
df <- df[order(df$standId, df$date), ]


###############################################################
# remove repeated clearcut
###############################################################
if(1)
{
  # order df by standid then date
  df <- df[order(df$standId, df$date),]
  testStand <- df$standId[1:(nrow(df)-1)]==df$standId[2:nrow(df)]
  testYear <- df$date[1:(nrow(df)-1)]==df$date[2:nrow(df)]-3
  testVolume <- df$volumeRemoved_m3[1:(nrow(df)-1)]==df$volumeRemoved_m3[2:nrow(df)]
  testVolumeZero <- df$volumeRemoved_m3[-nrow(df)]!=0
  testManagTyp <- df$managType[-nrow(df)]=="Har"
  # testBasalArea <- df$basalArea_m2[1:(nrow(df)-1)]>=df$basalArea_m2[2:nrow(df)]
  falseVolume <- which(testStand & testVolume & testVolumeZero & testYear)
  df$volumeRemoved_m3[falseVolume+1] <- 0
  #
  # test volume removed while ba increases
  which(df$basalArea_m2[1:(nrow(df)-1)]<=df$basalArea_m2[2:nrow(df)] & df$volumeRemoved_m3[-nrow(df)] >0)
  # 
  sum(df$volumeRemoved_m3[df$managType=="Har"])
}

# remove last year
df <- df[df$date!=max(df$date),]


###############################################################
# basal area and density (rdi)
###############################################################
df$standColor <- as.numeric(as.factor(df$standId))

if (0)
{
  # BA
  ggplot(data = df, aes(x = date, y = basalArea_m2, group = standId, col = standId)) +
    geom_line() +
    scale_color_gradient2(low="blue", mid="green", high="red", midpoint = mean(df$standColor)) +
    # geom_point() +
    facet_grid(compo ~ managType)
  
  # RDI
  ggplot(data = df, aes(x = date, y = density_01, group = standId, col = standId)) +
    geom_line() +
    scale_color_gradient2(low="blue", mid="green", high="red", midpoint = mean(df$standColor)) +
    # geom_point() +
    facet_grid(compo ~ managType)
}


###############################################################
# Annual volume removed
###############################################################

# time step = 3 yrs
timeStep <- 3

# multiply volume * area / 3 yrs
df$annualVolumeRemoved_m3 <- df$volumeRemoved_m3 * (df$AREA / 10000) / timeStep

# total volume removed each year
voltot <- ddply(df[df$volumeRemoved_m3 > 0,], .(date), summarise, vol = sum(annualVolumeRemoved_m3))
plot(voltot$vol ~ voltot$date, pch = 16, ylab = "volume exploité ou éclairci", xlab = "")
lines(voltot$vol ~ voltot$date)

# volume removed each year for each managtype
voltot <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, managType), summarise, vol = sum(annualVolumeRemoved_m3))
ggplot(data = voltot, aes(x = date, y = vol, group = managType, col = managType)) +
  geom_line() +
  geom_point()

# volume removed each year for each compo type
voltot <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, compo), summarise, vol = sum(annualVolumeRemoved_m3))
ggplot(data = voltot, aes(x = date, y = vol, group = compo, col = compo)) +
  geom_line() +
  geom_point()

# volume removed each year for each ownership type
voltot <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, domainType), summarise, vol = sum(annualVolumeRemoved_m3))
ggplot(data = voltot, aes(x = date, y = vol, group = domainType, col = domainType)) +
  geom_line() +
  geom_point()

# volume removed each year for each managtype and compo
vol <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, managType, compo), summarise, vol = sum(annualVolumeRemoved_m3))
ggplot(data = vol, aes(x = date, y = vol, group = compo)) +
  geom_line() +
  geom_point() +
  facet_grid(compo ~ managType)

###############################################################
# Annual surface harvested/thinned
###############################################################

# time step = 3 yrs
df$annualAREA <- (df$AREA / 10000) / timeStep
df[df$annualVolumeRemoved_m3 == 0, "annualAREA"] <- 0

# total area harvested/thinned each year
areatot <- ddply(df[df$volumeRemoved_m3 > 0,], .(date), summarise, area = sum(annualAREA))
plot(areatot$area ~ areatot$date, pch = 16, ylab = "surface exploitée - éclaircie", xlab = "")
lines(areatot$area ~ areatot$date)

# total area harvested/thinned each year for each managType
area <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, managType), summarise, area = sum(annualAREA))
ggplot(data = area, aes(x = date, y = area, group = managType, col = managType)) +
  geom_line() +
  geom_point()

# surface harvested each year for each ownership type
areaPP <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, domainType), summarise, area = sum(annualAREA))
ggplot(data = areaPP, aes(x = date, y = area, group = domainType, col = domainType)) +
  geom_line() +
  geom_point()

# surface harvested each year for each ownership type for Har scenario
areaPP <- ddply(df[df$volumeRemoved_m3 > 0 & df$managType=="Th2" & df$compo=="bfs",], .(date, domainType), summarise, area = sum(annualAREA))
ggplot(data = areaPP, aes(x = date, y = area, group = domainType, col = domainType)) +
  geom_line() +
  geom_point()

# surface harvested each year for each compo type
areaPP <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, compo), summarise, area = sum(annualAREA))
ggplot(data = areaPP, aes(x = date, y = area, group = compo, col = compo)) +
  geom_line() +
  geom_point()

# total area harvested/thinned each year for each managType and compo
area <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, managType, compo), summarise, area = sum(annualAREA))
ggplot(data = area, aes(x = date, y = area, group = managType)) +
  geom_line() +
  geom_point() +
  facet_grid(compo ~ managType)

#
##############################################
# load full file
forestStands <- read.table("./output/forestStandsFULL.txt", header=TRUE, sep = '\t')
forestStands$WKT.GEOM <- as.character(forestStands$WKT.GEOM)
forestStands <- sf::st_sf(forestStands[,-which(names(forestStands)=="WKT.GEOM")], geom=sf::st_as_sfc(forestStands$WKT.GEOM))
sf::st_crs(forestStands) <- 2154
#
# mean annual harvest over interest period
dummy <- df[df$date<min(df$date+period),]
dummy <- lapply(split(dummy$volumeRemoved_m3, dummy$standId),
                 FUN=function(x)
                 {
                   mean.operation.per.year <- sum(x>0)/period
                   mean.volume.per.operation <- mean(x[x>0])
                   mean.volume.per.year <- sum(x)/period
                   data.frame(mean.operation.per.year, mean.volume.per.operation, mean.volume.per.year)
                 }
)
dummy.names <- names(dummy)
dummy <- do.call(rbind, dummy)
dummy$STAND_ID <- dummy.names
#
forestStands <- merge(forestStands, dummy)
# put 0 to plots without operations
forestStands$mean.volume.per.operation[is.na(forestStands$mean.volume.per.operation)] <- 0

sf::st_write(forestStands["STAND_ID"], "./output/forestStandsSimulatedPolygons.shp", delete_layer=TRUE)
setwd("./output")
write.table(sf::st_drop_geometry(forestStands), sep=";", row.names=FALSE, file="forestStandsSimulatedData.csv")
#
###########################################
#
# recap varies et interessants
#
periodsubset <- which(df$date<min(df$date+period))
mean(aggregate(df$annualAREA, list(df$date), FUN=sum)$x)
mean(aggregate(df$annualVolumeRemoved_m3, list(df$date), FUN=sum)$x)
#
# recap 
summary(forestStands[,-26])
hist(forestStands$RDI)
boxplot(RDI~gestion, data=forestStands)
boxplot(RDI~COMMENT, data=forestStands)
#
# Type de peuplements et  itinéraires
dummy <- as.data.frame.matrix(questionr::wtd.table(forestStands$typologie, substr(forestStands$COMMENT, 1, 2), weights = forestStands$AREA/10000, digits=0))
dummy <- as.matrix(t(dummy))
barplot(dummy, col=c("darkgrey", "red", "orange", "yellow"), las=2)
legend("topright", c("Inaction", "Coupe rase", "Irrégulier", "Amélioration"), fill=c("darkgrey", "red", "orange", "yellow"))
barplot(apply(dummy, 2, function(x){x/sum(x)*100}), width=apply(dummy, 2, sum), col=c("darkgrey", "red", "orange", "yellow"), las=2)
test <- apply(dummy, 2, sum)
round(test/sum(test)*100, 1)
test <- apply(dummy, 1, sum)
round(test/sum(test)*100, 1)
pie(test, col=c("darkgrey", "red", "orange", "yellow"), labels=paste(c("Inaction", "Coupe rase", "Irrégulier", "Amélioration"), round(test/sum(test)*100, 1)))

# gestion et type de peuplements
dummy <- as.data.frame.matrix(questionr::wtd.table(forestStands$typologie, forestStands$gestion, weights = forestStands$AREA/10000, digits=0))
dummy <- as.matrix(t(dummy))
barplot(dummy, col=c("green", "blue", "pink", "purple"), las=2)
legend("topright", c("Accessible géré", "Accessible non géré", "Non débardable", "Non bûcheronnable"), fill=c("green", "blue", "pink", "purple"))
barplot(apply(dummy, 2, function(x){x/sum(x)*100}), width=apply(dummy, 2, sum), col=c("green", "blue", "pink", "purple"), las=2)
test <- apply(dummy, 2, sum)
round(test/sum(test)*100, 1)
test <- apply(dummy, 1, sum)
round(test/sum(test)*100, 1)
pie(test, col=c("green", "blue", "pink", "purple"), labels=paste(c("Accessible géré", "Accessible non géré", "Non débardable", "Non bûcheronnable"), round(test/sum(test)*100, 1)))

# État initial des peuplements
# test <- aggregate(AREA~COMMENT, data)
par(las=2)
for (i in unique(substr(forestStands$COMMENT, 1, 2)))
{
  dummy <- forestStands[substr(forestStands$COMMENT, 1, 2)==i,]
  dummy$COMMENT <- factor(dummy$COMMENT)
  test <- aggregate(AREA~COMMENT, data=dummy, FUN=sum)
  boxplot(RDI~COMMENT, data=dummy, xlab="", width=test$AREA)#, xaxt="n")
  if (i=="Ha") {abline(h=1.5, col="red");abline(h=c(1.5-0.05, 1.5+0.05), col="red", lty=2)}
  if (i=="Th") {abline(h=c(0.6, 0.7), col="red");abline(h=c(0.55, 0.65, 0.75), col="red", lty=2)}
  if (i=="Ir") {abline(h=c(0.5, 0.7, 0.9), col="red");abline(h=c(0.4, 0.6, 0.8), col="red", lty=2)}
  #
  boxplot(DG_1~COMMENT, data=dummy, xlab="", width=test$AREA)#, xaxt="n")
  if (i=="Ha") {points(c(1,2,3), c(30, 40, 30),col="red", pch=3)}
  if (i=="Th") {points(c(1,2,3,4, 6, 7,8,9, 3,5,8,10), c(rep(40,8), rep(50, 4)),col="red", pch=3)}
}

# volumes et surface traités
# total area harvested/thinned each year for each managType and compo
df$iti <- substr(df$managType, 1, 2)
area <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, iti, compo, domainType), summarise, area = sum(annualAREA))
ggplot(data = area, aes(x = date, y = area, group = domainType, col=domainType)) +
  geom_line() +
  #geom_point() +
  facet_grid(compo ~ iti)

# total vol harvested/thinned each year for each managType and compo
vol <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, iti, compo, domainType), summarise, vol = sum(annualVolumeRemoved_m3))
ggplot(data = vol, aes(x = date, y = vol, group = domainType, col=domainType)) +
  geom_line() +
  #geom_point() +
  facet_grid(compo ~ iti)

managed.plots <- which(substr(forestStands$COMMENT, 1, 3)!="Con")
# récolte moyenne annuelle
plotrix::weighted.hist(forestStands$mean.volume.per.year[managed.plots], forestStands$AREA[managed.plots]/10000, xlab="Volume (m3/ha/an)", ylab="Surface (ha)", main="Récolte moyenne pour les parcelles gérées")
# récolte moyenne par parcelle (pondérée par surface et nombre d'opération)
plotrix::weighted.hist(forestStands$mean.volume.per.operation[managed.plots], forestStands$AREA[managed.plots]*forestStands$mean.operation.per.year[managed.plots]/10000, xlab="Volume (m3/ha)", ylab="Surface (ha)", main="Récolte moyenne par opération, par parcelle")

if (0)
{
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
}
