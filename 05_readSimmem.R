###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = setdiff(ls(), "user"))

# load packages
library(ggplot2)
library(plyr)

# set work directory
setwd(user$WorkingDir)

# load SIMMEM output file
df <- read.csv(file="./simmem/simmemOutput/exportAutoBAU_JMM_Irr2_modifDiam.txt", sep = "\t", skip = 3)
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
  # write.table(df[,c("standId","volumeRemoved_m3", "basalArea_m2", "date", "managType")],file="~/test.tsv", sep="\t")
  sum(df$volumeRemoved_m3[df$managType=="Har"])
}

if (0)
{
  # le script de Raph enleve toutes les coupes initiales (2016)
  df$nextYear <- df$date + 3
  df$standId_year <- paste(df$standId, df$date, sep = "_")
  df$standId_nextYear <- paste(df$standId, df$nextYear, sep = "_")
  test <- merge(df, df[, c("volumeRemoved_m3", "standId_nextYear")], by.x = "standId_year", by.y = "standId_nextYear", all = TRUE)
  test <- test[!is.na(test$standId),]
  test$diff <- test$volumeRemoved_m3.y - test$volumeRemoved_m3.x
  test[is.na(test$diff), 'diff'] <- 0
  test[test$diff < 0, 'diff'] <- test[test$diff < 0, 'diff'] * -1
  test$volumeRemoved_m3.x <- test$diff
  test$diff <- NULL
  test$volumeRemoved_m3.y <- NULL
  test$standId_nextYear <- NULL
  test$nextYear <- NULL
  test$standId_year <- NULL
  df <- test
  colnames(df)[colnames(df) == "volumeRemoved_m3.x"] <- 'volumeRemoved_m3'
  # test no volume removed while ba decreases
  which(df$basalArea_m2[1:(nrow(df)-1)]>df$basalArea_m2[2:nrow(df)] & df$volumeRemoved_m3[-nrow(df)] ==0)
}


# remove last year

df <- df[df$date!=max(df$date),]
###############################################################
# basal area and density (rdi)
###############################################################
df$standColor <- as.numeric(as.factor(df$standId))

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
# mean annual area
period <- 30
periodsubset <- which(df$date<min(df$date+period))
mean(aggregate(df$annualAREA, list(df$date), FUN=sum)$x)
mean(aggregate(df$annualVolumeRemoved_m3, list(df$date), FUN=sum)$x)

#
# load full file
dummy <- read.table("./output/forestStandsFULL.txt", header=TRUE, sep = '\t')
dummy$WKT.GEOM <- as.character(dummy$WKT.GEOM)
fsfull <- sf::st_sf(dummy[,-which(names(dummy)=="WKT.GEOM")], geom=sf::st_as_sfc(dummy$WKT.GEOM))
sf::st_crs(fsfull) <- 2154
#
# recap 
summary(fsfull)
hist(fsfull$RDI)
boxplot(RDI~gestion, data=fsfull)
boxplot(RDI~COMMENT, data=fsfull)
# test <- aggregate(AREA~COMMENT, data)
par(las=2)
for (i in unique(substr(fsfull$COMMENT, 1, 2)))
{
  dummy <- fsfull[substr(fsfull$COMMENT, 1, 2)==i,]
  dummy$COMMENT <- factor(dummy$COMMENT)
  test <- aggregate(AREA~COMMENT, data=dummy, FUN=sum)
  boxplot(RDI~COMMENT, data=dummy, xlab="", width=test$AREA)#, xaxt="n")
  if (i=="Ha") {abline(h=1.5, col="red");abline(h=c(1.5-0.05, 1.5+0.05), col="red", lty=2)}
  if (i=="Th") {abline(h=c(0.6, 0.7), col="red");abline(h=c(0.55, 0.65, 0.75), col="red", lty=2)}
  if (i=="Ir") {abline(h=c(0.5, 0.7, 0.9), col="red");abline(h=c(0.4, 0.6, 0.8), col="red", lty=2)}
  #
  boxplot(DG_1~COMMENT, data=dummy, xlab="", width=test$AREA)#, xaxt="n")
  if (i=="Ha") {points(c(1,2,3), c(20, 40, 20),col="red", pch=3)}
  if (i=="Th") {points(c(1,2,3,4, 6, 7,8,9, 3,5,8,10), c(rep(40,8), rep(50, 4)),col="red", pch=3)}
}
# mean annual harvest

dummy <- df[df$date<min(df$date+period),]
#
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
fsfull <- merge(fsfull, dummy)
#
# only managed plots
fsfull$mean.volume.per.operation[is.na(fsfull$mean.volume.per.operation)] <- 0
managed.plots <- which(substr(fsfull$COMMENT, 1, 3)!="Con")
# récolte moyenne annuelle
plotrix::weighted.hist(fsfull$mean.volume.per.year[managed.plots], fsfull$AREA[managed.plots]/10000, xlab="Volume (m3/ha/an)", ylab="Surface (ha)", main="Récolte moyenne pour les parcelles gérées")
# récolte moyenne par parcelle (pondérée par surface et nombre d'opération)
plotrix::weighted.hist(fsfull$mean.volume.per.operation[managed.plots], fsfull$AREA[managed.plots]*fsfull$mean.operation.per.year[managed.plots]/10000, xlab="Volume (m3/ha)", ylab="Surface (ha)", main="Récolte moyenne par opération, par parcelle")
#
fsfull2 <- fsfull
names(fsfull2) <- c(paste("n", 1:(length(names(fsfull2))-1)), "geometry")
sf::st_write(fsfull2, "forestStandsSimulated.shp", delete_layer=TRUE)

