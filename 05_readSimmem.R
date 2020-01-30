###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
library(ggplot2)
library(plyr)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# load SIMMEM output file
df <- read.csv(file="./input/exportSimuAlain.txt", sep = "\t", skip = 3)
colnames(df)[1] <- "standId"

# load SIMMEM input to retrieve plot surface
forestPlots <- read.csv(file="./output/forestPlots.txt", sep = "\t", skip = 8)
colnames(forestPlots)[1] <- "STAND_ID"

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
df <- merge(df, forestPlots[,c("STAND_ID", "AREA")], by.x = "standId", by.y = "STAND_ID")

# managType
df$managType <- substr(df$comment, 1, 3)

# compo
df$compo <- substr(df$comment, 4, 6)

# owner
df$owner <- substr(df$comment, 7, 8)

# order by standId and then by date
df <- df[order(df$standId, df$date), ]

###############################################################
# verif
###############################################################

# Calculate basal area increment
df$deltaG <- 9999



df <- df[1:1000,]

for (i in unique(df$standId)){
  dfTemp <- df[df$standId == i,]
  for (j in nrow(dfTemp):2){
    deltaG <- dfTemp[j, "basalArea_m2"] - dfTemp[j-1, "basalArea_m2"]
    df[df$standId == i & df$date == dfTemp[j, "date"], "deltaG"] <- deltaG/3
  }
}

hist(df[df$deltaG != 9999 & df$deltaG >= 0, "deltaG"], breaks = 100)

# remove BAI when thinning / harvesting
BAI <- df[df$deltaG < 0, ]

# save table
write.table(BAI, file="./output/BAI.txt", row.names = FALSE, sep = '\t')


###############################################################
# volume output
###############################################################



# multiply volume * area
df$volumeRemoved_m3 <- df$volumeRemoved_m3 * (df$AREA / 10000)

# time step = 3 yrs
df$volumeRemoved_m3 <- df$volumeRemoved_m3 / 3

# total volume removed each year
voltot <- ddply(df, .(date), summarise, vol = sum(volumeRemoved_m3))
plot(voltot$vol ~ voltot$date, pch = 16, ylab = "volume exploitée - éclaircie", xlab = "")
lines(voltot$vol ~ voltot$date)

# volume removed each year for each managtype
voltot <- ddply(df, .(date, managType), summarise, vol = sum(volumeRemoved_m3))
voltot <- ddply(df, .(date, managType), summarise, vol = sum(volumeRemoved_m3))
ggplot(data = voltot, aes(x = date, y = vol, group = managType, col = managType)) +
  geom_line() +
  geom_point()

# volume removed each year for each managtype and compo
vol <- ddply(df, .(date, managType, compo), summarise, vol = sum(volumeRemoved_m3))
ggplot(data = vol, aes(x = date, y = vol, group = compo)) +
  geom_line() +
  geom_point() +
  facet_grid(compo ~ managType)


###############################################################
# surface output
###############################################################

# time step = 3 yrs
df$AREA <- (df$AREA / 3) / 10000

# total area harvested/thinned each year
areatot <- ddply(df[df$volumeRemoved_m3 > 0,], .(date), summarise, area = sum(AREA))
plot(areatot$area ~ areatot$date, pch = 16, ylab = "surface exploitée - éclaircie", xlab = "")
lines(areatot$area ~ areatot$date)

# total area harvested/thinned each year for each managType
area <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, managType), summarise, area = sum(AREA))
ggplot(data = area, aes(x = date, y = area, group = managType, col = managType)) +
  geom_line() +
  geom_point()

# total area harvested/thinned each year for each managType and compo
area <- ddply(df[df$volumeRemoved_m3 > 0,], .(date, managType, compo), summarise, area = sum(AREA))
ggplot(data = area, aes(x = date, y = area, group = managType)) +
  geom_line() +
  geom_point() +
  facet_grid(compo ~ managType)


#
#
# par(mfrow= c(3, 2))
# plot(vol[vol$comment1 == 'Con', 'vol'] ~ vol[vol$comment1 == 'Con', 'date'], pch = 16, xlab = "", ylab = 'vol (m3)', main = "inaction")
# plot(vol[vol$comment1 == 'Irr', 'vol'] ~ vol[vol$comment1 == 'Irr', 'date'], pch = 16, xlab = "", ylab = 'vol (m3)', main = "irregulier")
# plot(vol[vol$comment1 == 'Har', 'vol'] ~ vol[vol$comment1 == 'Har', 'date'], pch = 16, xlab = "", ylab = 'vol (m3)', main = "coupe finale")
# plot(vol[vol$comment1 == 'Thi', 'vol'] ~ vol[vol$comment1 == 'Thi', 'date'], pch = 16, xlab = "", ylab = 'vol (m3)', main = "éclaircie + coupe finale")
# plot(voltot$vol ~ voltot$date, pch = 16, xlab = "", ylab = 'vol (m3)', main = "volume total")
# plot(areatot$area ~ areatot$date, pch = 16, xlab = "", ylab = 'area (ha)', main = "surface total")
# lines(areatot$area ~ areatot$date)
# abline(h = mean(areatot$area), col = 'red')
#
# # plot surface
# ggplot(data = area, aes(x = date, y = area, group = comment1, col = comment1))+
#   geom_line()
