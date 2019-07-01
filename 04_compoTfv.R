###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
library(ggplot2)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

# load NFI points within the study area (Bauges)
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/modfertInd.R')

# load correspondence NFI point / code BDforet
correspond <- read.csv("Z:/Private/croisementIfn.csv", header = TRUE, sep = ";")

# load PROTEST points
protestPt <- load(file = "Z:/Private/protestPt.rda")

# load BDforet
forestPlots <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "forestPlots3Ha", encoding = "UTF-8", use_iconv = TRUE)

###############################################################
# retrieve code TFV for all NFI points within the study area
###############################################################

# select only NFI points within the study area
correspond <- correspond[correspond$idp %in% bdBauges$idp, ]

# assign codeTFVdist as TFV when TFV is missing
correspond[correspond$TFV == "", "TFV"] <- correspond[correspond$TFV == "", "codeTFVdist"]

###############################################################
# retrieve code TFV for all protest points within the study area
###############################################################

# convert placette.mes into a SpatialPointsDataFrame
xy <- placette.mes[, c('Xplac_recale', 'Yplac_recale')]
protestPlots <- SpatialPointsDataFrame(coords = xy, data = placette.mes, proj4string = crs(forestPlots))

# intersect
protestPlots <- intersect(protestPlots, forestPlots)

# plot
plot(forestPlots, col = forestPlots$CODE_TFV, border = forestPlots$CODE_TFV)
points(protestPlots, asp = 1, pch = 16, col = 'black')

###############################################################
# surface of each TFV types
###############################################################

# surface for each TFV type throughout the study area
surfaceTfv <- data.frame("tfv" = NA, "surface" = NA)
for (i in unique(forestPlots$CODE_TFV)){
  surf <- sum(area(forestPlots[forestPlots$CODE_TFV == i,]))
  aa <- c(i, surf)
  surfaceTfv <- rbind(surfaceTfv, aa)
}
surfaceTfv <- surfaceTfv[-1, ]
surfaceTfv$surface <- as.numeric(surfaceTfv$surface)
surfaceTfv$surface <- surfaceTfv$surface / 10000 # convert into hectars


# number of inventory points for each TFV type throughout the study area
protestPtSurfTfv <- data.frame(table(protestPlots$CODE_TFV))
protestPtSurfTfv <- merge(surfaceTfv, protestPtSurfTfv, by.x = "tfv", by.y = "Var1")
colnames(protestPtSurfTfv)[colnames(protestPtSurfTfv) == "Freq"] <- "points"
protestPtSurfTfv <- protestPtSurfTfv[order(protestPtSurfTfv$surface, decreasing = T),]

# plot
bp <- barplot(protestPtSurfTfv$surface, names.arg = protestPtSurfTfv$tfv, las = 2, ylim = c(-1000, 20000), main = "Surface (ha) and number of\ninventory points for each TFV type")
barplot(protestPtSurfTfv$surface, names.arg = protestPtSurfTfv$tfv, las = 2, ylim = c(-1000, 20000), main = "Surface (ha) and number of\ninventory points for each TFV type")
text(bp, protestPtSurfTfv$surface + 500, label = protestPtSurfTfv$points, cex = 1, col = 'red')
text(bp, protestPtSurfTfv$surface - 500, label = round(protestPtSurfTfv$surface), cex = 1, col = 'black')
legend("topright", legend = c("nb protest pt", 'surface'), fill = c("red", 'black'), col = c('red'))

###############################################################
# group TFV types
###############################################################




###############################################################
# species distribution within each TFV types
###############################################################

# https://inventaire-forestier.ign.fr/IMG/pdf/dc_bdforet_2-0.pdf
