library(rgdal)
library(raster)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

###############################################################
# import IFN points (their potential index & associated
# data (slope, expoNS...)
###############################################################

ifnCircular <- readOGR(dsn = "./data", layer = "ifnCircular", encoding = "UTF-8", use_iconv = TRUE)
bdBauges <- ifnCircular@data

# add a column to plot ifn points (and set the point shape)
bdBauges$ifnPt <- "NFI points"

###############################################################
# load geol classification
###############################################################

classGeol <- read.csv("./data/classificationGeol.csv", header = TRUE, sep = ";")
classGeol <- classGeol[, c('NOTATION', 'Code_carbonate', 'Code_hydro')]
colnames(classGeol) <- c('NOTATION', 'Cd_crbn', 'Cd_hydr')

classGeol$rochClc <- 0
classGeol[classGeol$Cd_crbn > 0, 'rochClc'] <- 1
classGeol$Cd_crbn <- as.factor(classGeol$Cd_crbn)
classGeol$Cd_hydr <- as.factor(classGeol$Cd_hydr)
classGeol$rochClc <- as.factor(classGeol$rochClc)
bdBauges <- merge(bdBauges, classGeol, by.x = 'gelNttn', by.y = 'NOTATION', all.x = TRUE)

###############################################################
# prediction function
###############################################################

predFert <- function(mod, modData, sp){
  modData$pot <- predict(mod, newdata = modData, type = 'response')
  # add a random variation for gamma glms
  shape_estim  <- drop(MASS::gamma.shape(mod)[1]$alpha)
  modData$potEpsilon <- rgamma(nrow(modData), rate = shape_estim / modData$pot, shape = shape_estim)
  # add a random variation for lms
  # modData$potEpsilon <- modData$pot + rnorm(nrow(modData), 0, sd(residuals(mod)))
  # add sp to colnames
  colnames(modData)[colnames(modData) == "pot"] <- paste("pot", sp, sep="")
  colnames(modData)[colnames(modData) == "potEpsilon"] <- paste("pot", sp, "Epsilon", sep="")
  return(modData)
}

###############################################################
# model quercus petraea (03) potential index
###############################################################

# create a data subset without NA
bdBauges03 <- bdBauges

# complete model
# alti + slope + rum + ph + expoNS + expoEW +
#           I(greco == "H") +
#           I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") +
#           I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2")

# glm Gamma
mod03 <- glm(ptnt_03 ~ rum + expoNS + I(alti^2) +
          I(greco == "H") +
          I(Cd_hydr == "1") +
          I(Cd_crbn == "1") + I(Cd_crbn == "2"),
          family = Gamma(link = "identity"),
          data = bdBauges03)

# lm
# mod03 <- lm(ptnt_03 ~ slope + rum + I(alti^2) + I(ph^2) +
#             I(greco == "H") +
#             I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2"),
#             data = bdBauges03)

summary(mod03)

# prediction on the same data set (for obs vs pred comparison)
bdBauges03 <- predFert(mod03, bdBauges03, "03")

###############################################################
# model fagus sylvatica (09) potential index
###############################################################

# create a data subset without NA
bdBauges09 <- bdBauges[!is.na(bdBauges$ptnt_09), ]

# complete model
# alti + slope + rum + ph + expoNS + expoEW +
#           I(greco == "H") +
#           I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") +
#           I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2")

# glm Gamma
mod09 <- glm(ptnt_09 ~ alti + slope + expoNS + I(rum^2),
          family = Gamma(link = "identity"),
          data = bdBauges09)

# lm
# mod09 <- lm(ptnt_09 ~ alti + slope + expoNS + I(rum^2),
#             data = bdBauges09)

summary(mod09)

# prediction on the same data set (for obs vs pred comparison)
bdBauges09 <- predFert(mod09, bdBauges09, "09")

###############################################################
# model abies alba (61) potential index
###############################################################

# create a data subset without NA
bdBauges61 <- bdBauges

# complete model
# alti + slope + rum + ph + expoNS + expoEW +
#           I(greco == "H") +
#           I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") +
#           I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2")

# glm Gamma
mod61 <- glm(ptnt_61 ~ alti + expoNS + I(rum^2) +
          I(greco == "H") +
          I(Cd_crbn == "2"),
          family = Gamma(link = "identity"),
          data = bdBauges61)

# lm
# mod61 <- lm(ptnt_61 ~ alti + expoEW + I(rum^2) +
#             I(greco == "H") +
#             I(Cd_crbn == "0"),
#             data = bdBauges61)

summary(mod61)

# prediction on the same data set (for obs vs pred comparison)
bdBauges61 <- predFert(mod61, bdBauges61, "61")

###############################################################
# model picea abies (62) potential index
###############################################################

# create a data subset without NA
bdBauges62 <- bdBauges[!is.na(bdBauges$ptnt_62), ]

# complete model
# alti + slope + rum + ph + expoNS + expoEW +
#           I(greco == "H") +
#           I(Cd_hydr == "0") + I(Cd_hydr == "1") + I(Cd_hydr == "2") +
#           I(Cd_crbn == "0") + I(Cd_crbn == "1") + I(Cd_crbn == "2")

# glm Gamma
mod62 <- glm(ptnt_62 ~ alti + slope + I(slope^2) + I(rum^2) +
          I(Cd_hydr == "2"),
          family = Gamma(link = "identity"),
          data = bdBauges62)

# lm
# mod62 <- lm(ptnt_62 ~ slope + expoEW + I(slope^2) + I(rum^2) +
#             I(greco == "H") +
#             I(Cd_crbn == "0"),
#             data = bdBauges62)

summary(mod62)

# prediction on the same data set (for obs vs pred comparison)
bdBauges62 <- predFert(mod62, bdBauges62, "62")
