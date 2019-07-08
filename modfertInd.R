library(rgdal)
library(raster)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/PROTEST")

###############################################################
# import IFN points (their potential index & associated
# data (slope, expoNS...)
###############################################################

ifnCircular <- readOGR(dsn = ".", layer = "ifnCircular", encoding = "UTF-8", use_iconv = TRUE)
bdBauges <- ifnCircular@data

# add a column to plot ifn points (and set the point shape)
bdBauges$ifnPt <- "NFI points"

###############################################################
# load geol classification
###############################################################

classGeol <- read.csv("classificationGeol.csv", header = TRUE, sep = ";")
classGeol <- classGeol[, c('NOTATION', 'Code_carbonate', 'Code_hydro')]
bdBauges <- merge(bdBauges, classGeol, by.x = 'gelNttn', by.y = 'NOTATION', all.x = TRUE)

###############################################################
# prediction function
###############################################################

predFert <- function(mod, modData, sp){
  # 1 - calculate the known part
  if (sp == '03'){
    modData$knPa <- 0
  } else if (sp == '09'){
    modData$knPa <- -0.0083 * modData$alti + -0.086 * modData$slope
  } else if (sp == '61'){
    modData$knPa <- 0
    modData[modData$greco =="H", "knPa"] <- -11.56
  } else if (sp == '62'){
    modData$knPa <- -0.255 * modData$slope + -3.705 * modData$expoNS
  }
  # 2 - calculate the unknow part
  modData$unPa <- predict(mod, newdata = modData)
  # 3 - calculate the predicted fertility index (by adding both parts)
  modData$pot <- modData$unPa + modData$knPa
  # 4 - calculate the potential index with a random variation
  modData$potEpsilon <- modData$unPa + modData$knPa + rnorm(nrow(modData), 0, sd(residuals(mod)))
  # 5 - add sp to colnames
  colnames(modData)[colnames(modData) == "knPa"] <- paste("knPa", sp, sep="")
  colnames(modData)[colnames(modData) == "unPa"] <- paste("unPa", sp, sep="")
  colnames(modData)[colnames(modData) == "pot"] <- paste("pot", sp, sep="")
  colnames(modData)[colnames(modData) == "potEpsilon"] <- paste("pot", sp, "Epsilon", sep="")
  return(modData)
}

###############################################################
# variance partition function
###############################################################

varPar <- function(mod, modData, sp){
  # retrieve colnames without sp code
  colnames(modData)[colnames(modData) == paste('knPa', sp, sep = "")] <- 'knPa'
  colnames(modData)[colnames(modData) == paste('unPa', sp, sep = "")] <- 'unPa'
  colnames(modData)[colnames(modData) == paste('ptnt_', sp, sep = "")] <- 'ptnt'
  # potentiel variance
  varPot <- var(modData$ptnt)
  # residuals variance
  varEpsi <- var(residuals(mod))
  # global goodness of fit
  # ratio of var(residuals) / var(potentiel)
  ratEpsiPot <- varEpsi / varPot
  # variance of the known part
  varKnPa <- var(modData$knPa)
  # variance of the unknown part
  varUnPa <- var(modData$unPa)
  # covarances
  covKnUn <- 2 * cov(modData$knPa, modData$unPa)
  covKnEpsi <- 2 * cov(modData$knPa, residuals(mod))
  covUnEpsi <- 2 * cov(modData$unPa, residuals(mod))
  # verification
  # var(potentiel) = var(a) + var(b) + var(c) + 2*cov(a,b) + 2*cov(a,c) + 2*cov(b,c)
  sumVarCov <- varKnPa + varUnPa + varEpsi + covKnUn + covKnEpsi + covUnEpsi
  # save
  varPar <- data.frame(round(c(varPot, varKnPa, varUnPa, varEpsi, covKnUn, covKnEpsi, covUnEpsi, ratEpsiPot, sumVarCov), 5))
  return(varPar)
}

###############################################################
# model quercus petraea (03) potential index
###############################################################

# known variables:   - rocheCalc      # unknown variables: - Intercept
#                    - GRECO D        #                    - tmin_12
#                                     #                    - CN_decor
#                                     #                    - de_7
#                                     #                    - swhc

# create a data subset without NA (depends on the variables included in the model)
bdBauges03 <- bdBauges[!is.na(bdBauges$slope), ]

# model
modUnPa03 <- lm(unknP03 ~ X + greco + slope + alti +ph + expoEW + expoNS, data = bdBauges03)
summary(modUnPa03)

# prediction on the same data set (for obs vs pred comparison)
bdBauges03 <- predFert(modUnPa03, bdBauges03, "03")

# variance partition
varPar03 <- varPar(modUnPa03, bdBauges03, "03")
###############################################################
# model fagus sylvatica (09) potential index
###############################################################

# known variables:   - alti           # unknown variables: - Intercept
#                    - slope          #                    - C/N
#                    - GRECO B        #                    - swhc
#                                     #                    - solD

# create a data subset without NA (depends on the variables included in the model)
bdBauges09 <- bdBauges[!is.na(bdBauges$slope) & !is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW), ]

# model
modUnPa09 <- lm(unknP09 ~ alti + X + expoEW + slope + ph + expoNS + expoEW, data = bdBauges09)
summary(modUnPa09)

# prediction on the same data set (for obs vs pred comparison)
bdBauges09 <- predFert(modUnPa09, bdBauges09, "09")

# variance partition
varPar09 <- varPar(modUnPa09, bdBauges09, "09")
###############################################################
# model abies alba (61) potential index
###############################################################

# known variables:   - GRECO A        # unknown variables: - Intercept
#                    - GRECO H        #                    - ETP June
#                                     #                    - swhc_A
#                                     #                    - C/N

# create a data subset without NA (depends on the variables included in the model)
bdBauges61 <- bdBauges[!is.na(bdBauges$Code_carbonate), ]

# model
modUnPa61 <- lm(unknP61 ~ alti + X + Y + expoNS + expoEW + slope + rum + Code_carbonate + Code_hydro, data = bdBauges61) #
summary(modUnPa61)

# prediction on the same data set (for obs vs pred comparison)
bdBauges61 <- predFert(modUnPa61, bdBauges61, "61")

# variance partition
varPar61 <- varPar(modUnPa61, bdBauges61, "61")

###############################################################
# model picea abies (62) potential index
###############################################################

# known variables:   - slope          # unknown variables: - Intercept
#                    - expoNS         #                    - tmin_2
#                    - rocheCalc      #                    - swhc_A
#                                     #                    - bhc_5
#                                     #                    - C/N

# create a data subset without NA (depends on the variables included in the model)
bdBauges62 <- bdBauges[!is.na(bdBauges$slope), ]

# model
modUnPa62 <- lm(unknP62 ~ alti + X + Y + slope, data = bdBauges62)
summary(modUnPa62)

# prediction on the same data set (for obs vs pred comparison)
bdBauges62 <- predFert(modUnPa62, bdBauges62, "62")

# variance partition
varPar62 <- varPar(modUnPa62, bdBauges62, "62")

###############################################################
# save variance partition into a df
###############################################################

varPar <- cbind(varPar03, varPar09, varPar61, varPar62)
colnames(varPar) <- c("Q. petraea", "F. sylvatica", "A. alba", "P. abies")
varPar <- t(varPar)
colnames(varPar) <- c("varPot", "varKnPa", "varUnPa", "varEpsi", "covKnUn", "covKnEpsi", "covUnEpsi", "ratEpsiPot", "sumVarCov")

write.csv(varPar, file = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/varPar.csv")
