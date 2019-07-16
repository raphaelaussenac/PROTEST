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

classGeol$rocheCalc <- 0
classGeol[classGeol$Code_carbonate > 0, 'rocheCalc'] <- 1
classGeol$Code_carbonate <- as.factor(classGeol$Code_carbonate)
classGeol$Code_hydro <- as.factor(classGeol$Code_hydro)
classGeol$rocheCalc <- as.factor(classGeol$rocheCalc)
bdBauges <- merge(bdBauges, classGeol, by.x = 'gelNttn', by.y = 'NOTATION', all.x = TRUE)

###############################################################
# prediction function
###############################################################

predFert <- function(mod, modData, sp, mode, method){
  # 1 - calculate the known part
  if (method == "direct"){
    modData$knPa <- 0
  } else if (method == "knPa-unPa"){
    if (sp == '03'){
      modData$knPa <- 0
      modData[modData$rocheCalc == 1, "knPa"] <- -11.097
    } else if (sp == '09'){
      modData$knPa <- -0.0083 * modData$alti + -0.086 * modData$slope
    } else if (sp == '61'){
      modData$knPa <- 0
      modData[modData$greco =="H", "knPa"] <- -11.56
    } else if (sp == '62'){
      modData$knPa <- 0
      modData[modData$rocheCalc == 1, "knPa"] <- -8.100
      modData$continuousknPa2 <- -0.255 * modData$slope + -3.705 * modData$expoNS
      modData$knPa <- modData$knPa + modData$continuousknPa2
    }
  }
  # 2 - calculate the unknow part
  modData$unPa <- predict(mod, newdata = modData)
  # 3 - calculate the predicted fertility index (by adding both parts)
  modData$pot <- modData$unPa + modData$knPa
  # 4 - calculate the potential index with a random variation
  if (mode == 'calibration'){
    modData$potEpsilon <- modData$unPa + modData$knPa + residuals(mod)
  } else if (mode == 'prediction'){
    modData$potEpsilon <- modData$unPa + modData$knPa + rnorm(nrow(modData), 0, sd(residuals(mod)))
  }
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
  colnames(modData)[colnames(modData) == paste('pot', sp, sep = "")] <- 'pot'
  colnames(modData)[colnames(modData) == paste('pot', sp, 'Epsilon', sep = "")] <- 'potEpsilon'
  # potentiel variance
  varPtnt <- var(modData$ptnt)
  # residuals variance
  varEpsi <- var(residuals(mod))
  # global goodness of fit
  # ratio of var(residuals) / var(potentiel)
  ratEpsiPot <- varEpsi / varPtnt
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
  varPar <- data.frame(round(c(varPtnt, varKnPa, varUnPa, varEpsi, covKnUn, covKnEpsi, covUnEpsi, ratEpsiPot, sumVarCov), 5))
  return(varPar)
}

###############################################################
# model quercus petraea (03) potential index
###############################################################

# known variables:   - GRECO D        # unknown variables: - Intercept
#                    - rocheCalc      #                    - tmin_12
#                                     #                    - CN_decor
#                                     #                    - de_7
#                                     #                    - swhc

# create a data subset without NA (depends on the variables included in the model)
bdBauges03 <- bdBauges

# model
if (chosenMethod == 'direct'){
  modUnPa03 <- lm(ptnt_03 ~ alti + rum + ph + greco + rocheCalc + Code_hydro, data = bdBauges03) #
} else if (chosenMethod == 'knPa-unPa'){
  modUnPa03 <- lm(unknP03 ~ alti + rum + I(rum^2) + Code_carbonate + Code_hydro, data = bdBauges03)
}
summary(modUnPa03)

# prediction on the same data set (for obs vs pred comparison)
bdBauges03 <- predFert(modUnPa03, bdBauges03, "03", 'calibration', chosenMethod)

# variance partition
varPar03 <- varPar(modUnPa03, bdBauges03, "03")

###############################################################
# model fagus sylvatica (09) potential index
###############################################################

# known variables:   - alti           # unknown variables: - Intercept
#                    - slope          #                    - C/N
#                    - GRECO B        #                    - swhc
#                                     #                    - sol carbonaté

# create a data subset without NA (depends on the variables included in the model)
bdBauges09 <- bdBauges[!is.na(bdBauges$ptnt_09), ]

# model
if (chosenMethod == 'direct'){
  modUnPa09 <- lm(ptnt_09 ~ alti + expoNS + slope + I(rum^2), data = bdBauges09) #
} else if (chosenMethod == 'knPa-unPa'){
  modUnPa09 <- lm(unknP09 ~ alti + rum + I(rum^2) + Code_carbonate + Code_hydro, data = bdBauges09)
}
summary(modUnPa09)

# prediction on the same data set (for obs vs pred comparison)
bdBauges09 <- predFert(modUnPa09, bdBauges09, "09", 'calibration', chosenMethod)

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
bdBauges61 <- bdBauges

# model
if (chosenMethod == 'direct'){
  modUnPa61 <- lm(ptnt_61 ~ alti + expoEW + rum + greco + rocheCalc, data = bdBauges61) #
} else if (chosenMethod == 'knPa-unPa'){
  modUnPa61 <- lm(unknP61 ~ alti + rum + I(rum^2) + Code_carbonate + Code_hydro, data = bdBauges61)
}
summary(modUnPa61)

# prediction on the same data set (for obs vs pred comparison)
bdBauges61 <- predFert(modUnPa61, bdBauges61, "61", 'calibration', chosenMethod)

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
bdBauges62 <- bdBauges[!is.na(bdBauges$ptnt_62), ]

# model
if (chosenMethod == 'direct'){
  modUnPa62 <- lm(ptnt_62 ~ expoEW + slope + rum + greco + rocheCalc + I(slope^2), data = bdBauges62) #
} else if (chosenMethod == 'knPa-unPa'){
  modUnPa62 <- lm(unknP62 ~ alti + rum + I(rum^2) + Code_carbonate + Code_hydro, data = bdBauges62)
}
summary(modUnPa62)

# prediction on the same data set (for obs vs pred comparison)
bdBauges62 <- predFert(modUnPa62, bdBauges62, "62", 'calibration', chosenMethod)

# variance partition
varPar62 <- varPar(modUnPa62, bdBauges62, "62")

###############################################################
# save variance partition into a df
###############################################################

varPar <- cbind(varPar03, varPar09, varPar61, varPar62)
colnames(varPar) <- c("Q. petraea", "F. sylvatica", "A. alba", "P. abies")
varPar <- t(varPar)
colnames(varPar) <- c("varPtnt", "varKnPa", "varUnPa", "varEpsi", "covKnUn", "covKnEpsi", "covUnEpsi", "ratEpsiPot", "sumVarCov")

write.csv(varPar, file = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/varPar.csv")
