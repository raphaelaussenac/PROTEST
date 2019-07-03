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
colnames(bdBauges)[colnames(bdBauges) == "xl93"] <- "X"
colnames(bdBauges)[colnames(bdBauges) == "yl93"] <- "Y"

# add a column to plot ifn points (and set the point shape)
bdBauges$ifnPt <- "NFI points"

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
modUnPa03 <- lm(unknP03 ~ X + greco + slope + alti , data = bdBauges03)
summary(modUnPa03)

############################## variance partition
# potentiel variance
varPot03 <- var(bdBauges03$ptnt_03)

# residuals variance
varEpsi03 <- var(residuals(modUnPa03))

# global goodness of fit
# ratio of var(residuals) / var(potentiel)
ratEpsiPot03 <- varEpsi03 / varPot03

# variance of the known part
bdBauges03$knPa03 <- 0
bdBauges03[bdBauges03$greco =="H", "knPa03"] <- -11.56
varKnPa03 <- var(bdBauges03$knPa03)

# variance of the unknown part
bdBauges03$unPa03 <- predict(modUnPa03, newdata = bdBauges03)
varUnPa03 <- var(bdBauges03$unPa03)

# covarances
covKnUn03 <- 2 * cov(bdBauges03$knPa03, bdBauges03$unPa03)
covKnEpsi03 <- 2 * cov(bdBauges03$knPa03, residuals(modUnPa03))
covUnEpsi03 <- 2 * cov(bdBauges03$unPa03, residuals(modUnPa03))

# verification
# var(potentiel) = var(a) + var(b) + var(c) + 2*cov(a,b) + 2*cov(a,c) + 2*cov(b,c)
sumVarCov03 <- varKnPa03 + varUnPa03 + varEpsi03 + covKnUn03 + covKnEpsi03 + covUnEpsi03

# save
varPar03 <- data.frame(round(c(varPot03, varKnPa03, varUnPa03, varEpsi03, covKnUn03, covKnEpsi03, covUnEpsi03, ratEpsiPot03, sumVarCov03), 5))

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
modUnPa09 <- lm(unknP09 ~ alti + X + expoEW + slope, data = bdBauges09)
summary(modUnPa09)

############################## variance partition
# potentiel variance
varPot09 <- var(bdBauges09$ptnt_09)

# residuals variance
varEpsi09 <- var(residuals(modUnPa09))

# global goodness of fit
# ratio of var(residuals) / var(potentiel)
ratEpsiPot09 <- varEpsi09 / varPot09

# variance of the known part
bdBauges09$knPa09 <- 0
bdBauges09[bdBauges09$greco =="H", "knPa09"] <- -11.56
varKnPa09 <- var(bdBauges09$knPa09)

# variance of the unknown part
bdBauges09$unPa09 <- predict(modUnPa09, newdata = bdBauges09)
varUnPa09 <- var(bdBauges09$unPa09)

# covarances
covKnUn09 <- 2 * cov(bdBauges09$knPa09, bdBauges09$unPa09)
covKnEpsi09 <- 2 * cov(bdBauges09$knPa09, residuals(modUnPa09))
covUnEpsi09 <- 2 * cov(bdBauges09$unPa09, residuals(modUnPa09))

# verification
# var(potentiel) = var(a) + var(b) + var(c) + 2*cov(a,b) + 2*cov(a,c) + 2*cov(b,c)
sumVarCov09 <- varKnPa09 + varUnPa09 + varEpsi09 + covKnUn09 + covKnEpsi09 + covUnEpsi09

# save
varPar09 <- data.frame(round(c(varPot09, varKnPa09, varUnPa09, varEpsi09, covKnUn09, covKnEpsi09, covUnEpsi09, ratEpsiPot09, sumVarCov09), 5))

###############################################################
# model abies alba (61) potential index
###############################################################

# known variables:   - GRECO A        # unknown variables: - Intercept
#                    - GRECO H        #                    - ETP June
#                                     #                    - swhc_A
#                                     #                    - C/N

# create a data subset without NA (depends on the variables included in the model)
bdBauges61 <- bdBauges[!is.na(bdBauges$slope), ]

# model
modUnPa61 <- lm(unknP61 ~ alti + X + slope + rum, data = bdBauges61) #
summary(modUnPa61)

############################## variance partition
# potentiel variance
varPot61 <- var(bdBauges61$ptnt_61)

# residuals variance
varEpsi61 <- var(residuals(modUnPa61))

# global goodness of fit
# ratio of var(residuals) / var(potentiel)
ratEpsiPot61 <- varEpsi61 / varPot61

# variance of the known part
bdBauges61$knPa61 <- 0
bdBauges61[bdBauges61$greco =="H", "knPa61"] <- -11.56
varKnPa61 <- var(bdBauges61$knPa61)

# variance of the unknown part
bdBauges61$unPa61 <- predict(modUnPa61, newdata = bdBauges61)
varUnPa61 <- var(bdBauges61$unPa61)

# covarances
covKnUn61 <- 2 * cov(bdBauges61$knPa61, bdBauges61$unPa61)
covKnEpsi61 <- 2 * cov(bdBauges61$knPa61, residuals(modUnPa61))
covUnEpsi61 <- 2 * cov(bdBauges61$unPa61, residuals(modUnPa61))

# verification
# var(potentiel) = var(a) + var(b) + var(c) + 2*cov(a,b) + 2*cov(a,c) + 2*cov(b,c)
sumVarCov61 <- varKnPa61 + varUnPa61 + varEpsi61 + covKnUn61 + covKnEpsi61 + covUnEpsi61

# save
varPar61 <- data.frame(round(c(varPot61, varKnPa61, varUnPa61, varEpsi61, covKnUn61, covKnEpsi61, covUnEpsi61, ratEpsiPot61, sumVarCov61), 5))

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

############################## variance partition
# potentiel variance
varPot62 <- var(bdBauges62$ptnt_62)

# residuals variance
varEpsi62 <- var(residuals(modUnPa62))

# global goodness of fit
# ratio of var(residuals) / var(potentiel)
ratEpsiPot62 <- varEpsi62 / varPot62

# variance of the known part
bdBauges62$knPa62 <- 0
bdBauges62[bdBauges62$greco =="H", "knPa62"] <- -11.56
varKnPa62 <- var(bdBauges62$knPa62)

# variance of the unknown part
bdBauges62$unPa62 <- predict(modUnPa62, newdata = bdBauges62)
varUnPa62 <- var(bdBauges62$unPa62)

# covarances
covKnUn62 <- 2 * cov(bdBauges62$knPa62, bdBauges62$unPa62)
covKnEpsi62 <- 2 * cov(bdBauges62$knPa62, residuals(modUnPa62))
covUnEpsi62 <- 2 * cov(bdBauges62$unPa62, residuals(modUnPa62))

# verification
# var(potentiel) = var(a) + var(b) + var(c) + 2*cov(a,b) + 2*cov(a,c) + 2*cov(b,c)
sumVarCov62 <- varKnPa62 + varUnPa62 + varEpsi62 + covKnUn62 + covKnEpsi62 + covUnEpsi62

# save
varPar62 <- data.frame(round(c(varPot62, varKnPa62, varUnPa62, varEpsi62, covKnUn62, covKnEpsi62, covUnEpsi62, ratEpsiPot62, sumVarCov62), 5))

###############################################################
# save variance partition into a df
###############################################################

varPar <- cbind(varPar03, varPar09, varPar61, varPar62)
colnames(varPar) <- c("Q. petraea", "F. sylvatica", "A. alba", "P. abies")
varPar <- t(varPar)
colnames(varPar) <- c("varPot", "varKnPa", "varUnPa", "varEpsi", "covKnUn", "covKnEpsi", "covUnEpsi", "ratEpsiPot", "sumVarCov")

write.csv(varPar, file = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/output/varPar.csv")
