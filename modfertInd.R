###############################################################
# import IFN points & their potential index
###############################################################

# load IFN points
source('Z:/Private/Calcul_Potentiels/Calcul_Potentiels_Purs.R')

# load a mask of the study area
pnr <- readOGR(dsn = "Z:/Private/PNR Bauges/Sans_Trou", layer = "parc_filled", encoding = "UTF-8", use_iconv = TRUE)
# plot(pnr)
# points(bd$xl93, bd$yl93, pch = 16, col = 'black')

# select points in the study area
ptIfn <- SpatialPointsDataFrame(bd[,c("xl93", "yl93")], data = data.frame(bd), proj4string = CRS(proj4string(pnr)))
baugesIfn <-over(ptIfn, pnr)
baugesIfn <- droplevels(baugesIfn[!is.na(baugesIfn$ID),])
bdBauges <- droplevels(bd[as.numeric(row.names(baugesIfn)),])
# points(bdBauges$xl93, bdBauges$yl93, pch = 16, col = 'black', cex = 2)

# assign geological data to each point
geol <- readOGR(dsn = "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST", layer = "geol", encoding = "UTF-8", use_iconv = TRUE)
# plot(geol, col = geol$CODE, border = geol$CODE, add = TRUE)
ifnGeol <- SpatialPointsDataFrame(bdBauges[,c("xl93", "yl93")], data = data.frame(bdBauges), proj4string = CRS(proj4string(pnr)))
ifnGeol <- intersect(ifnGeol, geol)
bdBauges <- ifnGeol@data

# harmonise colnames between bdBauges and forestPlots
# usefull for subsequent prediction of fertility index
colnames(bdBauges)[colnames(bdBauges) == "xl93"] <- "X"
colnames(bdBauges)[colnames(bdBauges) == "yl93"] <- "Y"
colnames(bdBauges)[colnames(bdBauges) == "pent2"] <- "slope"

###############################################################
# model quercus petraea (03) potential index
###############################################################

# known variables:   - rocheCalc      # unknown variables: - Intercept
#                    - GRECO D        #                    - tmin_12
#                                     #                    - CN_decor
#                                     #                    - de_7
#                                     #                    - swhc

modUnPa03 <- lm(unknownPart03 ~ X + greco + slope + alti , data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa03)

###############################################################
# model fagus sylvatica (09) potential index
###############################################################

# known variables:   - alti           # unknown variables: - Intercept
#                    - slope          #                    - C/N
#                    - GRECO B        #                    - swhc
#                                     #                    - solD

modUnPa09 <- lm(unknownPart09 ~ alti + X + expoEW + slope, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa09)

###############################################################
# model abies alba (61) potential index
###############################################################

# known variables:   - GRECO A        # unknown variables: - Intercept
#                    - GRECO H        #                    - ETP June
#                                     #                    - RUM 1st horizon
#                                     #                    - C/N

# create a data subset without NA (depends on the variables included in the model)
bdBauges61 <- bdBauges[!is.na(bdBauges$slope), ]

# model
modUnPa61 <- lm(unknownPart61 ~ alti + X + slope, data = bdBauges61)
summary(modUnPa61)

# variance of the known part
bdBauges61$knPa61 <- 0
bdBauges61[bdBauges61$greco =="H", "knPa61"] <- -11.56
varKnPa61 <- var(bdBauges61$knPa61)

# variance of the unknown part
bdBauges61$unPa61 <- modUnPa61$coef["(Intercept)"] + (modUnPa61$coef["alti"] * bdBauges61$alti) +
                                                (modUnPa61$coef["X"] * bdBauges61$X) +
                                                (modUnPa61$coef["slope"] * bdBauges61$slope)
varUnPa61 <- var(bdBauges61$unPa61)

# var(potentiel) = var(a) + var(b) + var(c) + 2*cov(a,b) + 2*cov(a,c) + 2*cov(b,c)
var(bdBauges61$potentiel_61)
varKnPa61 + varUnPa61 + 2 * cov(bdBauges61$knPa61, bdBauges61$unPa61) + var(residuals(modUnPa61)) + 2 * cov(residuals(modUnPa61), bdBauges61$unPa61) + 2 * cov(bdBauges61$knPa61, residuals(modUnPa61))

# global goodness of fit (pot61 = knPa + unPa + epsilon)
# ratio of var(residuals) / var(potentiel)
var(residuals(modUnPa61)) / var(bdBauges61$potentiel_61)

###############################################################
# model picea abies (62) potential index
###############################################################

# known variables:   - slope          # unknown variables: - Intercept
#                    - expoNS         #                    - tmin_2
#                    - rocheCalc      #                    - swhc_A
#                                     #                    - bhc_5
#                                     #                    - C/N

modUnPa62 <- lm(unknownPart62 ~ alti + X + Y + slope, data = bdBauges[!is.na(bdBauges$expoNS) & !is.na(bdBauges$expoEW),])
summary(modUnPa62)
