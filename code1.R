###############################################################
# calculate RDI
###############################################################

forestStands$RDI1 <- NA
forestStands$RDI2 <- NA
forestStands$RDI <- NA

# 1st sp  == beech
forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech', 'salem_beech_fir', 'salem_beech_spruce'), 'RDI1'] <- forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech', 'salem_beech_fir', 'salem_beech_spruce'), 'NHA_1'] /
                                                          exp(13.99 - 2.181 * log(forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech', 'salem_beech_fir', 'salem_beech_spruce'), 'DG_1']))
# 1st sp  == fir
forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_fir', 'salem_fir_spruce'), 'RDI1'] <- forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_fir', 'salem_fir_spruce'), 'NHA_1'] /
                                                          exp(13.076 - 1.862 * log(forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_fir', 'salem_fir_spruce'), 'DG_1']))
# 1st sp  == spruce
forestStands[forestStands$FOREST_TYPE_CODE == 'salem_spruce', 'RDI1'] <- forestStands[forestStands$FOREST_TYPE_CODE == 'salem_spruce', 'NHA_1'] /
                                                          exp(12.876 - 1.762 * log(forestStands[forestStands$FOREST_TYPE_CODE == 'salem_spruce', 'DG_1']))
# 1st sp = oak
forestStands[forestStands$FOREST_TYPE_CODE == 'salem_oak', 'RDI1'] <- forestStands[forestStands$FOREST_TYPE_CODE == 'salem_oak', 'NHA_1'] /
                                                          exp(13.096 - 2.008 * log(forestStands[forestStands$FOREST_TYPE_CODE == 'salem_oak', 'DG_1']))
# 2nd sp  == spruce
forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), 'RDI2'] <- forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), 'NHA_2'] /
                                                          exp(12.876 - 1.762 * log(forestStands[forestStands$FOREST_TYPE_CODE %in% c('salem_beech_spruce', 'salem_fir_spruce'), 'DG_2']))
# 2nd sp  == fir
forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', 'RDI2'] <- forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', 'NHA_2'] /
                                                          exp(13.076 - 1.862 * log(forestStands[forestStands$FOREST_TYPE_CODE == 'salem_beech_fir', 'DG_2']))
# RDI total
forestStands$RDI <- forestStands$RDI1 + forestStands$RDI2
forestStands[is.na(forestStands$RDI), 'RDI'] <- forestStands[is.na(forestStands$RDI), 'RDI1']

###############################################################
# calculate Dgtot
###############################################################

# Dgtot
forestStands$Dgtot <- NA
forestStands$Dgtot <- sqrt(( (forestStands$NHA_1 * forestStands$DG_1^2) + (forestStands$NHA_2 * forestStands$DG_2^2) ) / (forestStands$NHA_1 + forestStands$NHA_2))
forestStands[forestStands$DG_2 == -1, 'Dgtot'] <- forestStands[forestStands$DG_2 == -1, 'DG_1']

###############################################################
# assign to each plot probability of being a irregular stand
###############################################################

# create probability space to help visualise the joint proba distribution
# calculate range of Dg and N
minDg <- round(min(forestStands$Dgtot) - 1)
maxDg <- round(max(forestStands$Dgtot) + 1)
minRDI <- round(min(forestStands$RDI))
maxRDI <- round(max(forestStands$RDI) + 1)
seqDg <- seq(from = minDg, to = maxDg, by = (maxDg - minDg) / (100 - 1))
seqRDI <- seq(from = minRDI, to = maxRDI, by = (maxRDI - minRDI) / (100 - 1))
proba <- expand.grid(Dg = seqDg, RDI = seqRDI)

# assign probability distribution to Dg
muDg <- mean(forestStands$Dgtot)
sigmaDg <- sd(forestStands$Dgtot)
proba$probaDg <- (1/(sigmaDg*sqrt(2*pi))) * exp( (-(proba$Dg-muDg)^2 / (2*sigmaDg^2)) )
plot(proba$probaDg ~ proba$Dg)

# assign probability distribution to RDI
muRDI <- mean(forestStands$RDI)
sigmaRDI <- sd(forestStands$RDI)
proba$probaRDI <- (1/(sigmaRDI*sqrt(2*pi))) * exp( (-(proba$RDI-muRDI)^2 / (2*sigmaRDI^2)) )
plot(proba$probaRDI ~ proba$RDI)

# calculate joint probability distribution
proba$jointProba <- proba$probaDg * proba$probaRDI

# calculate joint proba for all stands
forestStands$probaDg <- (1/(sigmaDg*sqrt(2*pi))) * exp( (-(forestStands$Dgtot-muDg)^2 / (2*sigmaDg^2)) )
forestStands$probaRDI <- (1/(sigmaRDI*sqrt(2*pi))) * exp( (-(forestStands$RDI-muRDI)^2 / (2*sigmaRDI^2)) )
forestStands$jointProba <- forestStands$probaDg * forestStands$probaRDI

ggplot() +
geom_point(data = forestStands, aes(Dgtot, RDI), alpha = 1) +
geom_tile(data = proba, aes(Dg, RDI, fill = jointProba), alpha = 0.8) +
scale_fill_gradient2(low = "blue", mid = "green",  high = "red", midpoint = 0.00010)
# geom_point(data = forestStands, aes(Dgtot, RDI, col = jointProba)) +
# scale_colour_gradient2(low = "blue", mid = "green",  high = "red", midpoint = 0.004)
