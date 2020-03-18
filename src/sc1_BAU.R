# load management function function
source('./src/management.R')

###############################################################
# assign management type to each plot
###############################################################


# type <- 'fspPu'
# plotList <- firSprucePubList
# conservationThresh <- 0.02
# HarvThresh <- 0
# thinHarvThresh <- 0.60
# irrThresh <- 0.38


# fir + spruce + fir-spruce Public ---------------------------------------------
forestStands <- management(type = 'fspPu', plotList = firSprucePubList,
                        conservationThresh = 0.02, HarvThresh = 0,
                        thinHarvThresh = 0.60, irrThresh = 0.38)

# fir + spruce + fir-spruce Private --------------------------------------------
forestStands <- management(type = 'fspPr', plotList = firSprucePrivList,
                        conservationThresh = 0.27, HarvThresh = 0.15,
                        thinHarvThresh = 0.39, irrThresh = 0.19)

# beech Public + Private -------------------------------------------------------
forestStands <- management(type = 'beePP', plotList = beechList,
                        conservationThresh = 0.24, HarvThresh = 0.22,
                        thinHarvThresh = 0.21, irrThresh = 0.33)

# oak Public + Private ---------------------------------------------------------
forestStands <- management(type = 'oakPP', plotList = oakList,
                        conservationThresh = 0.57, HarvThresh = 0.43,
                        thinHarvThresh = 0, irrThresh = 0)

# beech-spruce + beech-fir Public ----------------------------------------------
forestStands <- management(type = 'bfsPu', plotList = beechFirSprucePubList,
                        conservationThresh = 0, HarvThresh = 0,
                        thinHarvThresh = 0.34, irrThresh = 0.66)

# beech-spruce + beech-fir Private ---------------------------------------------
forestStands <- management(type = 'bfsPr', plotList = beechFirSprucePrivList,
                        conservationThresh = 0.29, HarvThresh = 0,
                        thinHarvThresh = 0.14, irrThresh = 0.57)

###############################################################
# verification
###############################################################

# unique(forestStands$COMMENT)
# table(forestStands$COMMENT)
#
#
# typeArea <- ddply(forestStands, .(COMMENT), summarise, AREA = sum(AREA / 10000))
#
#
# # fir + spruce + fir-spruce Public ---------------------------------------------
# firSprucePubArea <- sum(forestStands[forestStands$STAND_ID %in% firSprucePubList, 'AREA']) / 10000
# cons <- (typeArea[typeArea$COMMENT == "fsPubCons", "AREA"] * 100 / firSprucePubArea) - 24.09
# cons
# harv <- (typeArea[typeArea$COMMENT == "fsPubHarv", "AREA"] * 100 / firSprucePubArea) - 0
# harv
# thinHarv <- (typeArea[typeArea$COMMENT == "fsPubThinHarv", "AREA"] * 100 / firSprucePubArea) - 46.47
# thinHarv
# irr <- (typeArea[typeArea$COMMENT == "fsPubIrr", "AREA"] * 100 / firSprucePubArea) - 29.43
# irr
#
# # fir + spruce + fir-spruce Private --------------------------------------------
# firSprucePrivArea <- sum(forestStands[forestStands$STAND_ID %in% firSprucePrivList, 'AREA']) / 10000
# cons <- (typeArea[typeArea$COMMENT == "fsPrivCons", "AREA"] * 100 / firSprucePrivArea) - 28.19
# cons
# harv <- (typeArea[typeArea$COMMENT == "fsPrivHarv", "AREA"] * 100 / firSprucePrivArea) - 14.75
# harv
# thinHarv <- (typeArea[typeArea$COMMENT == "fsPrivThinHarv", "AREA"] * 100 / firSprucePrivArea) - 38.36
# thinHarv
# irr <- (typeArea[typeArea$COMMENT == "fsPrivIrr", "AREA"] * 100 / firSprucePrivArea) - 18.69
# irr

# # beech ------------------------------------------------------------------------
# beechArea <- sum(forestStands[forestStands$STAND_ID %in% beechList, 'AREA']) / 10000
# cons <- (typeArea[typeArea$COMMENT == "bCons", "AREA"] * 100 / beechArea) - 77  # 24 --> 77
# cons
# harv <- (typeArea[typeArea$COMMENT == "bHarv", "AREA"] * 100 / beechArea) - 6.67 # 22
# harv
# thinHarv <- (typeArea[typeArea$COMMENT == "bThinHarv", "AREA"] * 100 / beechArea) - 6.37 # 21
# thinHarv
# irr <- (typeArea[typeArea$COMMENT == "bIrr", "AREA"] * 100 / beechArea) - 10 # 33
# irr
#
# # oak --------------------------------------------------------------------------
# oakArea <- sum(forestStands[forestStands$STAND_ID %in% oakList, 'AREA']) / 10000
# cons <- (typeArea[typeArea$COMMENT == "oCons", "AREA"] * 100 / oakArea) - 57
# cons
# harv <- (typeArea[typeArea$COMMENT == "oHarv", "AREA"] * 100 / oakArea) - 43
# harv
# thinHarv <- (typeArea[typeArea$COMMENT == "oThinHarv", "AREA"] * 100 / oakArea) - 0
# thinHarv
# irr <- (typeArea[typeArea$COMMENT == "oIrr", "AREA"] * 100 / oakArea) - 0
# irr
#
# # beech-spruce + beech-fir Public ----------------------------------------------
# beechFirSprucePubArea <- sum(forestStands[forestStands$STAND_ID %in% beechFirSprucePubList, 'AREA']) / 10000
# cons <- (typeArea[typeArea$COMMENT == "bfsPubCons", "AREA"] * 100 / beechFirSprucePubArea) - 0
# cons
# harv <- (typeArea[typeArea$COMMENT == "bfsPubHarv", "AREA"] * 100 / beechFirSprucePubArea) - 0
# harv
# thinHarv <- (typeArea[typeArea$COMMENT == "bfsPubThinHarv", "AREA"] * 100 / beechFirSprucePubArea) - 34
# thinHarv
# irr <- (typeArea[typeArea$COMMENT == "bfsPubIrr", "AREA"] * 100 / beechFirSprucePubArea) - 66
# irr
#
# # beech-spruce + beech-fir Public ----------------------------------------------
# beechFirSprucePrivArea <- sum(forestStands[forestStands$STAND_ID %in% beechFirSprucePrivList, 'AREA']) / 10000
# cons <- (typeArea[typeArea$COMMENT == "bfsPrivCons", "AREA"] * 100 / beechFirSprucePrivArea) - 29
# cons
# harv <- (typeArea[typeArea$COMMENT == "bfsPrivHarv", "AREA"] * 100 / beechFirSprucePrivArea) - 0
# harv
# thinHarv <- (typeArea[typeArea$COMMENT == "bfsPrivThinHarv", "AREA"] * 100 / beechFirSprucePrivArea) - 14
# thinHarv
# irr <- (typeArea[typeArea$COMMENT == "bfsPrivIrr", "AREA"] * 100 / beechFirSprucePrivArea) - 57
# irr
