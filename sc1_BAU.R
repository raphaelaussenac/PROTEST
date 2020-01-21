# load management function function
source('C:/Users/raphael.aussenac/Documents/GitHub/PROTEST/management.R')

###############################################################
# assign management type to each plot
###############################################################

# fir + spruce + fir-spruce Public ---------------------------------------------
forestPlots <- management(type = 'fsPub', plotList = firSprucePubList,
                        conservationThresh = 0.02, HarvThresh = 0,
                        thinHarvThresh = 0.60, irrThresh = 0.38)

# fir + spruce + fir-spruce Private --------------------------------------------
forestPlots <- management(type = 'fsPriv', plotList = firSprucePrivList,
                        conservationThresh = 0.27, HarvThresh = 0.15,
                        thinHarvThresh = 0.39, irrThresh = 0.19)

# beech Public + Private -------------------------------------------------------
forestPlots <- management(type = 'b', plotList = beechList,
                        conservationThresh = 0.24, HarvThresh = 0.22,
                        thinHarvThresh = 0.21, irrThresh = 0.33)

# oak Public + Private ---------------------------------------------------------
forestPlots <- management(type = 'o', plotList = oakList,
                        conservationThresh = 0.57, HarvThresh = 0.43,
                        thinHarvThresh = 0, irrThresh = 0)

# beech-spruce + beech-fir Public ----------------------------------------------
forestPlots <- management(type = 'bfsPub', plotList = beechFirSprucePubList,
                        conservationThresh = 0, HarvThresh = 0,
                        thinHarvThresh = 0.34, irrThresh = 0.66)

# beech-spruce + beech-fir Private ---------------------------------------------
forestPlots <- management(type = 'bfsPriv', plotList = beechFirSprucePrivList,
                        conservationThresh = 0.29, HarvThresh = 0,
                        thinHarvThresh = 0.14, irrThresh = 0.57)

###############################################################
# verification
###############################################################

unique(forestPlots$COMMENT)
table(forestPlots$COMMENT)


typeArea <- ddply(forestPlots, .(COMMENT), summarise, AREA = sum(AREA / 10000))


# fir + spruce + fir-spruce Public ---------------------------------------------
firSprucePubArea <- sum(forestPlots[forestPlots$STAND_ID %in% firSprucePubList, 'AREA']) / 10000
cons <- (typeArea[typeArea$COMMENT == "fsPubCons", "AREA"] * 100 / firSprucePubArea) - 2
cons
harv <- (typeArea[typeArea$COMMENT == "fsPubHarv", "AREA"] * 100 / firSprucePubArea) - 0
harv
thinHarv <- (typeArea[typeArea$COMMENT == "fsPubThinHarv", "AREA"] * 100 / firSprucePubArea) - 60
thinHarv
irr <- (typeArea[typeArea$COMMENT == "fsPubIrr", "AREA"] * 100 / firSprucePubArea) - 38
irr

# fir + spruce + fir-spruce Private --------------------------------------------
firSprucePrivArea <- sum(forestPlots[forestPlots$STAND_ID %in% firSprucePrivList, 'AREA']) / 10000
cons <- (typeArea[typeArea$COMMENT == "fsPrivCons", "AREA"] * 100 / firSprucePrivArea) - 27
cons
harv <- (typeArea[typeArea$COMMENT == "fsPrivHarv", "AREA"] * 100 / firSprucePrivArea) - 15
harv
thinHarv <- (typeArea[typeArea$COMMENT == "fsPrivThinHarv", "AREA"] * 100 / firSprucePrivArea) - 39
thinHarv
irr <- (typeArea[typeArea$COMMENT == "fsPrivIrr", "AREA"] * 100 / firSprucePrivArea) - 19
irr

# beech ------------------------------------------------------------------------
beechArea <- sum(forestPlots[forestPlots$STAND_ID %in% beechList, 'AREA']) / 10000
cons <- (typeArea[typeArea$COMMENT == "bCons", "AREA"] * 100 / beechArea) - 24
cons
harv <- (typeArea[typeArea$COMMENT == "bHarv", "AREA"] * 100 / beechArea) - 22
harv
thinHarv <- (typeArea[typeArea$COMMENT == "bThinHarv", "AREA"] * 100 / beechArea) - 21
thinHarv
irr <- (typeArea[typeArea$COMMENT == "bIrr", "AREA"] * 100 / beechArea) - 33
irr

# oak --------------------------------------------------------------------------
oakArea <- sum(forestPlots[forestPlots$STAND_ID %in% oakList, 'AREA']) / 10000
cons <- (typeArea[typeArea$COMMENT == "oCons", "AREA"] * 100 / oakArea) - 57
cons
harv <- (typeArea[typeArea$COMMENT == "oHarv", "AREA"] * 100 / oakArea) - 43
harv
thinHarv <- (typeArea[typeArea$COMMENT == "oThinHarv", "AREA"] * 100 / oakArea) - 0
thinHarv
irr <- (typeArea[typeArea$COMMENT == "oIrr", "AREA"] * 100 / oakArea) - 0
irr

# beech-spruce + beech-fir Public ----------------------------------------------
beechFirSprucePubArea <- sum(forestPlots[forestPlots$STAND_ID %in% beechFirSprucePubList, 'AREA']) / 10000
cons <- (typeArea[typeArea$COMMENT == "bfsPubCons", "AREA"] * 100 / beechFirSprucePubArea) - 0
cons
harv <- (typeArea[typeArea$COMMENT == "bfsPubHarv", "AREA"] * 100 / beechFirSprucePubArea) - 0
harv
thinHarv <- (typeArea[typeArea$COMMENT == "bfsPubThinHarv", "AREA"] * 100 / beechFirSprucePubArea) - 34
thinHarv
irr <- (typeArea[typeArea$COMMENT == "bfsPubIrr", "AREA"] * 100 / beechFirSprucePubArea) - 66
irr

# beech-spruce + beech-fir Public ----------------------------------------------
beechFirSprucePrivArea <- sum(forestPlots[forestPlots$STAND_ID %in% beechFirSprucePrivList, 'AREA']) / 10000
cons <- (typeArea[typeArea$COMMENT == "bfsPrivCons", "AREA"] * 100 / beechFirSprucePrivArea) - 29
cons
harv <- (typeArea[typeArea$COMMENT == "bfsPrivHarv", "AREA"] * 100 / beechFirSprucePrivArea) - 0
harv
thinHarv <- (typeArea[typeArea$COMMENT == "bfsPrivThinHarv", "AREA"] * 100 / beechFirSprucePrivArea) - 14
thinHarv
irr <- (typeArea[typeArea$COMMENT == "bfsPrivIrr", "AREA"] * 100 / beechFirSprucePrivArea) - 57
irr
