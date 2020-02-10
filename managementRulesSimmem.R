###############################################################
# retrieve list of plots for each management type
###############################################################

# create list of CON plots
con <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Con", "STAND_ID"]
con <- paste('        <int>', con, '</int>', sep = '')

# create list of HAR plots
har <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Har", "STAND_ID"]
har <- paste('        <int>', har, '</int>', sep = '')

# create list of THI Pu plots
thiPu <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Thi" & substr(forestPlots$COMMENT, 7, 8) == "Pu", "STAND_ID"]
thiPu <- paste('        <int>', thiPu, '</int>', sep = '')

# create list of THI Pr & PP plots
thiPrPP <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Thi" & substr(forestPlots$COMMENT, 7, 8) != "Pu", "STAND_ID"]
thiPrPP <- paste('        <int>', thiPrPP, '</int>', sep = '')

# create list of CON plots
irr <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Irr", "STAND_ID"]
irr <- paste('        <int>', irr, '</int>', sep = '')

###############################################################
# assemble java commande lines and plot lists
###############################################################

# read text file
sc1 <- readLines("./managementScenario/1scBAU")
sc2 <- readLines("./managementScenario/2scBAU")
sc3 <- readLines("./managementScenario/3scBAU")
sc4 <- readLines("./managementScenario/4scBAU")
sc5 <- readLines("./managementScenario/5scBAU")
sc6 <- readLines("./managementScenario/6scBAU")


###############################################################
# save scenario
###############################################################

writeLines(c(sc1, con, sc2, har, sc3, thiPu, sc4, thiPrPP, sc5, irr, sc6), con = "./managementScenario/BAU")
