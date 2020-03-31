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

# create list of Irr plots
ir1 <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Ir1", "STAND_ID"]
ir1 <- paste('        <int>', ir1, '</int>', sep = '')

ir2 <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Ir2", "STAND_ID"]
ir2 <- paste('        <int>', ir2, '</int>', sep = '')

ir3 <- forestPlots[substr(forestPlots$COMMENT, 1, 3) == "Ir3", "STAND_ID"]
ir3 <- paste('        <int>', ir3, '</int>', sep = '')

###############################################################
# assemble java commande lines and plot lists
###############################################################

# read text file
sc1 <- readLines("./simmem/simmemRules/1scBAU")
sc2 <- readLines("./simmem/simmemRules/2scBAU")
sc3 <- readLines("./simmem/simmemRules/3scBAU")
sc4 <- readLines("./simmem/simmemRules/4scBAU")
sc5 <- readLines("./simmem/simmemRules/5scBAU")
sc6 <- readLines("./simmem/simmemRules/6scBAU")
sc7 <- readLines("./simmem/simmemRules/7scBAU")
sc8 <- readLines("./simmem/simmemRules/8scBAU")

###############################################################
# save scenario
###############################################################

writeLines(c(sc1, con, sc2, har, sc3, thiPu, sc4, thiPrPP, sc5, ir1, sc6, ir2, sc7, ir3, sc8), con = "./output/BAU")
