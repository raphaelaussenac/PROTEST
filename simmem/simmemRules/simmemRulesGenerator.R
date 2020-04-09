###############################################################
# retrieve list of plots for each management type
###############################################################

# create list of CON plots
con <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Con", "STAND_ID"]
con <- paste('        <int>', con, '</int>', sep = '')

# create list of HAR plots
har <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Har", "STAND_ID"]
har <- paste('        <int>', har, '</int>', sep = '')

# create list of THI Pu plots
th1Pu <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Th1" & substr(forestStands$COMMENT, 7, 8) == "Pu", "STAND_ID"]
th1Pu <- paste('        <int>', th1Pu, '</int>', sep = '')

th2Pu <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Th2" & substr(forestStands$COMMENT, 7, 8) == "Pu", "STAND_ID"]
th2Pu <- paste('        <int>', th2Pu, '</int>', sep = '')

# create list of THI Pr & PP plots
th1PrPP <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Th1" & substr(forestStands$COMMENT, 7, 8) != "Pu", "STAND_ID"]
th1PrPP <- paste('        <int>', th1PrPP, '</int>', sep = '')

th2PrPP <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Th2" & substr(forestStands$COMMENT, 7, 8) != "Pu", "STAND_ID"]
th2PrPP <- paste('        <int>', th2PrPP, '</int>', sep = '')

# create list of Irr plots
ir1 <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Ir1", "STAND_ID"]
ir1 <- paste('        <int>', ir1, '</int>', sep = '')

ir2 <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Ir2", "STAND_ID"]
ir2 <- paste('        <int>', ir2, '</int>', sep = '')

ir3 <- forestStands[substr(forestStands$COMMENT, 1, 3) == "Ir3", "STAND_ID"]
ir3 <- paste('        <int>', ir3, '</int>', sep = '')

###############################################################
# assemble java commande lines and plot lists
###############################################################

# read text file
sc1 <- readLines("./simmem/simmemRules/1scBAU")
sc2 <- readLines("./simmem/simmemRules/2scBAU")
sc3 <- readLines("./simmem/simmemRules/3scBAU")
sc4 <- readLines("./simmem/simmemRules/4scBAU")
sc4bis <- readLines("./simmem/simmemRules/4bisscBAU")
sc5 <- readLines("./simmem/simmemRules/5scBAU")
sc5bis <- readLines("./simmem/simmemRules/5bisscBAU")
sc6 <- readLines("./simmem/simmemRules/6scBAU")
sc7 <- readLines("./simmem/simmemRules/7scBAU")
sc8 <- readLines("./simmem/simmemRules/8scBAU")

###############################################################
# save scenario
###############################################################

writeLines(c(sc1, con, sc2, har, sc3, th1Pu, sc4, th2Pu, sc4bis, th1PrPP, sc5, th2PrPP, sc5bis, ir1, sc6, ir2, sc7, ir3, sc8), con = "./output/BAU")
