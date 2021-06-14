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

dummy <- "XXX"
# read text file
# start list conservation
sc1 <- readLines(paste0("./simmem/simmemRules/1sc", dummy))
#
# conservation parameter + start list harvest
# sc2 <- readLines("./simmem/simmemRules/2scBAU")
sc2 <- readLines(paste0("./simmem/simmemRules/2sc", dummy))
#
# harvest parameter + start list th1Pu
# sc3 <- readLines("./simmem/simmemRules/3scBAU")
sc3 <- readLines(paste0("./simmem/simmemRules/3sc", dummy))
# ADA: PIAB and ABAL ddom from 40 to 30
if (scenario == "ADA") sc3 <- readLines(paste0("./simmem/simmemRules/3sc", scenario))
#
# th1Pu parameter + start list th2Pu
# sc4 <- readLines("./simmem/simmemRules/4scBAU")
sc4 <- readLines(paste0("./simmem/simmemRules/4sc", dummy))
#
# th2Pu parameter + start list th1PrPP
# sc4bis <- readLines("./simmem/simmemRules/4bisscBAU")
sc4bis <- readLines(paste0("./simmem/simmemRules/4bissc", dummy))
#
# th1PrPP parameter + start list th2PrPP
# sc5 <- readLines("./simmem/simmemRules/5scBAU")
sc5 <- readLines(paste0("./simmem/simmemRules/5sc", dummy))
#
# th2PrPP parameter + start list ir1
# sc5bis <- readLines("./simmem/simmemRules/5bisscBAU")
sc5bis <- readLines(paste0("./simmem/simmemRules/5bissc", dummy))
#
# ir1 parameter + start list ir2
# sc6 <- readLines("./simmem/simmemRules/6scBAU")
sc6 <- readLines(paste0("./simmem/simmemRules/6sc", dummy))
#
# ir2 parameter + start list ir3
# sc7 <- readLines("./simmem/simmemRules/7scBAU")
sc7 <- readLines(paste0("./simmem/simmemRules/7sc", dummy))
#
# ir3 parameter
# sc8 <- readLines("./simmem/simmemRules/8scBAU")
sc8 <- readLines(paste0("./simmem/simmemRules/8sc", dummy))

###############################################################
# save scenario
###############################################################

writeLines(c(sc1, con, sc2, har, sc3, th1Pu, sc4, th2Pu, sc4bis, th1PrPP, sc5, th2PrPP, sc5bis, ir1, sc6, ir2, sc7, ir3, sc8), con = paste0("./output/",scenario))
