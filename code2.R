
# verification
ggplot() +
# geom_point(data = forestStands, aes(Dgtot, RDI), alpha = 1) +
# geom_tile(data = proba, aes(Dg, RDI, fill = jointProba), alpha = 0.8) +
# scale_fill_gradient2(low = "blue", mid = "green",  high = "red", midpoint = 0.004) +
geom_point(data = forestStands[substr(forestStands$COMMENT, 1, 3) %in% 'Con', ], aes(Dgtot, RDI), col = 'green') +
geom_point(data = forestStands[!(substr(forestStands$COMMENT, 1, 3) %in% c('Irr', 'Con')), ], aes(Dgtot, RDI), col = 'blue') +
geom_point(data = forestStands[substr(forestStands$COMMENT, 1, 3) == 'Irr', ], aes(Dgtot, RDI), col = 'red')


###############################################################
# define RDI classes for each management type
###############################################################

# stands (with mean site index and mean Dg) usually grow of 0.05 RDI (average of
# fir, spruce and beech = 0.047) every 3 years
# stands with RDI < 0.65 will be included in the management '0.4<RDI<0.6'
# so that logging is performed the first year (and so on for other management)

# Irr
hist(forestStands[substr(forestStands$COMMENT, 1, 3) == 'Irr', 'RDI'])
substr(forestStands[substr(forestStands$COMMENT, 1, 3) == 'Irr' & forestStands$RDI < 0.65, 'COMMENT'], 1, 3) <- 'Ir1'
substr(forestStands[substr(forestStands$COMMENT, 1, 3) == 'Irr' & forestStands$RDI >= 0.65 & forestStands$RDI < 0.85, 'COMMENT'], 1, 3) <- 'Ir2'
substr(forestStands[substr(forestStands$COMMENT, 1, 3) == 'Irr' & forestStands$RDI >= 0.85, 'COMMENT'], 1, 3) <- 'Ir3'

hist(forestStands[substr(forestStands$COMMENT, 1, 3) == 'Thi', 'RDI'])
substr(forestStands[substr(forestStands$COMMENT, 1, 3) == 'Thi' & forestStands$RDI < 0.70, 'COMMENT'], 1, 3) <- 'Th1'
substr(forestStands[substr(forestStands$COMMENT, 1, 3) == 'Thi' & forestStands$RDI >= 0.70, 'COMMENT'], 1, 3) <- 'Th2'
