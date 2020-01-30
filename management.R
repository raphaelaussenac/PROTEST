###############################################################
# plot random selection function
###############################################################

rdmSelect <- function(plotSubset, threshold, switchCons, plotCons){

  # if propInaccess > conservationThresh
  if(switchCons == 0){
    plots <- plotCons
  } else {
    # randomly pick first plot
    plots <- plotSubset[round(runif(1, min = 1, max = length(plotSubset)))]
  }
  remaining <- plotSubset[!(plotSubset %in% plots)]
  # add new plots till the threshold is reached
  while(sum(forestPlots[forestPlots$STAND_ID %in% plots, 'AREA']) < threshold && length(remaining) > 0){
    # randomly select 1 more plot
    addplot <- remaining[round(runif(1, min = 1, max = length(remaining)))]
    plots <- c(plots, addplot)
    # list of remaining plots
    remaining <- plotSubset[!(plotSubset %in% plots)]
    # print(length(remaining))
  }
  return(plots)
}

###############################################################
# assign management function
###############################################################

management <- function(type, plotList, conservationThresh, HarvThresh,
                                                thinHarvThresh, irrThresh){
  # calculate area
  area <- sum(forestPlots[forestPlots$STAND_ID %in% plotList, 'AREA'])

  # conservation ---------------------------------------------------------------
  plotCons <- c()

  # all non-harvestable plots are necessarily in conservation
  plotCons <- forestPlots[forestPlots$STAND_ID %in% plotList & forestPlots$nonHarv == 0, 'STAND_ID']

  # the proportion are only applied to the remaining plots
  # remaining plots proportion
  propRemaining <- (area - sum(forestPlots[forestPlots$STAND_ID %in% plotCons, 'AREA'])) / area
  # correct proportions
  conservationThresh <- propRemaining * conservationThresh
  HarvThresh <- propRemaining * HarvThresh
  thinHarvThresh <- propRemaining * thinHarvThresh
  irrThresh <- propRemaining * irrThresh

  # inaccessible plots are also classified in conservation
  plotCons <- c(plotCons, forestPlots[forestPlots$STAND_ID %in% plotList & forestPlots$nonHarv == 1 & forestPlots$dist == 0, 'STAND_ID'])
  propInaccess <- sum(forestPlots[forestPlots$STAND_ID %in% plotList & forestPlots$nonHarv == 1 & forestPlots$dist == 0, 'AREA']) / area

  # 2 possible cases:
  # propInaccess < conservationThresh
  switchCons <- 1
  if(propInaccess < conservationThresh){
    switchCons <- 0
    conservationThresh <- area * (conservationThresh + (1- propRemaining))
    plotCons <- rdmSelect(plotSubset = plotList, threshold = conservationThresh, switchCons = switchCons, plotCons = plotCons)
    forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- paste("Con", type, sep = "")
    switchCons <- 1
  }

  # propInaccess > conservationThresh
  if(propInaccess > conservationThresh){
    propRemaining2 <- (area - sum(forestPlots[forestPlots$STAND_ID %in% plotCons, 'AREA'])) / area
    # convert prortion of other types of management to "relative" proportion
    totalPropRemainingType <- HarvThresh + thinHarvThresh + irrThresh
    # calculate relative proportion of each type
    HarvThresh <- HarvThresh / totalPropRemainingType
    thinHarvThresh <- thinHarvThresh / totalPropRemainingType
    irrThresh <- irrThresh / totalPropRemainingType
    # apply relative proportion to remaining proportion 2
    HarvThresh <- propRemaining2 * HarvThresh
    thinHarvThresh <- propRemaining2 * thinHarvThresh
    irrThresh <- propRemaining2 * irrThresh
    forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- paste("Con", type, sep = "")
  }

  # final Harvest --------------------------------------------------------------
  plotHarv <- c()
  if (HarvThresh > 0){
    HarvThresh <- area * HarvThresh
    plotHarv <- rdmSelect(plotSubset = plotList[!(plotList %in% plotCons)], threshold = HarvThresh, switchCons = switchCons, plotCons = plotCons)
    forestPlots[forestPlots$STAND_ID %in% plotHarv, "COMMENT"] <- paste("Har", type, sep = "")
  }

  # thinning and harvest -------------------------------------------------------
  plotThinHarv <- c()
  if (thinHarvThresh > 0){
    thinHarvThresh <- area * thinHarvThresh
    plotThinHarv <- rdmSelect(plotSubset = plotList[!(plotList %in% c(plotCons, plotHarv))], threshold = thinHarvThresh, switchCons = switchCons, plotCons = plotCons)
    forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- paste("Thi", type, sep = "")
  }

  # irregular ------------------------------------------------------------------
  plotIrr <- c()
  if (irrThresh > 0){
    irrThresh <- area * irrThresh
    plotIrr <- rdmSelect(plotSubset = plotList[!(plotList %in% c(plotCons, plotHarv, plotThinHarv))], threshold = irrThresh, switchCons = switchCons, plotCons = plotCons)
    forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- paste("Irr", type, sep = "")
  }

  return(forestPlots)

}
