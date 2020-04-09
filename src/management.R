###############################################################
# random (or highest G) forestplot selection function
###############################################################

rdmSelect <- function(plotSubset, threshold, switch, plotCons){

  # if propHarvNonmanaged > conservationThresh
  if(switch == 0){
    plots <- plotCons
    remaining <- plotSubset[!(plotSubset %in% plots)]
  } else if(switch == 1){
    # randomly pick first plot
    plots <- plotSubset[round(runif(1, min = 1, max = length(plotSubset)))]
    remaining <- plotSubset[!(plotSubset %in% plots)]
  } else if(switch == 2){
    # randomly pick first according to its probability of being an irregular plot
    plots <- sample(x = forestStands[forestStands$STAND_ID %in% plotSubset, 'STAND_ID'], size = 1, prob = forestStands[forestStands$STAND_ID %in% plotSubset, 'jointProba'])
    remaining <- plotSubset[!(plotSubset %in% plots)]
  }

  if (switch %in% c(0,1)){
    # add new plots till the threshold is reached
    while(sum(forestStands[forestStands$STAND_ID %in% plots, 'AREA']) < threshold && length(remaining) > 0){
      # randomly select 1 more plot
      addplot <- remaining[round(runif(1, min = 1, max = length(remaining)))]
      plots <- c(plots, addplot)
      # list of remaining plots
      remaining <- plotSubset[!(plotSubset %in% plots)]
      # print(length(remaining))
    }
  }

  if (switch == 2){
    # add new plots till the threshold is reached
    while(sum(forestStands[forestStands$STAND_ID %in% plots, 'AREA']) < threshold && length(remaining) > 0){
      # randomly select 1 more plot according to its probability of being an irregular plot
      addplot <- sample(x = forestStands[forestStands$STAND_ID %in% remaining, 'STAND_ID'], size = 1, prob = forestStands[forestStands$STAND_ID %in% remaining, 'jointProba'])
      plots <- c(plots, addplot)
      # list of remaining plots
      remaining <- plotSubset[!(plotSubset %in% plots)]
      # print(length(remaining))
    }
  }

  return(plots)
}

###############################################################
# assign management function
###############################################################

management <- function(type, plotList, conservationThresh, HarvThresh,
                                                thinHarvThresh, irrThresh){
  # calculate area
  area <- sum(forestStands[forestStands$STAND_ID %in% plotList, 'AREA'])

  # conservation ---------------------------------------------------------------
  plotCons <- c()

  # all non-harvestable plots are necessarily in conservation and out of proportions
  plotCons <- forestStands[forestStands$STAND_ID %in% plotList & forestStands$nonHarv == 0, 'STAND_ID']

  # the proportion are only applied to the remaining plots
  # remaining plots proportion
  propRemaining <- (area - sum(forestStands[forestStands$STAND_ID %in% plotCons, 'AREA'])) / area
  # correct proportions
  conservationThresh <- propRemaining * conservationThresh
  HarvThresh <- propRemaining * HarvThresh
  thinHarvThresh <- propRemaining * thinHarvThresh
  irrThresh <- propRemaining * irrThresh

  # Harvestable but not managed plots are also classified in conservation
  plotCons <- c(plotCons, forestStands[forestStands$STAND_ID %in% plotList & forestStands$nonHarv == 1 & forestStands$EXPLOITABILITY==0, 'STAND_ID'])
  propHarvNonmanaged <- sum(forestStands[forestStands$STAND_ID %in% plotList & forestStands$nonHarv == 1 & forestStands$EXPLOITABILITY==0, 'AREA']) / area

  # 2 possible cases:
  # propHarvNonmanaged < conservationThresh
  switch <- 1
  if(propHarvNonmanaged <= conservationThresh){
    switch <- 0
    conservationThresh <- area * (conservationThresh + (1- propRemaining))
    plotCons <- rdmSelect(plotSubset = plotList, threshold = conservationThresh, switch = switch, plotCons = plotCons)
    forestStands[forestStands$STAND_ID %in% plotCons, "COMMENT"] <- paste("Con", type, sep = "")
    switch <- 1
  }

  # propHarvNonmanaged > conservationThresh
  if(propHarvNonmanaged > conservationThresh){
    propRemaining2 <- (area - sum(forestStands[forestStands$STAND_ID %in% plotCons, 'AREA'])) / area
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
    forestStands[forestStands$STAND_ID %in% plotCons, "COMMENT"] <- paste("Con", type, sep = "")
  }

  # irregular ------------------------------------------------------------------
  plotIrr <- c()
  if (irrThresh > 0){
    switch <- 2
    irrThresh <- area * irrThresh
    plotIrr <- rdmSelect(plotSubset = plotList[!(plotList %in% plotCons)], threshold = irrThresh, switch = switch, plotCons = plotCons)
    forestStands[forestStands$STAND_ID %in% plotIrr, "COMMENT"] <- paste("Irr", type, sep = "")
    switch <- 1
  }

  # final Harvest --------------------------------------------------------------
  plotHarv <- c()
  if (HarvThresh > 0){
    HarvThresh <- area * HarvThresh
    plotHarv <- rdmSelect(plotSubset = plotList[!(plotList %in% c(plotCons, plotIrr))], threshold = HarvThresh, switch = switch, plotCons = plotCons)
    forestStands[forestStands$STAND_ID %in% plotHarv, "COMMENT"] <- paste("Har", type, sep = "")
  }

  # thinning and harvest -------------------------------------------------------
  plotThinHarv <- c()
  if (thinHarvThresh > 0){
    thinHarvThresh <- area * thinHarvThresh
    plotThinHarv <- rdmSelect(plotSubset = plotList[!(plotList %in% c(plotCons, plotIrr, plotHarv))], threshold = thinHarvThresh, switch = switch, plotCons = plotCons)
    forestStands[forestStands$STAND_ID %in% plotThinHarv, "COMMENT"] <- paste("Thi", type, sep = "")
  }

  return(forestStands)

}
