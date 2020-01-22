###############################################################
# plot random selection function
###############################################################

rdmSelect <- function(plotSubset, threshold, switchNonHarv){
  # randomly pick first plot
  plots <- plotSubset[round(runif(1, min = 1, max = length(plotSubset)))]

  # force nonHarvestable plots to be part of the Conservation type and then select
  # extra plot to reach the defined threshold
  if(switchNonHarv == 0){
    plots <- forestPlots[forestPlots$STAND_ID %in% plotSubset & forestPlots$EXPLOITABILITY == 0, 'STAND_ID']
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

  # correct management proportion considering inaccessible/non-harvestable plots
  nonHarv <- sum(forestPlots[forestPlots$STAND_ID %in% plotList & forestPlots$EXPLOITABILITY == 0, 'AREA'])
  propNonHarv <- nonHarv / area

  switchNonHarv <- 1
  if (propNonHarv < conservationThresh){
    switchNonHarv <- 0
    conservationThresh <- area * conservationThresh
    plotCons <- rdmSelect(plotSubset = plotList, threshold = conservationThresh, switchNonHarv = switchNonHarv)
    forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- paste(type, "Cons", sep = "")
    switchNonHarv <- 1
  } else if (propNonHarv > conservationThresh){
    # conservation plots
    plotCons <- forestPlots[forestPlots$STAND_ID %in% plotList & forestPlots$EXPLOITABILITY == 0, 'STAND_ID']
    forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- paste(type, "Cons", sep = "")

    # corrected management proportion
    remainingProp <- 1 - propNonHarv

    # convert prortion of other types of management to "relative" proportion
    totalPropRemainingType <- HarvThresh + thinHarvThresh + irrThresh
    # calculate relative proportion of each type
    HarvThresh <- HarvThresh / totalPropRemainingType
    thinHarvThresh <- thinHarvThresh / totalPropRemainingType
    irrThresh <- irrThresh / totalPropRemainingType
    # assign these relative proportion to remaining plots
    HarvThresh <- remainingProp * HarvThresh
    thinHarvThresh <- remainingProp * thinHarvThresh
    irrThresh <- remainingProp * irrThresh
  }

  # final Harvest --------------------------------------------------------------
  plotHarv <- c()
  if (HarvThresh > 0){
    HarvThresh <- area * HarvThresh
    plotHarv <- rdmSelect(plotSubset = plotList[!(plotList %in% plotCons)], threshold = HarvThresh, switchNonHarv = switchNonHarv)
    forestPlots[forestPlots$STAND_ID %in% plotHarv, "COMMENT"] <- paste(type, "Harv", sep = "")
  }

  # thinning and harvest -------------------------------------------------------
  plotThinHarv <- c()
  if (thinHarvThresh > 0){
    thinHarvThresh <- area * thinHarvThresh
    plotThinHarv <- rdmSelect(plotSubset = plotList[!(plotList %in% c(plotCons, plotHarv))], threshold = thinHarvThresh, switchNonHarv = switchNonHarv)
    forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- paste(type, "ThinHarv", sep = "")
  }

  # irregular ------------------------------------------------------------------
  plotIrr <- c()
  if (irrThresh > 0){
    irrThresh <- area * irrThresh
    plotIrr <- rdmSelect(plotSubset = plotList[!(plotList %in% c(plotCons, plotHarv, plotThinHarv))], threshold = irrThresh, switchNonHarv = switchNonHarv)
    forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- paste(type, "Irr", sep = "")
  }

  return(forestPlots)

}
