###############################################################
# plot random selection function
###############################################################

rdmSelect <- function(plotList, threshold){
  # randomly pick first plot
  plots <- plotList[round(runif(1, min = 1, max = length(plotList)))]
  remaining <- plotList
  # add new plots till the threshold is reached
  while(sum(forestPlots[forestPlots$STAND_ID %in% plots, 'AREA']) < threshold && length(remaining) > 0){
    # randomly select 1 more plot
    addplot <- remaining[round(runif(1, min = 1, max = length(remaining)))]
    plots <- c(plots, addplot)
    # list of remaining plots
    remaining <- plotList[!(plotList %in% plots)]
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

  # conservation
  plotCons <- c()
  if (conservationThresh > 0){
    conservationThresh <- area * conservationThresh
    plotCons <- rdmSelect(plotList = plotList, threshold = conservationThresh)
    forestPlots[forestPlots$STAND_ID %in% plotCons, "COMMENT"] <- paste(type, "Cons", sep = "")
  }

  # final Harvest
  plotHarv <- c()
  if (HarvThresh > 0){
    HarvThresh <- area * HarvThresh
    plotHarv <- rdmSelect(plotList = plotList[!(plotList %in% plotCons)], threshold = HarvThresh)
    forestPlots[forestPlots$STAND_ID %in% plotHarv, "COMMENT"] <- paste(type, "Harv", sep = "")
  }

  # thinning and harvest
  plotThinHarv <- c()
  if (thinHarvThresh > 0){
    thinHarvThresh <- area * thinHarvThresh
    plotThinHarv <- rdmSelect(plotList = plotList[!(plotList %in% c(plotCons, plotHarv))], threshold = thinHarvThresh)
    forestPlots[forestPlots$STAND_ID %in% plotThinHarv, "COMMENT"] <- paste(type, "ThinHarv", sep = "")
  }

  # irregular
  plotIrr <- c()
  if (irrThresh > 0){
    irrThresh <- area * irrThresh
    plotIrr <- rdmSelect(plotList = plotList[!(plotList %in% c(plotCons, plotHarv, plotThinHarv))], threshold = irrThresh)
    forestPlots[forestPlots$STAND_ID %in% plotIrr, "COMMENT"] <- paste(type, "Irr", sep = "")
  }

  return(forestPlots)

}
