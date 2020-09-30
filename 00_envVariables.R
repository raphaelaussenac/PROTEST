## READ ME
# set the following variable in the console then run this script before any other
user <- list("jm")
# user <- list("raph")
#
if (user[[1]]=="raph")
{
  user$WorkingDir <- "C:/Users/raphael.aussenac/Documents/GitHub/PROTEST"
  user$NetworkProtestDir <- "X:/ProjetsCommuns/PROTEST/"
}
# define path variables
if (user[[1]]=="jm")
{
  user$WorkingDir <- "/media/data/R/packages/PROTEST"
  # user$WorkingDir <- "/home/jean-matthieu/R/PROTEST"
  user$NetworkProtestDir <- "/media/reseau/lessem/ProjetsCommuns/PROTEST/"
  user$confinement <- FALSE
}

