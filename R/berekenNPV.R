#'  Bereken NPV
#'


calNPV <- function(i, cf, t=seq(along=cf)) sum(cf/(1+i)^t)



