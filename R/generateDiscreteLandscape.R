library(mco)
source("MOSAP/experiments/generateApprFrontWeighted.R")
source("MOSAP/experiments/generateApprFrontNSGA2.R")

## 4 Funktionen, um eine diskrete Approximation einer Pareto Landschaft
## zu erzeugen. Eingabe ist jeweils eine Pareto-Landschaft und die Anzahl
## Punkte, die pro Front erzeugt werden soll

generateDiscreteParetoLandscapeDeterministic = function(landscape, k) {
  w = seq(0, 1, length.out = k)
  return(generatePointsWeighted(landscape, w))
}

generateDiscreteParetoLandscapeUniform = function(landscape, k) {
  w = runif(k, 0, 1)
  return(generatePointsWeighted(landscape, w))
}

generateDiscreteParetoLandscapeNSGA2 = function(landscape, k, idim = 2L, generations = 20L) {
  generatePointsNSGA2(landscape, k, use.g = FALSE, idim = idim, generations = generations)
}

generateDiscreteParetoLandscapeNSGA2_g = function(landscape, k, idim = 2L, generations = 20L) {
  generatePointsNSGA2(landscape, k, use.g = TRUE,  idim = idim, generations = generations)
}

