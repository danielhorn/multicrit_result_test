## 4 Funktionen, um eine diskrete Approximation einer Pareto Landschaft
## zu erzeugen. Eingabe ist jeweils eine Pareto-Landschaft und die Anzahl
## Punkte, die pro Front erzeugt werden soll

generateDiscreteParetoLandscapeDeterministic = function(landscape, k) {
  w = seq(0, 1, length.out = k)
  return(generatePointsWeighted(landscape, w))
}

generateDiscreteParetoLandscapeRandom = function(landscape, k) {
  w = runif(k, 0, 1)
  return(generatePointsWeighted(landscape, w))
}

generateDiscreteParetoLandscapeNSGAII = function(landscape, k, idim = 2L, generations = 20L) {
  generatePointsNSGA2(landscape, k, use.g = FALSE, idim = idim, generations = generations)
}

generateDiscreteParetoLandscapeNSGAII_g = function(landscape, k, idim = 2L, generations = 20L) {
  generatePointsNSGA2(landscape, k, use.g = TRUE,  idim = idim, generations = generations)
}

