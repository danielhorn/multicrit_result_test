# HelferFunktion: Erzeuge Punkte auf der Pareto-Front zu einem gegebenem
# Gewichtsvektor, so dass jeweils gilt: w * f1 = (1 - w) * f2, wobei
# f1 und f2 die beiden objectives sind

## Ober-Funktion: Erzeuge fuer jede Funktion die Punkte und baue Ergebnis-
## Datensatz
generatePointsWeighted = function(landscape, w) {
  res = data.frame()
  for (i in 1:length(landscape$f.list)) {
    fun = landscape$f.list[[i]]
    pars = landscape$pars[i, ]
    points = generateApprFrontWeighted(fun, pars, w)
    res.tmp = cbind(algorithm = paste("algo", i, sep = ""), points)
    res = rbind(res, res.tmp)
  }
  return(res)
}

## Arbeiterfunktion: Erzeuge die Punkte fuer eine gegebene Funktion
generateApprFrontWeighted = function(fun, pars, w) {
  f.inv = generateSingleParetoFront(a = pars$a, b = -pars$b, c = 0, d = 0, e = 1)
  xmin = f.inv(pars$e - pars$d) - pars$c
  if (!is.finite(xmin)) {
    v = (1 - sapply(seq(0,1, length.out = 1001), fun)) ^ 2
    xmin = (which.min(v) - 1) / 1000
  }
  
  xmax = 1
  ymin = fun(xmax)
  ymax = 1
  
  x.vals = numeric(length(w))
  for (i in 1:length(w)) {
  minimi = function(x) (w[i] * (x - xmin) / (xmax - xmin) -
      (1 - w[i]) * (fun(x) - ymin) / (ymax - ymin)) ^ 2
  x.vals[i] = optimize(minimi, c(0, 1))$minimum
  }
  y.vals = fun(x.vals)
  
  return(data.frame(x = x.vals, y = y.vals))
}
