
# Erzeuge zufaellige Parametern fuer eine einzelne Front
# Eingabe sind jeweils obere und untere Grenze fuer alle Params
# Achtung: c und d wird fest auf 0 gesetzt, e auf 1, und das Vorzeichen von b wird
# getrennt gesampelt

sampleParetoFrontParams = function(a.lim = c(-1, 5), b.lim = c(0.05, 5), sign.b = NULL) {
  
  x = runif(n = 1, min = a.lim[1], max = a.lim[2])
  a = 2 ^ x
  if (is.null(sign.b)) {
    sign.b = sample(c(-1, 1), 1)
  }
  b = runif(n = 1, min = b.lim[1], max = b.lim[2])
  b = b * sign.b
  c = d = 0
  e = 1
  
  return(list(a = a, b = b, c = c, d = d, e = e))
}


# Funktion, die angibt, ob eine neue Front die gemeinsame Front schneidet oder 
# ob sie unterhalb der gemeinsamen Front liegt oder ob sie einer Front auf der 
# gemeinsamen Front zu aehnlich ist
# 
# m: Index der aktuellen Front ausserhalb der gemeinsamen Front
intersectWithCommonFront = function(f.par, f.list, split.points, y.split.point, 
  m, N, Z) {
  # Ueberpruefen, ob die Funktion an jedem Split-Punkt groesser ist als
  # die gemeinsame Front
  for (j in seq_along(split.points)) {
    z1 = f.par(split.points[j])
    z2 = y.split.point[j]
    o = (z1 < z2)
    if (o) return(o)
  }
  # auf Schnittpunkte mit der gemeinsamen Front ueberpruefen
  for (i in 1:N) {
    opt = optimize(f = function(x) {abs(f.par(x) - f.list[[i]](x))}, 
      lower = split.points[i], upper = split.points[i + 1])
    o = (opt$objective < 0.1)   
    if (o) return(o)
    
    # Ueberpruefen, ob die Funktion einer anderen Funktion der gemeinsamen Front
    #  zu aehnlich ist (mehr als 25% der Punkte zu aehnlich)
    o = (quantile(abs(Z[, N + m] - Z[, i]), probs = 0.25, na.rm = TRUE) < 0.05)
    if (o) return(o)
    
  }
  return(FALSE)
}

## Gets a paretoLandscape and adds a bit noise to every param
makeNoisy = function(landscape, sd = list(a = 0.03, b = 0.004, c = 0.02, d = 0.02)) {
  addNoise = function(algo.obj) {
    # First: Deep Copy the landscape - we need a new environment
    old.pars = as.list(environment(algo.obj))
    environment(algo.obj) = new.env()
    setAlgoPar(algo.obj, "a", old.pars$a + rnorm(1, 0, sd$a))
    setAlgoPar(algo.obj, "b", old.pars$b + rnorm(1, 0, sd$b))
    setAlgoPar(algo.obj, "c", old.pars$c + rnorm(1, 0, sd$c))
    setAlgoPar(algo.obj, "d", old.pars$d + rnorm(1, 0, sd$d))
    setAlgoPar(algo.obj, "e", old.pars$e)
    
    algo.obj
  }
  landscape$front.funs = lapply(landscape$front.funs, addNoise)
  landscape
}


