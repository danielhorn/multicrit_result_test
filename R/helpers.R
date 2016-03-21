
# Erzeuge zufaellige Parametern fuer eine einzelne Front
# Eingabe sind jeweils obere und untere Grenze fuer alle Params
# Achtung: c und d wird fest auf 0 gesetzt, e auf 1, und das Vorzeichen von b wird
# getrennt gesampelt

sampleParetoFrontParams = function(a.lim = c(10, 25), b.lim = c(0.05, 2), sign.b = NULL) {
  
  a = runif(n = 1, min = a.lim[1], max = a.lim[2])
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
makeNoisy = function(funs) {
  addNoise = function(fun) {
    params = as.list(environment(fun))
    fun2 = fun
    environment(fun2) = new.env()
    environment(fun2)$a = params$a + rnorm(1, 0, 0.03)
    environment(fun2)$b = params$b + rnorm(1, 0, 0.004)
    environment(fun2)$c = params$c + rnorm(1, 0, 0.02)
    environment(fun2)$d = params$d + rnorm(1, 0, 0.02)
    environment(fun2)$e = params$e
    environment(fun2)$g = params$g
    fun2
  }
  funs$f.list = lapply(funs$f.list, addNoise)
  funs
}


