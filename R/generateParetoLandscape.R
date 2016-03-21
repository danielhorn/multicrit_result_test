#library(checkmate)
library(BBmisc)

source("MOSAP/experiments/generateSingleParetoFront.R")
source("MOSAP/experiments/helpers.R")

# Echte Parameter:
# N: Anzahl Fronten auf der gemeinsamen Front 
# M: Anzahl Fronten ausserhalb der gemeinsamen Front ("Stoerfronten")
# split.points: Schnittpunkte der Fronten auf der gemeinsamen Front

# Technische Parameter:
# max.iter: max. Neusampling einer Funktion, bis die letzte Funktion neu 
#   gesampelt wird
# max.iter.k_c: max. Neusampling des Parameters c, bis die ganze Funktion neu 
#   gesampelt wird

generateParetoLandscape = function(N = 2L, M = 2L, split.points = 0.5,
  max.iter = 100L, max.iter.k_c = 10L) {
  
  assertInt(N, lower = 1, upper = Inf)
  assertInt(M, lower = 0, upper = Inf)
  assertNumeric(split.points, len = N - 1, lower = 0, upper = 1, any.missing = FALSE, 
    unique = TRUE)
  if (any(diff(split.points) <= 0))
    stop("split.points must be defined increasing")
  
  assertInt(max.iter, lower = 1, upper = Inf)
  assertInt(max.iter.k_c, lower = 1, upper = Inf)

  # erste Front samplen 
  par1 = sampleParetoFrontParams()
  # c samplen, leichte Verschiebung nach links
  par1$c = abs(rnorm(1, mean = 0, sd = 0.05))
  f.par1 = do.call(generateSingleParetoFront, as.list(par1))
  # Verschieben nach oben/unten, sodass die Funktion durch c(0,1) geht
  y_0 = f.par1(0)
  par1$d = 1 - y_0
  
  pars = BBmisc::convertListOfRowsToDataFrame(list(par1))
  f.list = list(do.call(generateSingleParetoFront, as.list(pars[1, ])))
  
  split.points = c(0, split.points, 1)
  # y-Werte an den Splitpoints
  y.split.point = numeric(length(split.points))
  y.split.point[1] = 1
  
  # wird gebraucht, um spaeter die Funktionen vergleichen zu koennen, denn es
  # sollen sich 2 Funktionen jeweils nicht zu aehnlich sein.
  z1000 = seq(0, 1, length.out = 1000)
  Z = as.data.frame(matrix(ncol = N + M, nrow = 1000))
  Z[, 1] = sapply(z1000, f.list[[1]])
  
  # welche Funktion ist wo auf der gemeinsamen Front?
  for (i in 2:(N + 1)) {
    Z$f[z1000 >= split.points[i-1] & z1000 <= split.points[i]] = (i-1)
  }

  # i: Zaehler fuer die Front
  i = 2
  # k: Zaehler fuer neues Sampling pro Front
  k = 0
  repeat {
    # Zaehler k hochsetzen. Falls Maximum erreicht, letzte Front neu sampeln
    k = k + 1
    if (k == max.iter && i >= 2) {
      k = 1
      i = i - 1
      # Falls wir die 1. Front wegmachen muessen, fangen wir einfach mal
      # komplett neu an
      if (i == 1)
        return(Recall(N, M, split.points[-c(1, N + 1)], max.iter, max.iter.k_c))
      next
    }
    
    # Sampeln:
    par = sampleParetoFrontParams()
    
    # k_c: Zaehler fuer neues Sampling von c
    k_c = 1
    
    repeat {
      print(c(i, k, k_c))
      if (k_c == max.iter.k_c) {
        break
      }
      
      # d wieder auf 0 setzen
      par$d = 0
      # horizontale Verschiebung der Funktion zufaellig waehlen, Verschiebung 
      # nach rechts in die Naehe des Split-Punktes
      par$c = -abs(rnorm(1, mean = split.points[i], sd = 0.05))
      
      f.par = do.call(generateSingleParetoFront, as.list(par))
      
      y.split.point[i] = f.list[[i - 1]](split.points[i])
      
      # Wert der neuen Funktion am Splitpunkt
      y = f.par(split.points[i])
      
      # neue Funktion vertikal so verschieben, dass sie durch den Split-Punkt geht
      par$d = y.split.point[i] - y
      f.par = do.call(generateSingleParetoFront, as.list(par))
      
      # Ueberpruefen, dass die neue Funktion (i) an allen vorherigen 
      # Splitpunkten (j in (1, ..., i-1)) groesser ist als die dazugehoerige 
      # Funktion (j) und dass sie am naechsten Splitpunkt (i+1) 
      # kleiner ist als alle vorherigen Funktionen.
      # Dadurch ist auch sichergestellt, dass keine ungewollten 
      # Schnittpunkte mit der gemeinsamen Paretofront gegeben sind.
      
      # neue Funktion an allen Stellen vor dem Splitpunkt größer als die dazugehörige
      # Funktion auf der gemeinsamen Front, an allen Stellen nach dem Splitpunkt bis
      # zum nächsten Splitpunkt größer als die Funktion j
      Z[, i] = sapply(z1000, f.par)
      
      for (j in 1:(i - 1)) { 
        # an allen Stellen vor dem aktuellen Splitpunkt soll die neue Funktion größer 
        # sein als die entsprechende auf der gemeinsamen Front
        o = any((Z[Z$f == j, i] < Z[Z$f == j, j]))
        if (o) break
        
        # an allen Stellen nach dem aktuellen Splitpunkt bis zum nächsten Splitpunkt
        # soll die neue Funktion kleiner sein als alle anderen Funktionen bisher
        o = any((Z[Z$f == i, i] > Z[Z$f == i, j]))
        if (o) break

        # Funktionswert der neuen Funktion am Splitpunkt j
        z1 = f.par(split.points[j])
        # Funktionswert der Funktion j am Splitpunkt j
        z2 = f.list[[j]](split.points[j])
        o = (z1 <= z2) 
        if (o) break
        
        # Funktionswerte der neuen Funktion und der Funktion j 
        # am Splitpunkt i+1
        z1 = f.par(split.points[i + 1])
        z2 = f.list[[j]](split.points[i + 1])
        o = (z1 >= z2) 
        if (o) break
        
        # Ueberpruefen, ob die Funktion einer bisherigen anderen Funktion zu aehnlich ist
        # (mehr als 25% der Punkte zu aehnlich)
        o = (quantile(abs(Z[, i] - Z[, j]), probs = 0.25, na.rm = TRUE) < 0.05)
        if (o) break
      }
      
      if (o) {
        k_c = k_c + 1
        next
      }
      
      break
    }
    
    # falls k_c das Maximum erreicht hat, komplett neue Funktion sampeln
    if (k_c == max.iter.k_c) {
      k_c = 1
      next
    }
    
    
    
    # falls alles ok (oder i = 1), Funktion abspeichern, Zaehler k auf 0 setzen
    k = 1
    pars[i, ] = par
    f.list[[i]] = f.par
    if (i == N) {
      y.split.point[N + 1] = f.list[[N]](1)
      break
    }
    
    i = i + 1
    
  }
  
  # Fronten ausserhalb der gemeinsamen Front
  if (M > 0) {
    m = 1
    repeat {
      # a und b sampeln, c und d erstmal wie eine der N Fronten auf der gemeinsamen
      # Front waehlen, dann noch etwas verschieben
      par = sampleParetoFrontParams()
      J = sample(1:N, 1)
      par$c = pars[J, ]$c - abs(rnorm(1, mean = 0, sd = 0.05))
      par$d = pars[J, ]$d + abs(rnorm(1, mean = 0, sd = 0.05))
      
      f.par = do.call(generateSingleParetoFront, as.list(par))
      Z[, N + m] = sapply(z1000, f.par)
      
      if (intersectWithCommonFront(f.par, f.list, split.points, y.split.point, m, N, Z)) {
        next
      }
      
      pars[N + m, ] = par
      f.list[[N + m]] = f.par
      
      if (m  == M) break
      
      m = m + 1
      
    }
  }
  
  # zum Schluss alle Fronten auf y in (0, 1) Skalieren
  # (um y.min nach oben schieben, dann mit dem Abstand zwischen
  # y.min und y.max skalieren)
  y.max = f.list[[1]](0)
  y.min = f.list[[N]](1)

  for (j in 1:(N + M)) {
    pars[j, ]$d = pars[j, ]$d - y.min
    pars[j, ]$e = abs(y.max - y.min)
    f.list[[j]] = do.call(generateSingleParetoFront, as.list(pars[j, ]))
  }

  
  # y-Werte an den Splitpoints nach der Skalierung:
  y.split.point[1] = f.list[[1]](0)
  for (j in 1:N) {
    y.split.point[j + 1] = f.list[[j]](split.points[j + 1])
  }

  return(list(pars = pars, f.list = f.list, split.points = split.points, 
    y.split.point = y.split.point))
}

