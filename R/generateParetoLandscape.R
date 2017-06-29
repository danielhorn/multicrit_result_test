# Echte Parameter:
# N: Anzahl Fronten auf der gemeinsamen Front 
# M: Anzahl Fronten ausserhalb der gemeinsamen Front ("Stoerfronten")
# split.points: Schnittpunkte der Fronten auf der gemeinsamen Front
# algo.order: numerischer Vektor der Laenge N,
#             Reihenfolge der Algorithmen auf der Paretofront

# Technische Parameter:
# max.iter: max. Neusampling einer Funktion, bis die letzte Funktion neu 
#   gesampelt wird
# max.iter.k_c: max. Neusampling des Parameters c, bis die ganze Funktion neu 
#   gesampelt wird

generateParetoLandscape = function(id = "My Landscape", N = 3L, M = 1L,
  split.points = c(1 / 3, 2/  3), algo.order = 1:(N + M),
  max.iter = 100L, max.iter.k_c = 10L) {
  
  assertInt(N, lower = 1, upper = Inf)
  assertInt(M, lower = 0, upper = Inf)
  assertNumeric(split.points, len = N - 1, lower = 0, upper = 1, any.missing = FALSE, 
    unique = TRUE)
  if (any(diff(split.points) <= 0))
    stop("split.points must be defined increasing")
  
  assertInt(max.iter, lower = 1, upper = Inf)
  assertInt(max.iter.k_c, lower = 1, upper = Inf)
  
  # Initialisieren:
  split.points = c(0, split.points, 1)
  
  front.funs = list()
  
  # wird gebraucht, um spaeter die Funktionen vergleichen zu koennen, denn es
  # sollen sich 2 Funktionen jeweils nicht zu aehnlich sein.
  z1000 = seq(0, 1, length.out = 1000)
  Z = as.data.frame(matrix(ncol = N + M, nrow = 1000))
  
  # welche Funktion ist wo auf der gemeinsamen Front?
  for (i in 1:N) {
    Z$f[z1000 >= split.points[i] & z1000 <= split.points[i + 1]] = (i)
  }
  
  # erste Front samplen 
  par = sampleParetoFrontParams()
  # c samplen, leichte Verschiebung nach links
  par$c = abs(rnorm(1, mean = 0, sd = 0.05))
  
  front.funs[[1]] = structure(do.call(generateSingleParetoFront, par), 
    id = paste0("algo", algo.order[1]),
    range.x = split.points[1:2], 
    class = "algo.obj"
    )
  
  # Verschieben nach oben/unten, sodass die Funktion durch c(0,1) geht
  setAlgoPar(front.funs[[1]], "d", 1 - front.funs[[1]](0))
  
  # Buchhaltung in f
  Z[, 1] = sapply(z1000, front.funs[[1]])
  
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
      
      front.funs[[i]] = structure(do.call(generateSingleParetoFront, par), 
        id = paste0("algo", algo.order[i]),
        range.x = split.points[i + 0:1], 
        class = "algo.obj"
      )
      
      # Wert der alten und neuen Funktion am Splitpunkt
      y.last = getAlgoRangeY(front.funs[[i - 1]])[2]
      y.current = front.funs[[i]](split.points[i])
      
      # neue Funktion vertikal so verschieben, dass sie durch den Split-Punkt geht
      setAlgoPar(front.funs[[i]], "d", y.last - y.current)
      
      # Ueberpruefen, dass die neue Funktion (i) an allen vorherigen 
      # Splitpunkten (j in (1, ..., i-1)) groesser ist als die dazugehoerige 
      # Funktion (j) und dass sie am naechsten Splitpunkt (i+1) 
      # kleiner ist als alle vorherigen Funktionen.
      # Dadurch ist auch sichergestellt, dass keine ungewollten 
      # Schnittpunkte mit der gemeinsamen Paretofront gegeben sind.
      
      # neue Funktion an allen Stellen vor dem Splitpunkt größer als die dazugehörige
      # Funktion auf der gemeinsamen Front, an allen Stellen nach dem Splitpunkt bis
      # zum nächsten Splitpunkt größer als die Funktion j
      Z[, i] = sapply(z1000, front.funs[[i]])
      
      for (j in 1:(i - 1)) { 
        # an allen Stellen vor dem aktuellen Splitpunkt soll die neue Funktion größer 
        # sein als die entsprechende auf der gemeinsamen Front
        ok = any((Z[Z$f == j, i] < Z[Z$f == j, j]))
        if (ok) break
        
        # an allen Stellen nach dem aktuellen Splitpunkt bis zum nächsten Splitpunkt
        # soll die neue Funktion kleiner sein als alle anderen Funktionen bisher
        ok = any((Z[Z$f == i, i] > Z[Z$f == i, j]))
        if (ok) break
        
        # Funktionswert der neuen Funktion am Splitpunkt j
        z1 = front.funs[[i]](split.points[j])
        # Funktionswert der Funktion j am Splitpunkt j
        z2 = front.funs[[j]](split.points[j])
        ok = (z1 <= z2) 
        if (ok) break
        
        # Funktionswerte der neuen Funktion und der Funktion j 
        # am Splitpunkt i+1
        z1 = front.funs[[i]](split.points[i + 1])
        z2 = front.funs[[j]](split.points[i + 1])
        ok = (z1 >= z2) 
        if (ok) break
        
        # Ueberpruefen, ob die Funktion einer bisherigen anderen Funktion zu aehnlich ist
        # (mehr als 25% der Punkte zu aehnlich)
        ok = (quantile(abs(Z[, i] - Z[, j]), probs = 0.25, na.rm = TRUE) < 0.05)
        if (ok) break
      }
      
      if (ok) {
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
    
    # falls alles ok ist, Zaehler k auf 0 setzen und naechste Funktion
    k = 1

    if (i == N) {
      #y.split.point[N + 1] = f.list[[N]](1)
      break
    }
    
    i = i + 1
    
  }
  
  # Current Front:
  Z.front = apply(Z[, 1:N], 1, min)
  
  # Fronten ausserhalb der gemeinsamen Front
  if (M > 0) {
    m = 1
    repeat {
      # a und b sampeln, c und d erstmal wie eine der N Fronten auf der gemeinsamen
      # Front waehlen, dann noch etwas verschieben
      par = sampleParetoFrontParams()
      J = sample(N, 1)
      par$c = getAlgoPar(front.funs[[J]], "c") - abs(rnorm(1, mean = 0, sd = 0.05))
      par$d = getAlgoPar(front.funs[[J]], "d") + abs(rnorm(1, mean = 0, sd = 0.05))
      
      front.funs[[N + m]] = structure(do.call(generateSingleParetoFront, par), 
        id = paste0("algo", algo.order[N + m]),
        range.x = numeric(0), 
        class = "algo.obj"
      )
      
      Z[, N + m] = sapply(z1000, front.funs[[N + m]])
      
      # Checks: Ist der neue immer hinter der front?
      if (any(Z.front - Z[, N + m] > 0))
        next
      
      # Zu aehnlich mit einer alten Front?
      dif.mat = abs(Z[, 1:(N + m - 1)] - Z[, N + m])
      if (any(apply(dif.mat, 2, quantile, probs = 0.25, na.rm = TRUE) < 0.05))
        next

      if (m == M)
        break
      
      m = m + 1
    }
  }
  
  # zum Schluss alle Fronten auf y in (0, 1) Skalieren
  # (um y.min nach oben schieben, dann mit dem Abstand zwischen
  # y.min und y.max skalieren)
  y.max = getAlgoRangeY(front.funs[[1]])[1]
  y.min = getAlgoRangeY(front.funs[[N]])[2]
  
  for (j in 1:(N + M)) {
    setAlgoPar(front.funs[[j]], "d", getAlgoPar(front.funs[[j]], "d") - y.min)
    setAlgoPar(front.funs[[j]], "e", abs(y.max - y.min))
  }
  
  landscape = makeS3Obj(
    id = id,
    f.list = front.funs,
    classes = "landscape"
  )
  
  return(landscape)
}

