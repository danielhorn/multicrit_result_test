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
  split.points = c(1 / 3, 2 / 3), algo.order = 1:(N + M),
  max.iter = 100L, max.iter.k_c = 10L) {
  
  N = asInt(N, lower = 1, upper = Inf)
  M = asInt(M, lower = 0, upper = Inf)
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
  z.seq = seq(0, 1, length.out = 1001)
  Z.X = as.data.frame(matrix(ncol = N + M, nrow = 1001))
  
  # welche Funktion ist wo auf der gemeinsamen Front?
  for (i in 1:N) {
    Z.X$f[z.seq >= split.points[i] & z.seq <= split.points[i + 1]] = (i)
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
  Z.X[, 1] = sapply(z.seq, front.funs[[1]])
  
  # i: Zaehler fuer die Front
  i = 2
  # k: Zaehler fuer neues Sampling pro Front
  k = 0
  while (i <= N) {
    # Zaehler k hochsetzen. Falls Maximum erreicht, letzte Front neu sampeln
    k = k + 1
    if (k == max.iter && i >= 2) {
      k = 1
      i = i - 1
      # Falls wir die 1. Front wegmachen muessen, fangen wir einfach mal
      # komplett neu an
      if (i == 1)
        return(Recall(id, N, M, split.points[-c(1, N + 1)], algo.order, max.iter, max.iter.k_c))
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
      Z.X[, i] = sapply(z.seq, front.funs[[i]])
      
      for (j in 1:(i - 1)) { 
        # an allen Stellen vor dem aktuellen Splitpunkt soll die neue Funktion größer 
        # sein als die entsprechende auf der gemeinsamen Front
        notok = any((Z.X[Z.X$f == j, i] < Z.X[Z.X$f == j, j]))
        if (notok) break
        
        # an allen Stellen nach dem aktuellen Splitpunkt bis zum nächsten Splitpunkt
        # soll die neue Funktion kleiner sein als alle anderen Funktionen bisher
        notok = any((Z.X[Z.X$f == i, i] > Z.X[Z.X$f == i, j]))
        if (notok) break
        
        # Funktionswert der neuen Funktion am Splitpunkt j
        z1 = front.funs[[i]](split.points[j])
        # Funktionswert der Funktion j am Splitpunkt j
        z2 = front.funs[[j]](split.points[j])
        notok = (z1 <= z2) 
        if (notok) break
        
        # Funktionswerte der neuen Funktion und der Funktion j 
        # am Splitpunkt i+1
        z1 = front.funs[[i]](split.points[i + 1])
        z2 = front.funs[[j]](split.points[i + 1])
        notok = (z1 >= z2) 
        if (notok) break
        
        # Ueberpruefen, ob die Funktion einer bisherigen anderen Funktion zu aehnlich ist
        # (mehr als 25% der Punkte zu aehnlich)
        notok = (quantile(abs(Z.X[, i] - Z.X[, j]), probs = 0.25, na.rm = TRUE) < 0.05)
        if (notok) break
      }
      
      if (notok) {
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
    i = i + 1
  }
  
  # Current Front:
  Z.front = apply(Z.X[, 1:N, drop = FALSE], 1, min)
  
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
      
      Z.X[, N + m] = sapply(z.seq, front.funs[[N + m]])
      
      # Checks: Ist der neue immer hinter der front?
      if (any(Z.front - Z.X[, N + m] > 0))
        next
      
      # Zu aehnlich mit einer alten Front?
      dif.mat = abs(Z.X[, 1:(N + m - 1), drop = FALSE] - Z.X[, N + m])
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
  
  # Bis jetzt wurde nur ueberprueft, ob die Fronten sich in Y-Richtung nicht
  # zu aehnlich sind. Im Nachhinein muessen wir jetzt noch pruefen, ob eine
  # Aehnlichkeit in x-Richtung vorliegt. Dafuer bauen wir uns zunaechst
  # eine Matrix genauso wie Z.X, nur in den andere Richtung. Hier muessen wir
  # noch ein wenig tricksen, da wir die Umkehrfunktion der front.funs nicht
  # kennen
  # Dafuer muessen wir  zunaechst Z.X neu bestimmen, da wir jetzt normalisiert
  # haben
  for (i in 1:(N + M)) {
    Z.X[, i] = sapply(z.seq, front.funs[[i]])
  }
  Z.Y = as.data.frame(matrix(ncol = N + M, nrow = 1001))
  for (j in 1:(N + M)) {
    Z.Y[, j] = sapply(z.seq, function(z) mean(z.seq[abs(Z.X[, j] - z) < 5e-2]))
    # NaNs in Z.Y sind moeglich:
    # 2 moegliche Ursachen: Die Funktion ist zu "steilt" und es liegen keine
    # Punkte aus Z.X im Berech < 5e-2. Das sollte nur selten auftreten (Daumen
    # drueck), ansonsten muessen wir uns darum noch kuemmern.
    # Anderer Fall: Die Front des Algo deckt nur einen kleinen Teil des Bereich
    # in y-Richtung ab (z.B. nur von 1.0 bis 0.8).
  }
  
  for (i in 1 + seq_len(N + M - 1)) {
    dif.mat = abs(Z.Y[, 1:(i - 1), drop = FALSE] - Z.Y[, i])
    diffs = apply(dif.mat, 2, quantile, probs = 0.33, na.rm = TRUE)
    # FIXME: Ist das is.na hier okay?
    if (any(diffs < 0.025) | all(is.na(diffs))) {
      print("Recalled - zu aehnlich in Y")
      # Falls es zu aehnlich ist, Recallen wir
      return(Recall(id, N, M, split.points[-c(1, N + 1)], algo.order, max.iter, max.iter.k_c))
    }
      
  }
  
  landscape = makeS3Obj(
    id = id,
    f.list = front.funs,
    classes = "landscape"
  )
  
  return(landscape)
}

