# erzeugen eine "zdt1"-funktion, ersetze dabei die Front-Form
# durch die Funktion pareto.function
# g = soll die Funktion g von zdt benutzt werden?
generateZDT1Function = function(pareto.function, use.g = TRUE) {
  zdt1 = function(x) {
    f1 = x[1L]
    g = 1 + ifelse(use.g, 9 * mean(x[-1L]), 0)
    f2 = g * pareto.function(f1/g)
    return(c(f1, f2))
  }
  return(zdt1)
}

# Lasse einen NSGA2 auf obiger Funktion (mit oder ohne g) laufen, um eine
# diskrete Menge an Punkten auf den Fronten zu erzeugen
# idim - idim der zdt1 funktion, generation: anzahl generations des nsga-II
generatePointsNSGA2 = function(landscape, k, use.g, idim, generations) {
  res = data.frame()
  for (i in 1:length(landscape$f.list)) {
    fun = landscape$f.list[[i]]
    pars = getAlgoPars(fun, letters[1:5])
    
    f.inv = generateSingleParetoFront(a = pars$a, b = -pars$b, c = 0, d = 0, e = 1)
    xmin = f.inv(pars$e - pars$d) - pars$c
    if (!is.finite(xmin)) {
      v = (1 - sapply(seq(0,1, length.out = 1001), fun)) ^ 2
      xmin = (which.min(v) - 1) / 1000
    }
    
    xmax = 1
    fun.tmp = function(x) {
      fun(x * (xmax - xmin) + xmin)
    }
    
    zdt = generateZDT1Function(fun.tmp, use.g)
    opt = mco::nsga2(zdt, idim = idim, odim = 2L, lower.bounds = rep(0, 10), 
      upper.bounds = rep(1, 10), popsize = k, generations = generations)
    points = mco::paretoFront(opt)
    points = data.frame(x = points[, 1], y = points[, 2])
    
    points$x = points$x * (xmax - xmin) + xmin

    res.tmp = cbind(algorithm = paste("algo", i, sep = ""), points)
    res = rbind(res, res.tmp)
  }
  return(res)
}
