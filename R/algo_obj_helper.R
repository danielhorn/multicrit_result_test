# Getter für Parameter (par.name) der Algo-Funktion (algo.fun)
getAlgoPar = function(algo.fun, par.name) {
  environment(algo.fun)[[par.name]]
}

getAlgoPars = function(algo.fun, par.names = letters[1:5]) {
  l = lapply(par.names, getAlgoPar, algo.fun = algo.fun)
  names(l) = par.names
  l
}

# Setter für Parameter (par.name) der Algo-Funktion (algo.fun) auf neuen Wert (value) 
setAlgoPar = function(algo.fun, par.name, value) {
  environment(algo.fun)[[par.name]] = value
  invisible(NULL)
}

# Getter für Algo ID
getAlgoID = function(algo.fun) {
  attr(algo.fun, which = "id")
}

# Getter für y-Range eines Algos algo.fun (auf der Front)
getAlgoRangeY = function(algo.fun) {
  algo.fun(getAlgoRangeX(algo.fun))
}

# Getter für x-Range eines Algos algo.fun (auf der Front) (ehem. split.points)
getAlgoRangeX = function(algo.fun) {
  attr(algo.fun, which = "range.x")
}

# Boolean: Ist der Algo auf der Front vertreten?
isAlgoParetoOpt = function(algo.fun) {
  !(length(getAlgoRangeX(algo.fun)) == 0)
}

# Print-Funktion für Algorithmen-Objekt
print.algo.obj = function(x, ...) {
  cat("Algorithm:", attr(x, which = "id"), "\n")
  cat("----------------- \n")
  
  if(isAlgoParetoOpt(x)) {
    cat("\t Pareto optimal for X in:", getAlgoRangeX(x), "\n")
    cat("\t Pareto optimal for Y in:", getAlgoRangeY(x), "\n")
  } else cat("\t Is not pareto optimal \n")
  cat("\n")
  pars = letters[1:5]
  vals = sapply(pars, getAlgoPar, algo.fun = x)
  print(vals)
}
