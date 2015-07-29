relevantAlgosDominationSelection = function(data, formula, contrFun, kappa, alpha) {
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]
  
  data.splitted = split(data, data[, repl.col])
  
  data.splitted = lapply(data.splitted, function(d)
    d[nds_rank(as.matrix(t(d[, var.cols]))) == 1, ])
  
  non.dom.algos = lapply(data.splitted, function(d) unique(d[, algo.col]))
  
  counts = table(unlist(non.dom.algos))
  
  relevant.algos = counts > length(data.splitted) / 2
  
  list(relevant.algos = relevant.algos, algo.contrs = counts)
}