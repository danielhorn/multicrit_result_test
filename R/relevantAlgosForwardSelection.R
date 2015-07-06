

relevantAlgosForwardSelection = function(data, formula, contrFun, kappa, alpha) {
  
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]

  data.splitted = split(data, data[, repl.col])
  
  solvers = unique(data[, algo.col])
  selected = c()
  selected.val = c()
  for (i in seq_len(length(solvers))) {
    algo.contrs = sapply(data.splitted, function(d) {
      o = as.matrix(d[, var.cols])
      points = lapply(setdiff(solvers, selected), function(s)
        as.matrix(d[d[, algo.col] %in% c(selected, s), var.cols]))
      sapply(points, contrFun, o = o)
    })
    if (is.matrix(algo.contrs)) {
      min.index = getMinIndex(apply(algo.contrs, 1, median))
      selected = c(selected, setdiff(solvers, selected)[min.index])
      selected.val = c(selected.val, rowMeans(algo.contrs)[min.index])
    } else {
      selected = c(selected, setdiff(solvers, selected))
      selected.val = c(selected.val, mean(algo.contrs))
    }
  }
  relevant.algos = selected.val > kappa  
  names(relevant.algos) = selected
  names(selected.val) = selected
  
  perm = sapply(solvers, function(a) which(a == selected))
  
  list(relevant.algos = relevant.algos[perm], algo.contrs = selected.val[perm])
}