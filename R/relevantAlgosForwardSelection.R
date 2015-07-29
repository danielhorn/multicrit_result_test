

relevantAlgosForwardSelection = function(data, formula, contrFun, kappa, alpha) {
  
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]

  data.splitted = split(data, data[, repl.col])
  
  solvers = unique(data[, algo.col])
  selected = c()
  # Each col of selected.val is one iteration of forward selection
  selected.val = matrix(NA, nrow = length(solvers), ncol = length(solvers))
  algo.contrs.list = list()
  colnames(selected.val) = seq_along(solvers)
  rownames(selected.val) = solvers 
  for (i in seq_len(length(solvers))) {
    # double for-loop: calculate algo contribution for every replication and
    # for every algo.
    algo.contrs = sapply(data.splitted, function(d) {
      o = as.matrix(d[, var.cols])
      points = lapply(setdiff(solvers, selected), function(s)
        as.matrix(d[d[, algo.col] %in% c(selected, s), var.cols]))
      sapply(points, contrFun, o = o)
    })
    
    if (is.matrix(algo.contrs)) {
      rownames(algo.contrs) = setdiff(solvers, selected)
      algo.contrs.list[[i]] = algo.contrs
      
      min.index = getMinIndex(apply(algo.contrs, 1, median))
      selected.val[solvers %nin% selected, i] = apply(algo.contrs, 1, median)
      selected = c(selected, setdiff(solvers, selected)[min.index])
    } else {
      # last iteration, we only have 1 algo left
      algo.contrs = matrix(algo.contrs, ncol = length(data.splitted))
      rownames(algo.contrs) = setdiff(solvers, selected)
      colnames(algo.contrs) = 1:length(data.splitted)
      algo.contrs.list[[i]] = algo.contrs
      
      selected.val[solvers %nin% selected, i] = median(algo.contrs)
      selected = c(selected, setdiff(solvers, selected))
    }
  }
  #print(selected.val)
   # The max val of each col is the selected algo of the coresponding iteration
  relevant.algos = apply(selected.val, 2, min, na.rm = TRUE) > kappa  
  names(relevant.algos) = selected
#  names(selected.val) = selected
  
  
  #perm = sapply(solvers, function(a) which(a == selected))
  
  list(relevant.algos = relevant.algos, algo.contrs = algo.contrs.list)
}