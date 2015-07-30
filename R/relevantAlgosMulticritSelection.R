
relevantAlgosMulticritSelection = function(data, formula, contrFun, kappa, alpha) {
  
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]
 
  algos = as.character(unique(data[, algo.col]))
  
  # first, we need the reference front. this is the median eaf of all algos and repls
  ref.front = eaf:::eafs(points = data[, var.cols], sets = data[, repl.col], percentiles = 50)[, 1:2]
  
  # a getIndicatorValue Function - gets a vector of algos and returns the indicator
  getIndicatorValue = function(algos) {
    # first get the eaf - but only of algos
    d = subset(data, data[, algo.col] %in% algos)
    front = eaf:::eafs(points = d[, var.cols], sets = d[, repl.col], percentiles = 50)[, 1:2]
    contrFun(front, ref.front)
  }
  
  # get every combination of algos
  combs = as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), length(algos))))
  combs = combs[-nrow(combs), ]
  colnames(combs) = algos
  # and compute its indicator value
  contr.vals = t(apply(combs, 1, function(inds) c(sum(inds), getIndicatorValue( algos[inds]))))
  
  # normalize and weighted sum of the 2 objective number of algos and indicator value
  contr.vals.norm = normalize(contr.vals, method = "range", margin = 2L)
  min.index = which.min(0.05 * contr.vals.norm[, 1L] + 0.95 * contr.vals.norm[, 2L])
  
  print(min.index)
  print(combs[min.index, ])
  
  list(relevant.algos = combs[min.index, ], algo.contrs = contr.vals)
}
