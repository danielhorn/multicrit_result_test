# Note: I "hacked" the EAF package - use the underlying eaf function to get points
# for my own eaf plotting

relevantAlgosEAFSelection = function(data, formula, contrFun, kappa, alpha) {
  
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]
  
  # first, we need the reference front. this is the median eaf of all algos and repls
  ref.front = eaf:::eafs(points = data[, var.cols], sets = data[, repl.col], percentiles = 50)[, 1:2]
  
  # a getIndicatorValue Function - gets a vector of algos and returns the indicator
  getIndicatorValue = function(algos) {
    # first get the eaf - but only of algos
    d = subset(data, data[, algo.col] %in% algos)
    front = eaf:::eafs(points = d[, var.cols], sets = d[, repl.col], percentiles = 50)[, 1:2]
    contrFun(front, ref.front)
  }
  
  # now a loop for the selection
  sel.algos = c()
  sel.val = c()
  i = 1L
  algos = as.character(unique(data[, algo.col]))
  algo.contrs.list = list()
  while (length(algos) > 0L) {
    algo.contrs = sapply(algos, function(a) getIndicatorValue(c(a, sel.algos)))
    algo.contrs.list[[i]] = algo.contrs
    i = i + 1
    min.index = which.min (algo.contrs)
    sel.algos = c(sel.algos, algos[min.index])
    algos = algos[-min.index]
    sel.val = c(sel.val, algo.contrs[min.index])
  }
  
  list(relevant.algos = sel.val > kappa, algo.contrs = algo.contrs.list)
}

