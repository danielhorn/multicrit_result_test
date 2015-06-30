# Select all algorithms that are significant better than kappa

relevantAlgosSelection = function(data, formula, contrFun, kappa, alpha) {
  
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]
  
  algo.contrs = t(sapply(split(data, data[, repl.col]), function(d) {
    o = as.matrix(d[, var.cols])
    points = lapply(unique(d[, algo.col]), function(s)
      as.matrix(d[d[, algo.col] != s, var.cols]))
    sapply(points, contrFun, o = o)
  }))
  colnames(algo.contrs) = unique(data[, algo.col])
  
  # Due to numerical reasons some values are less than or equal to zero. this
  # hould not happen, so round them to 1e-16 (since we want to log soon).
  # but add a small positive random value to each value here - we don't want to
  # have to many equal values
  small.inds = algo.contrs < 1e-16
  algo.contrs[small.inds] = 1e-16 + abs(rnorm(sum(small.inds), 0, 1e-16))
  
  # Test-Procedure to select k best algos
  # here we use as alpha half of the niveau given above
  relevant.algos = relevantAlgosTest(algo.contrs, kappa = kappa, alpha = alpha)
  
  list(relevant.algos = relevant.algos, algo.contrs = algo.contrs)
}