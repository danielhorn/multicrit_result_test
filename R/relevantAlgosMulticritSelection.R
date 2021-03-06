# Function for the second selection - here the "best" subset of all remaining
# algorithms is selected.
#
# Output:
# Named list of length 2:
#    relevant.algos: Named logical vector - TRUE for selected algorithm
#    algo.contrs: Data.frame. One row for each subset of algorithms. Firts cols
#                 are logical - one for each algorithm, value is TRUE if algo
#                 is active in this subset. Last 2 rows are numeric. First is
#                 the number of algos in this subset, second one the optimality gap.

relevantAlgosMulticritSelection = function(data, var.cols, algo.col, repl.col,
  data.col, contrFun, w) {
 
  algos = as.character(sort(unique(data[, algo.col])))

  # Precalculate every combination of algos
  combs = as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), length(algos))))
  combs = combs[-nrow(combs), ]
  colnames(combs) = algos
  
  # Do for every data set
  oneDataSet = function(dat) {
    
    # a getIndicatorValue Function - gets a vector of algos and returns the indicator
    getIndicatorValue = function(algos) {
      # first get the eaf - but only of algos
      d = subset(dat, dat[, algo.col] %in% algos)
      front = eaf:::eafs(points = d[, var.cols], sets = d[, repl.col], percentiles = 50)[, 1:2]
      contrFun(front, ref.front)
    }
    
    # first, we need the reference front. this is the median eaf of all algos and repls
    ref.front = eaf:::eafs(points = dat[, var.cols],
      sets = dat[, repl.col], percentiles = 50)[, 1:2]
    
    # and compute its indicator value
    t(apply(combs, 1, function(inds) c(sum(inds), getIndicatorValue(algos[inds]))))
  }
  
  data.splitted = split(data, data[, data.col])
  
  on.data.contr = lapply(data.splitted, oneDataSet)
  
  # "Loss" of a portfolio is the mean of the loss over all data sets
  contr.vals = Reduce(`+`, on.data.contr) / length(unique(data[, data.col]))
  
  
  # normalize and augmented chebycheff of the 2 objective number of algos and indicator value
  contr.vals.norm = normalize(contr.vals, method = "range", margin = 2L)
  ws.part = w[1] * contr.vals.norm[, 1L] + w[2] * contr.vals.norm[, 2L]
  max.part = pmax(w[1] * contr.vals.norm[, 1L], w[2] * contr.vals.norm[, 2L])
  min.index = which.min(max.part + 0.05 * ws.part)
  
  contr.vals = data.frame(combs, contr.vals)
  colnames(contr.vals) = c(algos, "algo.count", "contribution")
  
  list(relevant.algos = combs[min.index, ], algo.contrs = contr.vals)
}
