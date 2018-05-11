# First selection step: select all algorithms that have non-dominated points in
# more than eta replications
#
# Output:
# Named list of length 2:
#    relevant.algos: Named logical vector - TRUE for selected algorithm
#    counts: Named numeric vector - number of replications with non-dominated
#            points for each algorithm

relevantAlgosDominationSelection = function(data, var.cols, algo.col, repl.col, data.col, eta, ny) {
  
  n.repl = max(data[, repl.col])
  n.data = length(unique(data[, data.col]))
  
  #Calculate for one data set and each repl, if an algorithm had non-dominated points
  oneDataSet = function(d) {
    d.splitted = split(d, d[, c(repl.col)])
    
    d.splitted = lapply(d.splitted, function(tmp)
      tmp[nds_rank(as.matrix(t(tmp[, var.cols]))) == 1L, ])
    
    non.dom.algos = lapply(d.splitted, function(tmp) unique(tmp[, algo.col]))
    
    # Make a Table of it
    table(unlist(non.dom.algos))
  }
  
  # apply over all data sets
  counts = sapply(split(data, data[, c(data.col)]), oneDataSet)
  
  # An algorithm is relevant on one data set, if it has non-dominated points
  # in more than eta replications.
  # A data set is over all relevant, if he is relevant on ny data sets
  relevant = apply(counts, 2, function(x) x >= n.repl * eta)
  
  relevant.algos = rowSums(relevant) >= n.data * ny
  
  list(relevant.algos = relevant.algos, counts = counts)
}
