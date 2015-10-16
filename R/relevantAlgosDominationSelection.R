# First selection step: select all algorithms that have non-dominated points in
# more than eta replications
#
# Output:
# Named list of length 2:
#    relevant.algos: Named logical vector - TRUE for selected algorithm
#    counts: Named numeric vector - number of replications with non-dominated
#            points for each algorithm

relevantAlgosDominationSelection = function(data, var.cols, algo.col, repl.col, eta) {

  data.splitted = split(data, data[, repl.col])
  
  data.splitted = lapply(data.splitted, function(d)
    d[nds_rank(as.matrix(t(d[, var.cols]))) == 1L, ])
  
  non.dom.algos = lapply(data.splitted, function(d) unique(d[, algo.col]))
  
  counts = table(unlist(non.dom.algos))
  
  relevant.algos = counts > length(data.splitted) * eta
  
  list(relevant.algos = relevant.algos, counts = counts)
}
