

getTestSplitVals = function(data, perm, var.cols, repl.col, algo.col) {
  # Split dataset into it replications
  data.splitted = split(data, data[, repl.col])
  
  # Apply Pareto-Filt
  data.splitted = lapply(data.splitted, function(d)
    d[nds_rank(as.matrix(t(d[, var.cols]))) == 1, ])
  
  # normalize to [0, 1] and we only need var value, algo and repl
  data.splitted = lapply(data.splitted, function(d)
    data.frame(
      value = normalize(d[, var.cols[1]], method = "range"),
      algo = d[, algo.col],
      repl = d[, repl.col]
    )
  )
  
  # For every repl: get the "border" indices if the point were sorted
  algo.borders = lapply(data.splitted, function(x) {
    res = cumsum(table(x[, "algo"])[perm])
    res[-length(res)]
  }
  )
  
  split.vals.repls = lapply(seq_along(data.splitted), function(i) {
    vec = data.splitted[[i]]$value
    vec = sort(vec)
    (vec[algo.borders[[i]]] + vec[algo.borders[[i]] + 1]) / 2
  }
  )
  drop(colMeans(convertListOfRowsToDataFrame(split.vals.repls)))
}
