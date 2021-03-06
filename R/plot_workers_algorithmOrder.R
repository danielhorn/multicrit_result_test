# Plot function to visualise the decision tree.
plotAlgorithmOrder = function(data, best.algo.order, split.vals,
  var.cols, algo.col, repl.col, data.col, colors.relevant.algos, colors.best.algo.order) {
  
  # Do for every data set
  oneDataSet = function(d) {
    # Split dataset into it replications
    data.splitted = split(d, d[, repl.col])
    
    # Apply Pareto-Filt
    data.splitted = lapply(data.splitted, function(d)
      d[nds_rank(as.matrix(t(d[, var.cols]))) == 1, ])
    
    # normalize to [0, 1] and we only need var value, algo and repl
    data.splitted = lapply(data.splitted, function(d)
      data.frame(
        value = normalize(d[, var.cols[1]], method = "range"),
        algo = factor(d[, algo.col], levels = unique(data[, algo.col])),
        repl = d[, repl.col]
      )
    )
    
    # now merge everything
    d2 = Reduce(rbind, data.splitted)
    d2[, data.col] = d[1, data.col]
    d2
  }
  data.long = do.call(rbind, lapply(split(data, data[, data.col]), oneDataSet))
  
  data.geom.rect = data.frame(
    xmin = c(0, split.vals),
    xmax = c(split.vals, 1),
    ymin = -Inf,
    ymax = Inf,
    predicted = factor(best.algo.order, levels = unique(data[, algo.col]))
  )
  
  # now the plotting
  p = ggplot(data.long, ggplot2::aes_string(x = "value", y = "repl", col = "algo")) + 
    geom_point(size = 4) + 
    xlab(paste("Normalized", var.cols[1])) + ggplot2::ylab("Replication") + 
    scale_y_continuous(breaks = 1:max(data[, repl.col])) + 
    scale_color_manual(values = colors.relevant.algos, name = "observed") + 
    facet_wrap("dataset") + 
    geom_rect(data = data.geom.rect,
    aes_string(xmin = "xmin", ymin = "ymin", xmax = "xmax", ymax = "ymax", fill = "predicted"),
    alpha = 0.1, inherit.aes = FALSE) +
    scale_fill_manual(values = colors.best.algo.order, name = "predicted")
  
  return(p)
}