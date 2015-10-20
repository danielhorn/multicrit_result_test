# Ground Zero Plot, Chaos Plot
# Plot The Raw Data, Color for algo, x and y for the variables and facet wrap for repl
plotChaosPlot = function(data, var.cols, algo.col, repl.col, colors) {
  # Split dataset into it replications
  data.splitted = split(data, data[, c(repl.col, algo.col)])
  
  # Apply Pareto-Filt
  data.splitted = lapply(data.splitted, function(d) {
    if (nrow(d) == 0L)
      d
    else
      d[nds_rank(as.matrix(t(d[, var.cols]))) == 1L, ]
  })
  
  # remerge data
  data = do.call(rbind, data.splitted)
  
  p = ggplot2::ggplot(data, ggplot2::aes_string(var.cols[1], var.cols[2], colour = algo.col))
  p = p + ggplot2::geom_point(size = 3)
  p = p + ggplot2::ggtitle("All Pareto Optimal Points")
  p = p + ggplot2::scale_colour_manual(values = colors)
  p = p + ggplot2::facet_wrap(reformulate(repl.col))
  return(p)
}

# First, Third and Fifth Plot: EAF plot.
plotEAF = function(data, var.cols, algo.col, repl.col, colors) {
  
  fronts = data[, c(var.cols, algo.col, repl.col)]
  fronts[, algo.col] = factor(fronts[, algo.col])
  fronts[, repl.col] = factor(fronts[, repl.col])
  
  p = eafGGPlot(data = fronts, var.names = var.cols, group.name = algo.col,
    replication.name = repl.col,  percentiles = 50)
  p = p + geom_line(size = 2) 
  p = p + scale_colour_manual(values = c(colors))
  p = p + guides(linetype = FALSE)
  return(p)
}

# Second Plot: Plot in how many repls an algo has non.dom points, via barplot
plotDominationSelection = function(data, colors, repl.count, algo.col, eta) {
  
  data.long = unlist(lapply(1:length(data), function(i) rep(names(data)[i], data[i])))
  data.long = data.frame(algo = factor(data.long, levels = names(data)))
  
  p = ggplot2::ggplot(data.long, aes_string("algo", fill = "algo"))
  p = p + ggplot2::geom_bar()
  p = p + ggplot2::scale_fill_manual(values = colors, drop = FALSE, name = algo.col)
  p = p + scale_x_discrete(drop = FALSE)
  p = p + ggplot2::geom_hline(yintercept = repl.count * eta, size = 1, alpha = 0.5)
  return(p)
}


