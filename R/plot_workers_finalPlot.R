

finalPlot = function(data, best.algo.order, split.vals, var.cols, algo.col, repl.col, data.col, colors) {
  
  d = portfolioToParetoFront(data, best.algo.order, split.vals, var.cols,
    algo.col, data.col, repl.col)
  
  p = ggplot(d, aes_string("V1", "V2", colour = "colour", group = "group")) +
    geom_line(size = 2) +
    xlab(var.cols[1L]) + ggplot2::ylab(var.cols[2L]) + 
    ggtitle("Reduced common Pareto front") + 
    scale_colour_manual(values = colors, name = algo.col) 
  return(p)
}
