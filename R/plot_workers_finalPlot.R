

finalPlot = function(data, best.algo.order, split.vals, var.cols, algo.col, repl.col, colors) {

  d = portfolioToParetoFront(data, best.algo.order, split.vals, var.cols, algo.col, repl.col)
  p = ggplot2::ggplot(d, ggplot2::aes_string("X1", "X2", colour = "colour", group = "group"))
  p = p + ggplot2::geom_line(size = 2)
  p = p + ggplot2::xlab(var.cols[1L]) + ggplot2::ylab(var.cols[2L])
  p = p + ggplot2::ggtitle("Reduced common Pareto front")
  p = p + ggplot2::scale_colour_manual(values = colors, name = algo.col)
  return(p)
}
