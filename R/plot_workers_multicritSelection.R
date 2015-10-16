# Fourth Plot: The Multicrit Selection ...
plotMulticritSelection = function(data, colors, w) {
  
  # data to add algorithm colored points
  nof.algos = max(data$algo.count)
  algos = colnames(data)[1:nof.algos]
  algo.data = do.call(rbind, lapply(1:nof.algos, function(i) {
    inds = data[, i]
    tmp = data[inds, c("algo.count", "contribution")]
    tmp[, 1] = tmp[, 1] + (i + 1) / (nof.algos + 3)
    tmp$algo = algos[i]
    tmp$points = ".3.algo.points"
    tmp
  }))
  
  multicrit.data = data[, nof.algos + 1:2]
  multicrit.data$points = ".1.sel.points"
  multicrit.data$algo = NA
  
  # mark the best point
  contr.vals.norm = normalize(data[, c("algo.count", "contribution")],
    method = "range", margin = 2L)
  ws.part = w[1L] * contr.vals.norm[, 1L] + w[2L] * contr.vals.norm[, 2L]
  max.part = pmax(w[1L] * contr.vals.norm[, 1L], w[2L] * contr.vals.norm[, 2L])
  min.index = which.min(max.part + 0.05 * ws.part)
  multicrit.data = rbind(multicrit.data, multicrit.data[min.index, ])
  multicrit.data[nrow(multicrit.data), "points"] = ".2.best.point"
  
  rbind.data = rbind(multicrit.data, algo.data)
  
  # Contour lines of the underlying scalarized prob
  # First make the grid in the "Plot-Space":
  # Note: We will apply a log scale later, so we need to make the grid in the
  # log scale
  cont.data = expand.grid(
    x = seq(min(data$algo.count) - 0.5, max(data$algo.count) + 0.5, length.out = 64),
    y = 10^seq(log10(sort(data$contribution)[2]) - 0.5, log10(max(data$contribution)) + 0.5, length.out = 64)
    #y = seq(min(data$contribution), max(data$contribution), length.out = 64)
  )
  # scale with respect to data, not to cont.data
  cont.data.norm = data.frame(
    x = (cont.data$x - min(data$algo.count)) / diff(range(data$algo.count)),
    y = (cont.data$y - min(data$contribution)) / diff(range(data$contribution))
  )
  # add z coloum from augmented tschebyscheff
  aug.tsch = function(x) 
    max(w * x) + 0.05 * sum(w * x)
  cont.data$z = apply(cont.data.norm, 1, aug.tsch)
  
  p = ggplot2::ggplot()
  p = p + ggplot2::stat_contour(ggplot2::aes(x = x, y = y, z = z), data = cont.data,
    breaks = 2^seq(-10, 0, length.out = 31), alpha = 0.7, color = "grey50")
  p = p + ggplot2::geom_point(ggplot2::aes_string("algo.count", "contribution",
    color = "algo", shape = "points", size = "points"), data = rbind.data)
  p = p + ggplot2::scale_y_log10()
  p = p + ggplot2::scale_colour_manual(values = c(colors),  na.value = "black",
    name = "Used Algorithms")
  labs = c(
    `.3.algo.points` = "\nSubset of algorithms\nused for the next selection\npoint to the left\n",
    `.1.sel.points` = "\nPoints used\nin multicrit selection\n",
    `.2.best.point` = "\nBest Subset with respect\nto the used scalarization\n"
  )
  p = p + ggplot2::scale_shape_manual(values = c(16, 10L, 15L),
    name = "Meaning of points", labels = labs)
  p = p + ggplot2::scale_size_manual(values = c(5, 9, 2.5),
    name = "Meaning of points",  labels = labs)
  p = p + ggplot2::scale_x_continuous(limits = c(0.5, NA_real_),
    breaks = 1:nof.algos)#function(x) pretty(x, n = min(5, nof.algos)))
  p = p + ggplot2::guides(
    linetype = ggplot2::guide_legend(order = 1L),
    shape = ggplot2::guide_legend(order = 2L),
    size = ggplot2::guide_legend(order = 2L),
    colour = ggplot2::guide_legend(order = 3L, override.aes = list(shape = 15L, size = 4))
  )
  p = p + ggplot2::ggtitle("Pareto front of number of algorithms and algorithm contribution")
  p = p + ggplot2::xlab("number of algorithms")
  p = p + ggplot2::ylab("optimality gap")
  return(p)
}
