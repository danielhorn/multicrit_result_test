#' plot test result
#' 
#' 
#' @export
plot.frontTestResult = function(x, make.pause = TRUE, colors = NULL) {
  requirePackages(c("ggplot2", "eaf"))
  
  checkPause = function()
    if (make.pause) pause()
  
  # extract some stuff
  data = x$args$data
  formula = x$args$formula
  algo.col = as.character(formula[[2]])
  var.cols = as.character(formula[[3]][[2]])[-1]
  repl.col = as.character(formula[[3]][[3]])
  
  repls = length(unique((data[, repl.col])))
  
  algos = unique(data[, algo.col])
  non.dom.algos = names(which(x$non.dominated.algos))
  relevant.algos = names(which(x$relevant.algos))
  
  best.algo.order = x$best.algo.order
  split.vals = x$split.vals
  
  # First: Fix colors for algos!
  if (is.null(colors))
    colors = rainbow(length(algos))
  
  # Ground Zero: Chaos Plot
  plotChaosPlot(data, var.cols, algo.col, repl.col, colors)
  checkPause()
  
  # First Plot: EAF of everything
  plotEAF(data, formula, colors)
  checkPause()
  
  # Second Plot: Remove dominated algorithms
  plotDominationSelection(data = x$algos.domination.count, colors = colors, 
    repl.count = repls)
  checkPause()
  
  # Third Plot: EAF off all non-dominated algos
  colors.non.dom.algos = colors[algos %in% non.dom.algos]
  data = subset(data, data[, algo.col] %in% non.dom.algos)
  plotEAF(data, x$args$formula, colors.non.dom.algos)
  checkPause()
  
  # 4th Plot: Multicrit Selection of relevant algos
  plotMulticritSelection(data = x$algos.selection.vals, colors = colors.non.dom.algos)
  checkPause()
  
  # 5th Plot: EAF off all remaining algos
  colors.relevant.algos = colors[algos %in% relevant.algos]
  data = subset(data, data[, algo.col] %in% relevant.algos)
  plotEAF(data, x$args$formula, colors.relevant.algos)
  checkPause()
 
  # 6th Plot: Show permutation.
  colors.best.order = colors[algos %in% best.algo.order]
  plotAlgorithmOrder(data, best.algo.order, x$split.vals,
    var.cols, algo.col, repl.col, colors.relevant.algos, colors.best.order)
  checkPause()
  
  # 7th Plot: Final Plot with final reduced common Pareto front
  finalPlot(data, best.algo.order, split.vals, var.cols, algo.col, repl.col, 
    colors.best.order)
  
  return(invisible(NULL))
}


# Ground Zero Plot, Chaos Plot
# Plot The Raw Data, Color for algo, x and y for the variables and facet wrap for repl
plotChaosPlot = function(data, var.cols, algo.col, repl.col, colors) {
  # Split dataset into it replications
  data.splitted = split(data, data[, c(repl.col, algo.col)])
  
  # Apply Pareto-Filt
  data.splitted = lapply(data.splitted, function(d)
    d[nds_rank(as.matrix(t(d[, var.cols]))) == 1L, ])
  
  # remerge data
  data = do.call(rbind, data.splitted)
  
  p = ggplot2::ggplot(data, ggplot2::aes_string(var.cols[1], var.cols[2], colour = algo.col))
  p = p + ggplot2::geom_point(size = 3)
  p = p + ggplot2::ggtitle("All Pareto Optimal Points")
  p = p + ggplot2::scale_colour_manual(values = colors)
  p = p + ggplot2::facet_wrap(reformulate(repl.col))
  print(p)
}

# Second Plot: Plot in how many repls an algo has non.dom points, via barplot

plotDominationSelection = function(data, colors, repl.count) {
  
  data.long = unlist(lapply(1:length(data), function(i) rep(names(data)[i], data[i])))
  data.long = data.frame(algo = factor(data.long, levels = names(data)))
  
  p = ggplot2::ggplot(data.long, aes(algo, fill = algo))
  p = p + ggplot2::geom_bar()
  p = p + ggplot2::scale_fill_manual(values = colors, drop = FALSE)
  p = p + scale_x_discrete(drop = FALSE)
  p = p + ggplot2::geom_hline(yintercept = repl.count / 2, size = 1, alpha = 0.5)
  print(p)
}

# Fourth Plot: The Multicrit Selection ...
plotMulticritSelection = function(data, colors) {
  
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
  contr.vals.norm = normalize(data[, c("algo.count", "contribution")], method = "range", margin = 2L)
  ws.part = 0.05 * contr.vals.norm[, 1L] + 0.95 * contr.vals.norm[, 2L]
  max.part = pmax(0.05 * contr.vals.norm[, 1L], 0.95 * contr.vals.norm[, 2L])
  min.index = which.min(max.part + 0.05 * ws.part)
  multicrit.data[min.index, "points"] = ".2.best.point"
  
  rbind.data = rbind(multicrit.data, algo.data)
  
  # data to add the line of the weighted sum. first in the normalized space
  # line should end at nof.algos + 1!
  line.data = data.frame(
    x = seq(0, (nof.algos + 1) / nof.algos , length.out = 129L),
    y = seq(0, 0.05 / 0.95 * (nof.algos + 1) / nof.algos , length.out = 129L)
    )
  line.data = line.data[-129L, ]
  
  # now transform to the real data space
  line.data$x = line.data$x * diff(range(data$algo.count)) + min(data$algo.count)
  line.data$y = line.data$y * diff(range(data$contribution)) + min(data$contribution)
  line.data$line = "Scalar Problem"
  
  p = ggplot2::ggplot()
#  p = p + ggplot2::geom_point(ggplot2::aes_string("algo.count", "contribution"),
#    data = best.point, size = 8, color = "grey50")
  p = p + ggplot2::geom_point(ggplot2::aes_string("algo.count", "contribution",
    color = "algo", shape = "points", size = "points"), data = rbind.data)
  #p = p + ggplot2::geom_point(ggplot2::aes_string("algo.count", "contribution"),
  #  data = data, size = 3)
  #p = p + ggplot2::geom_point(ggplot2::aes_string("algo.count", "contribution", color = "algo"),
  #  data = algo.data)
  p = p + ggplot2::geom_line(ggplot2::aes_string(x = "x", y = "y",
    linetype = "line"), data = line.data)
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
  p = p + ggplot2::scale_size_manual(values = c(5, 7, 2.5),
    name = "Meaning of points",  labels = labs)
  p = p + ggplot2::scale_linetype_manual(values = "solid", name = "Meaning of line")
  p = p + ggplot2::scale_x_continuous(limits = c(0.5, NA_real_),
    breaks = 1:nof.algos)#function(x) pretty(x, n = min(5, nof.algos)))
  p = p + ggplot2::guides(
    linetype = ggplot2::guide_legend(order = 1L),
    shape = ggplot2::guide_legend(order = 2L),
    size = ggplot2::guide_legend(order = 2L),
    colour = ggplot2::guide_legend(order = 3L, override.aes = list(shape = 15L, size = 4))
    )
  p = p + ggplot2::ggtitle("Pareto Front of number of algorithms and algorithm contribution")
  p = p + ggplot2::xlab("Number of algorithms")
  print(p)
}

# First, Third and Fifth Plot: EAF plot.
plotEAF = function(data, formula, colors) {
  # build new formula for eaf
  algo = as.character(formula[[2]])
  repl = as.character(formula[[3]][[3]])
  vars = as.character(formula[[3]][[2]])[-1]
  new.formula = as.formula(sprintf("%s + %s ~ %s", vars[1L], vars[2L], repl))
  
  fronts = data[, c(vars, algo, repl)]
  fronts[, algo] = factor(fronts[, algo])
  fronts[, repl] = factor(fronts[, repl])
  
  eafplot(new.formula, groups = get(algo), percentiles = 50, 
    data = fronts, xlab = vars[1], ylab = vars[2], col = colors, lwd = 2.75)
}

# Plot function to visualise the decision tree.
plotAlgorithmOrder = function (data, best.algo.order, split.vals,
  var.cols, algo.col, repl.col, colors.relevant.algos, colors.best.algo.order) {  
  # Split dataset into it replications
  data.splitted = split(data, data[, repl.col])
  
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
  data.long = Reduce(rbind, data.splitted)
  
  data.geom.rect = data.frame(
    xmin = c(0, split.vals),
    xmax = c(split.vals, 1),
    ymin = -Inf,
    ymax = Inf,
    predicted = factor(best.algo.order, levels = unique(data[, algo.col]))
  )
  
  # now the plotting
  p = ggplot2::ggplot(data.long, ggplot2::aes(x = value, y = repl, col = algo))
  p = p + ggplot2::geom_point(size = 4)
  p = p + ggplot2::xlab(paste("Normalized", var.cols[1])) + ggplot2::ylab("Replication")
  p = p + ggplot2::scale_y_continuous(breaks = 1:max(data[, repl.col]))
  p = p + ggplot2::scale_color_manual(values = colors.relevant.algos, name = "observed")
  p = p + geom_rect(data = data.geom.rect,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = predicted),
    alpha = 0.1, inherit.aes = FALSE)
  p = p + ggplot2::scale_fill_manual(values = colors.best.algo.order, name = "predicted")
  
  print(p)
}

# Final Plot for the common reduced Pareto front
finalPlot = function(data, best.algo.order, split.vals, var.cols, algo.col, repl.col, colors) {
  split.vals = c(0, split.vals, 1)
  data.splitted = split(data, data[, repl.col])
  
  # Apply Pareto-Filt
  data.splitted = lapply(data.splitted, function(d)
    d[nds_rank(as.matrix(t(d[, var.cols]))) == 1, ])
  
  # Each algo is only interesting in its split.vals. 
  reduceData = function(d) {
    min.val = min(d[, var.cols[1]])
    max.val = max(d[, var.cols[1]])
    split.vals2 = min.val + split.vals * (max.val - min.val)
    #d = subset(d, d[, algo.col] %in% best.algo.order)
    do.call(rbind, lapply(seq_along(best.algo.order), function(i) {
      is.in = d[, var.cols[1]] > split.vals2[i] & d[, var.cols[1]] < split.vals2[i + 1]
      subset(d, d[, algo.col] %in% best.algo.order[i] & is.in)
    }))
  }
  reduced.fronts = do.call(rbind, lapply(data.splitted, reduceData))
  reduced.fronts[, algo.col] = factor(reduced.fronts[, algo.col])
 
  # Hack the EAF package - use the underlying eaf function to get points
  # for my own eaf plotting
  d = eaf:::eafs(points = reduced.fronts[, var.cols],
    sets = reduced.fronts[, repl.col], groups = reduced.fronts[, algo.col], percentiles = 50)
  d = d[order(d[, 1L]), ]
  # I need another pareto filter now
  d = d[!is_dominated(t(d[, 1:2])), ]
  
  # now it gets a little bit crazy ... add "middlepoints" between groups
  # so a nice colored line can be plotted
  # only necessary if there is more than 1 relevant algo
  if (length(best.algo.order) > 1L)  {
    # first, get "border" points. works since d is orderd
    borders = which(d[-1L, 4L]  != d[-nrow(d), 4L])
    # now, for each border, add 2 identical points, with mean of border and border + 1
    # with different algos, this means different colours
    new.points = (d[borders, 1:2] + d[borders + 1L, 1:2]) / 2L
    new.points = data.frame(
      X1 = rep(new.points$X1, each = 2L),
      X2 = rep(new.points$X2, each = 2L),
      X3 = 50,
      groups = d[sort(c(borders, borders + 1L)), 4L]
    )
    d = rbind(d, new.points)
    d = d[order(d[, 1L]), ]
  }
  
  # another nasty hack. if an algo has 2 non-connected parts of the front ...
  # we have to assign 2 different group levels for ggplot
  last.els.per.algo = c(0, which(d[-nrow(d), 4] != d[-1, 4]), nrow(d))
  # now give a unique id for every part
  counts.per.part = diff(last.els.per.algo)
  ids = unlist(lapply(seq_along(counts.per.part), function(i) rep(i, counts.per.part[i])))
  # an add this id to the group coloum
  d$group = factor(paste(d[, 4], ids))
  names(d)[4] = "colour"
  
  p = ggplot2::ggplot(d, ggplot2::aes_string("X1", "X2", colour = "colour", group = "group"))
  p = p + ggplot2::geom_line(size = 2)
  p = p + ggplot2::xlab(var.cols[1L]) + ggplot2::ylab(var.cols[2L])
  p = p + ggplot2::ggtitle("Reduced common Pareto front")
  p = p + ggplot2::scale_colour_manual(values = colors)
  print(p)
}
