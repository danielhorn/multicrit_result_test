#' plot test result
#' 
#' 
#' @export
plot.frontTestResult = function(x, make.pause = TRUE) {
  requirePackages(c("ggplot2", "eaf"))
  
  checkPause = function()
    if (make.pause) pause()
  
  # extract some stuff
  data = x$args$data
  formula = x$args$formula
  front.contribution = x$front.contribution
  algo.col = as.character(formula[[2]])
  var.cols = as.character(formula[[3]][[2]])[-1]
  repl.col = as.character(formula[[3]][[3]])
  relevant.algos = names(which(x$relevant.algos))
  split.vals = x$split.vals
  sign.perm = x$significant.permutations
  if (!is.list(split.vals))
    split.vals = list(split.vals)
  if (!is.list(sign.perm))
    sign.perm = list(sign.perm)
  
  
  # First Plot: EAF of everything
  plotEAF(x$args$data, x$args$formula)
  checkPause()
  
  # Second Plot: Show Front contribution of all algos
  if (x$args$sel.fun == "ind")
    plotRelevantAlgos(data = x$front.contribution, kappa = x$args$kappa)
  if (x$args$sel.fun == "forward")  
    plotForwardSelection(data = x$front.contribution, kappa = x$args$kappa)
      
  if (length(relevant.algos) == 0L)
    return(invisible(NULL))
  
  checkPause()
  # Third Plot: EAF of only relevant algorithms
  data = subset(data, data[, algo.col] %in% relevant.algos)
  plotEAF(data, x$args$formula)
  
  # Fourth Plot: Show permutation.
  for (i in seq_along(sign.perm)) {
    checkPause()
    plotAlgorithmOrder(data, sign.perm[[i]], split.vals[[i]], var.cols, algo.col, repl.col)
  }
  return(invisible(NULL))
}


plotRelevantAlgos = function(data, kappa) {
  data.long = data.frame(
    algo = rep(colnames(data), each = nrow(data)),
    contribution = as.vector(as.matrix(data))
    )
  
  p = ggplot2::ggplot(data.long, ggplot2::aes(algo, contribution))
  p = p + ggplot2::geom_boxplot()
  p = p + scale_y_log10()
  p = p + ggplot2::ggtitle("Contribution of algorithms to common pareto front")
  p = p + ggplot2::geom_hline(yintercept = kappa)
  print(p)
}

plotForwardSelection = function(data, kappa) {
  
  perm = order(data, decreasing = TRUE)
  
  data.long = data.frame(
    id = seq_along(data),
    algo = names(data)[perm],
    contribution = data[perm]
    )
  p = ggplot2::ggplot(data.long, ggplot2::aes(id, contribution))
  p = p + ggplot2::geom_line(size = 1, alpha = 0.5)
  p = p + ggplot2::geom_text(ggplot2::aes(label = algo), hjust = 0,
    vjust = 0, size = 6)
  p = p + ggplot2::scale_x_continuous(breaks = data.long$id, labels = names(data))
  p = p + ggplot2::geom_hline(yintercept = kappa, size = 1, alpha = 0.5)
  print(p)
}

plotEAF = function(data, formula) {
  
  # build new formula for eaf
  algo = as.character(formula[[2]])
  repl = as.character(formula[[3]][[3]])
  vars = as.character(formula[[3]][[2]])[-1]
  new.formula = as.formula(sprintf("%s + %s ~ %s", vars[1L], vars[2L], repl))
  
  fronts = data[, c(vars, algo, repl)]
  fronts[, algo] = factor(fronts[, algo])
  fronts[, repl] = factor(fronts[, repl])
  
  colors = rainbow(length(levels(fronts[, algo])))
  eafplot(new.formula, groups = get(algo), percentiles = 50, 
    data = fronts, xlab = vars[1], ylab = vars[2], col = colors)
}



plotAlgorithmOrder = function (data, sign.perm, split.vals, var.cols, algo.col, repl.col) {  
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
  
  # now merge everything
  data.long = Reduce(rbind, data.splitted)
  
  data.geom.rect = data.frame(
    xmin = c(0, split.vals),
    xmax = c(split.vals, 1),
    ymin = -Inf,
    ymax = Inf,
    predicted = sign.perm
  )
  

  
  colors = rainbow(length(unique(data[, algo.col])))
  
  # now the plotting
  p = ggplot2::ggplot(data.long, ggplot2::aes(x = value, y = repl, col = algo))
  p = p + ggplot2::geom_point(size = 4)
  p = p + ggplot2::xlab(paste("Normalized", var.cols[1])) + ggplot2::ylab("Replication")
  p = p + ggplot2::scale_y_continuous(breaks = 1:max(data[, repl.col]))
  p = p + ggplot2::scale_color_manual(values = colors, name = "observed")
  p = p + geom_rect(data = data.geom.rect,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = predicted),
    alpha = 0.1, inherit.aes = FALSE)
  p = p + ggplot2::scale_fill_manual(values = colors[which(unique(data[, algo.col]) %in% sign.perm)], name = "predicted")
  
  print(p)
}


