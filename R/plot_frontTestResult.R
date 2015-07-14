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
  
  # Fifth Plot: Final Plot
  for (i in seq_along(sign.perm)) {
    checkPause()
    finalPlot(data, sign.perm[[i]], split.vals[[i]], var.cols, algo.col, repl.col)
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
  
  data.long = data.frame(
    iter = rep(colnames(data), each = nrow(data)),
    algo = rep(rownames(data), nrow(data)),
    contribution = as.vector(data)
  )
  data.min = data.frame(
    iter = 1:6,
    iter2 = 1:6 + 0.05,
    algo = rownames(data)[apply(data, 1, function(x) sum(!is.na(x)))],
    contribution = apply(data, 2, min, na.rm = TRUE)
  )
  
  p = ggplot2::ggplot(data.long, ggplot2::aes(iter, contribution))
  p = p + ggplot2::geom_point(size = 3)
  p = p + ggplot2::scale_y_log10()
  p = p + ggplot2::geom_line(data = data.min, size = 1, alpha = 0.5)
  p = p + ggplot2::geom_text(data = data.min, ggplot2::aes(label = algo, x = iter2),
    hjust = 0, vjust = 0, size = 7)
  #p = p + ggplot2::scale_x_continuous(breaks = data.long$id, labels = data.long$algo)
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

finalPlot = function(data, sign.perm, split.vals, var.cols, algo.col, repl.col) {
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
    #d = subset(d, d[, algo.col] %in% sign.perm)
    do.call(rbind, lapply(seq_along(sign.perm), function(i) {
      is.in = d[, var.cols[1]] > split.vals2[i] & d[, var.cols[1]] < split.vals2[i + 1]
      subset(d, d[, algo.col] %in% sign.perm[i] & is.in)
    }))
  }
  reduced.fronts = do.call(rbind, lapply(data.splitted, reduceData))
 
  colors = rainbow(length(sign.perm))
  new.formula = as.formula(sprintf("%s + %s ~ %s", var.cols[1L], var.cols[2L], repl.col))
  eafplot(new.formula, groups = get(algo.col), percentiles = 50, 
    data = reduced.fronts, xlab = var.cols[1], ylab = var.cols[2], col = colors) 
}

