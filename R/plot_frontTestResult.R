#' render test result plots (without eaf plots)
#'
#' @param x [\code{frontTestResult}]\cr
#'   Result object from function \link{mainTestProcedure}.
#' @param colors [\code{character}] \cr
#'   Vector of colors for plotting. Length must be equal to number of algorithms.
#' 
#' @export
renderFrontTestResult = function(x, colors = NULL) {
  requirePackages(c("ggplot2", "eaf"))
  
  assertClass(x, "frontTestResult")
  assertCharacter(colors, len = length(algos))
  
  # extract some stuff
  data = x$args$data
  algo.col = x$args$algo.col
  var.cols = x$args$var.cols
  repl.col = x$args$repl.col
  
  repls = length(unique((data[, repl.col])))
  
  algos = unique(data[, algo.col])
  non.dom.algos = names(which(x$non.dominated.algos))
  relevant.algos = names(which(x$relevant.algos))
  
  best.algo.order = x$best.algo.order
  split.vals = x$split.vals
  
  # First: Fix colors for algos!
  if (is.null(colors))
    colors = rainbow(length(algos))
  else
    assertCharacter(colors, len = length(algos))
  
  plots = list()
  
  # Ground Zero: Chaos Plot
  plots[[1]] = plotChaosPlot(data, var.cols, algo.col, repl.col, colors)
  
  # First Plot: EAF of everything
  plots[[2]] = plotEAF(data, var.cols, algo.col, repl.col, colors)

  # Second Plot: Remove dominated algorithms
  plots[[3]] = plotDominationSelection(data = x$algos.domination.count, 
    colors = colors, repl.count = repls, algo.col = algo.col, eta = x$args$eta)

  # Third Plot: EAF off all non-dominated algos
  colors.non.dom.algos = colors[algos %in% non.dom.algos]
  data = subset(data, data[, algo.col] %in% non.dom.algos)
  plots[[4]] = plotEAF(data, var.cols, algo.col, repl.col, colors.non.dom.algos)

  # 4th Plot: Multicrit Selection of relevant algos
  plots[[5]] = plotMulticritSelection(data = x$algos.selection.vals, 
    colors = colors.non.dom.algos, w = x$args$w)

  # 5th Plot: EAF off all remaining algos
  colors.relevant.algos = colors[algos %in% relevant.algos]
  data = subset(data, data[, algo.col] %in% relevant.algos)
  plots[[6]] = plotEAF(data, var.cols, algo.col, repl.col, colors.relevant.algos)

  ## 6th Plot: Show permutation.
  colors.best.order = colors[algos %in% best.algo.order]
  #plots[[7]] = plotAlgorithmOrder(data, best.algo.order, split.vals,
  #  var.cols, algo.col, repl.col, colors.relevant.algos, colors.best.order)

  # 7th Plot: Final Plot with final reduced common Pareto front
  plots[[7]] = finalPlot(data, best.algo.order, split.vals, var.cols, algo.col, 
    repl.col, colors.best.order)
  
  return(plots)
}

#' Plots any MBO result objects. Plots for X-Space, Y-Space and any coloumn in
#' the optimization path are available. This function uses 
#' 
#' 
#' @param x [\code{frontTestResult}]\cr
#'   Result object from function \link{mainTestProcedure}.
#' @param make.pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @param ... [\code{any}] \cr
#'   Not used.
#' @return Nothing.
#'   
#' @export

plot.frontTestResult = function(x, make.pause = TRUE, ...) {
  requirePackages(c("ggplot2", "eaf"))
  
  checkPause = function()
    if (make.pause) pause()
  
  plots = renderFrontTestResult(x, ...)
  
  nof.plots = length(plots)
  for (i in 1:nof.plots) {
    print(plots[[i]])
    if (i != nof.plots) checkPause()
  }

  return(invisible(NULL))
}
