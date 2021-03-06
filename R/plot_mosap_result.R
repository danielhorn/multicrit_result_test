#' Render plots for a frontTestResult Object. In contrary to the plot-function
#' the redner function does not print but return the ggplot-objects.
#'
#' @param x [\code{mosap_result}]\cr
#'   Result object from function \link{selectPortfolio}.
#' @param colors [\code{character}] \cr
#'   Vector of colors for plotting. Length must be equal to number of algorithms.
#' 
#' @return [\code{list(7)}]
#'   List of 7 ggplot objects - each vizualising one step of the procedure:
#'   \itemize{
#'   \item{1} Raw data as scatter plots - one scatter plot per replication
#'   \item{2} EAF plot of all data points
#'   \item{3} Bar plot - in yhow many replications does each algorithm have non-
#'     dominated points?
#'   \item{4} EAF plot of all remaining algorithms
#'   \item{5} Plot of the multicrit algorithm selection
#'   \item{6} EAF plot of all remaining algorithms
#'   \item{7} Plot of the final common Pareto front
#' }
#' 
#' @export
renderFrontTestResult = function(x, colors = NULL) {
  requirePackages(c("ggplot2", "eaf"))
  
  assertClass(x, "mosap_result")
  
  # extract some stuff
  data = x$args$data
  algo.col = x$args$algo.col
  var.cols = x$args$var.cols
  repl.col = x$args$repl.col
  data.col = x$args$data.col
  
  repls = length(unique((data[, repl.col])))
  
  # Normalize Data par data set
  if (x$args$normalize) {
    splitted = split(data[, var.cols], data[, data.col])
    normalized = lapply(splitted, function(d) 
      normalize(d[, var.cols], method = "range", range = c(0, 1))
    )
    data[, var.cols] = do.call(rbind, normalized)
  }
  
  algos = unique(sort(data[, algo.col]))
  non.dom.algos = names(which(x$non.dominated.algos))
  
  best.algo.order = x$best.algo.order
  split.vals = x$split.vals
  
  if (is.null(colors))
    colors = rainbow(length(algos))
  else
    assertCharacter(colors, len = length(algos))
  
  plots = list()
  
  # Ground Zero: Chaos Plot
  plots[[1L]] = plotChaosPlot(data, var.cols, algo.col, repl.col, data.col, colors)
  
  # First Plot: EAF of everything
  plots[[2L]] = plotEAF(data, var.cols, algo.col, repl.col, data.col, colors)

  # Second Plot: Remove dominated algorithms
  plots[[3L]] = plotDominationSelection(data = x$algos.domination.count, 
    colors = colors, repl.count = repls, algo.col = algo.col, eta = x$args$eta)

  # Third Plot: EAF off all non-dominated algos
  colors.non.dom.algos = colors[algos %in% non.dom.algos]
  data = subset(data, data[, algo.col] %in% non.dom.algos)
  plots[[4L]] = plotEAF(data, var.cols, algo.col, repl.col, data.col, colors.non.dom.algos)

  # 4th Plot: Multicrit Selection of relevant algos
  if (!is.null(x$algos.selection.vals))
    plots[[5L]] = plotMulticritSelection(data = x$algos.selection.vals, 
      colors = colors.non.dom.algos, w = x$args$w)

  # 5th Plot: EAF off all remaining algos
  if (!is.null(x$relevant.algos)) {
    relevant.algos = names(which(x$relevant.algos))
    colors.relevant.algos = colors[algos %in% relevant.algos]
    data = subset(data, data[, algo.col] %in% relevant.algos)
    plots[[6L]] = plotEAF(data, var.cols, algo.col, repl.col, data.col, colors.relevant.algos)
  }

  ## 6th Plot: Show permutation.
  colors.best.order = colors[algos %in% best.algo.order]
  plots[[7]] = plotAlgorithmOrder(data, best.algo.order, split.vals,
    var.cols, algo.col, repl.col, data.col, colors.relevant.algos, colors.best.order)

  # 7th Plot: Final Plot with final reduced common Pareto front
  plots[[8L]] = finalPlot(data, best.algo.order, split.vals, var.cols, algo.col, 
    repl.col, data.col, colors.best.order)
  
  return(plots)
}

#' Plot-function for \code{frontTestResult}-objects. 
#' 
#' @param x [\code{mosap_result}]\cr
#'   Result object from function \link{selectPortfolio}.
#' @param make.pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @param ... [\code{any}] \cr
#'   Not used.
#' @return Nothing.
#'   
#' @export

plot.mosap_result = function(x, make.pause = TRUE, ...) {
  requirePackages(c("ggplot2", "eaf"))
  
  assertFlag(make.pause)
  
  checkPause = function()
    if (make.pause) pause()
  
  plots = renderFrontTestResult(x, ...)
  
  nof.plots = length(plots)
  for (i in 1:nof.plots) {
    if (!is.null(plots[[i]])) {
      print(plots[[i]])
      if (i != nof.plots) checkPause()
    }

  }

  return(invisible(NULL))
}
