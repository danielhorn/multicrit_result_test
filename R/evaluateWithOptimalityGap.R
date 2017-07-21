#' Measure for the comparison of a true and an estimated common pareto front
#' Uses optimality gap (based on hypervolume).
#'
#' @param validation.obj
#'   Result of \code{generateParetoLandscape}.
#' @param portfolio  
#'   Result of \code{MOSAP::selectPortfolio}. 
#' 
#' @return [\code{numeric}]
#'  List of two optimality gap-based measures for each data set,
#'  values between 0 and 1.
#'  
#' @export
evaluateWithOptimalityGap = function(validation.obj, portfolio){
  landscapes = validation.obj$landscape.list
  args = portfolio$args
  data = args$data
  var.cols = args$var.cols
  repl.col = args$repl.col
  algo.col = args$algo.col
  best.algo.order = portfolio$best.algo.order
  split.vals = portfolio$split.vals
  
  eaf.data = portfolioToParetoFront(data, best.algo.order, split.vals, var.cols, algo.col, repl.col)
  
  
  
}