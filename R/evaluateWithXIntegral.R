#' Measure for the comparison of a true and an estimated common pareto front
#' Only compares x-axes.
#'
#' @param landscape 
#'   Result of \code{generateParetoLandscape}.
#' @param portfolio  
#'   Result of \code{MOSAP::selectPortfolio}. 
#' 
#' @return [\code{numeric}]
#'  Value between 0 and 1.
#'  
#' @export
evaluateWithXIntegral = function(validation.obj, portfolio) {
  N = length(validation.obj$split.points) - 1
  n = length(portfolio$best.algo.order)
  
  true.split.points = validation.obj$split.points
  estimated.split.points = c(0, portfolio$split.vals, 1)
  
  rel.algo.names = validation.obj$algos
  estimated.algo.order = portfolio$best.algo.order
  
  res = 0
  
  for (i in 1:N) {
    for (j in 1:n) {
      if (rel.algo.names[i] == estimated.algo.order[j]) {
        l = max(true.split.points[i], estimated.split.points[j])
        u = min(true.split.points[i + 1], estimated.split.points[j + 1])
        res = res + ifelse(u - l > 0, u - l, 0)
      }
    }
  }
  return(res) 
}
