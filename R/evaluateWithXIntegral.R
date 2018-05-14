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
  #N.simulated = length(validation.obj$split.points) + 1
  #N.selected = length(portfolio$best.algo.order)
  
  
  getMeasure = function(true.split.points, true.algo.names,
    estimated.split.points, estimated.algo.names) {
    # Add edge points
    true.split.points = c(0, true.split.points, 1)
    estimated.split.points = c(0, estimated.split.points, 1)
    
    N.true = length(true.split.points) - 1
    N.estimated = length(estimated.split.points) - 1
    
    # Calculate the X-Integrale
    res = 0
     for (i in 1:N.true) {
      for (j in 1:N.estimated) {
        if (true.algo.names[i] == estimated.algo.names[j]) {
          l = max(true.split.points[i], estimated.split.points[j])
          u = min(true.split.points[i + 1], estimated.split.points[j + 1])
          res = res + ifelse(u - l > 0, u - l, 0)
        }
      }
    }
    return(res)
  }
  
  # First: Global value
  global = getMeasure(
    validation.obj$split.points, validation.obj$algos,
    portfolio$split.vals, portfolio$best.algo.order
    )
  
  # Second: Dataset dependent in training set
  training = sapply(validation.obj$train.landscapes, function(landscape) {
    getMeasure(
      landscape$split.points, landscape$algo.order,
      portfolio$split.vals, portfolio$best.algo.order
    )
  })
  
  # third: Dataset dependent in test set
  test = sapply(validation.obj$test.landscapes, function(landscape) {
    getMeasure(
      landscape$split.points, landscape$algo.order,
      portfolio$split.vals, portfolio$best.algo.order
    )
  })
  
  return(list(global = global, training = training, test = test)) 
}
