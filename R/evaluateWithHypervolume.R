#' Measure for the comparison of a true and an estimated common pareto front.
#' For each dataset, the ratio of the hypervolume of the selected portfolio
#' and the best portfolio for this data set is calculated. Additionaly,
#' the ratio of the global optimal portfolio is also returned.
#'
#' @param validation.obj
#'   Result of \code{generateParetoLandscape}.
#' @param portfolio  
#'   Result of \code{MOSAP::selectPortfolio}.
#' 
#' @return [\code{numeric}]
#' TODO
#'  
#' @export
evaluateWithHypervolume = function(validation.obj, portfolio){
  
  # For a given landscape and a portfolio, calculate the HV ratio
  getHypervolumeRatio = function(landscape, algos, split.points) {
    f.list = landscape$f.list
    algo.names = sapply(f.list, getAlgoID)
    
    # First: Function that describes the common front for this data set
    front.best = Vectorize(function(x) {
      # Which algorithm is on the common front for the given x?
      active.algo = landscape$algo.order[sum(x > landscape$split.points) + 1]
      # Select and evaluate the correct front function
      return(f.list[[which(algo.names == active.algo)]](x))
    })
    
    # Second: Function that describes the Pareto front of the portfolio
    front.selected = Vectorize(function(x) {
      # Which algorithm is on the common front for the given x?
      active.algo = algos[sum(x > split.points) + 1]
      # Select and evaluate the correct front function
      return(f.list[[which(algo.names == active.algo)]](x))
    })
    
    # Ganz viele Punkte auf die Fronten sampeln und da HV bestimmen
    x = seq(0, 1, length.out = 1048)
    hv.best = dominated_hypervolume(rbind(x, front.best(x)), ref = c(1.1, 1.1))
    hv.sel = dominated_hypervolume(rbind(x, front.selected(x)), ref = c(1.1, 1.1))
    
    return(hv.sel / hv.best)
  }
  
  train.ratios = sapply(validation.obj$train.landscapes, function(landscape) {
    return(c(
      true = getHypervolumeRatio(landscape, validation.obj$algos, validation.obj$split.points),
      selected = getHypervolumeRatio(landscape, portfolio$best.algo.order, portfolio$split.vals)
    ))
  })
  colnames(train.ratios) = 1:ncol(train.ratios)
  
  test.ratios = sapply(validation.obj$test.landscapes, function(landscape) {
    return(c(
      true = getHypervolumeRatio(landscape, validation.obj$algos, validation.obj$split.points),
      selected = getHypervolumeRatio(landscape, portfolio$best.algo.order, portfolio$split.vals)
    ))
  })
  colnames(test.ratios) = 1:ncol(test.ratios)
  
  return(list(
    train.ratios = cbind(train.ratios, mean = rowMeans(train.ratios)),
    test.ratios = cbind(test.ratios, mean = rowMeans(test.ratios))
  ))
}