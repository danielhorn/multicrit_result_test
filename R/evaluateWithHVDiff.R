#' Measure for the comparison of a true and an estimated common pareto front
#' Uses optimality gap (based on hypervolume).
#'
#' @param validation.obj
#'   Result of \code{generateParetoLandscape}.
#' @param portfolio  
#'   Result of \code{MOSAP::selectPortfolio}.
#' 
#' @return [\code{numeric}]
#'  Value between 0 and 1: Total difference of dominated hypervolume for selected set of algorithms with
#'  optimal set of algorithms for data set.
#'  
#' @export
evaluateWithHVDiff = function(validation.obj, portfolio){
  
  # First: extract a function from the estimated portfolio, that represents
  # the common Pareto fron based on the optimal algorithms and split points from
  # the portfolio
  
  # Extract the list of front function from the validation object
  landscape = validation.obj$landscape.list[[data.set]]
  f.list = landscape$f.list
  
  # Get algos and splits from portfolio
  best.algo.order = portfolio$best.algo.order
  split.vals = portfolio$split.vals
  
  # Reduce the list to only relevant function
  f.sub = f.list[best.algo.order]
  
  ## define the function
  f.portfolio = function(x) {
    # Based on x, select the correct function
    selected.f = f.sub[[1 + sum(x > split.vals)]]
    # And evaluate
    return(selected.f(x))
  }
  f.portfolio = Vectorize(f.portfolio)
  
  # Second: True common front function from the validation object
  
  # As in the portfolio, reduce the list of function to only the optimal ones
  f.paretoOpt = f.list[sapply(f.list, isAlgoParetoOpt)]
  # Sortieren, so dass aufsteigend in der x-Achse
  f.paretoOpt = f.paretoOpt[order(sapply(f.paretoOpt, getAlgoRangeX)[1, ])]
  # Wahre Split extrahieren
  true.splits = landscape$split.points
  
  f.optimal = function(x) {
    # Based on x, select the correct function
    selected.f = f.paretoOpt[[1 + sum(x > true.splits)]]
    # And evaluate
    return(selected.f(x))
  }
  f.optimal = Vectorize(f.optimal)
  
  # interceptions berechnen zw. f.portfolio und f.optimal (mit n = 100 Intervallen)
  intercepts = uniroot.all(function(x) abs(f.portfolio(x) - f.optimal(x)), c(0,1))
  # 0 und 1 als Randfälle anhängen
  intercepts = c(0, intercepts, 1.1)
  
  raster.seq = seq(0, 1, length.out = 1e5)
  hv.diff.sum = 0
  
  for(i in seq_along(intercepts)) {
    temp.raster = raster.seq[which(raster.seq >= intercepts[i] & raster.seq < intercepts[i + 1])]
    
    temp.y.pf = f.portfolio(temp.raster)
    temp.y.opt = f.optimal(temp.raster)
    
    hv.portfolio = dominated_hypervolume(points = rbind(temp.raster, temp.y.pf), ref = c(1,1))
    hv.optimal = dominated_hypervolume(points = rbind(temp.raster, temp.y.opt), ref = c(1,1))
    
    hv.diff.tmp = abs(hv.portfolio - hv.optimal)
    hv.diff.sum = hv.diff.sum + hv.diff.tmp
  }
  
  return(hv.diff.sum)
  }