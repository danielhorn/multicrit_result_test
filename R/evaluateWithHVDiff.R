#' Measure for the comparison of a true and an estimated common pareto front
#' Uses optimality gap (based on hypervolume).
#'
#' @param validation.obj
#'   Result of \code{generateParetoLandscape}.
#' @param portfolio  
#'   Result of \code{MOSAP::selectPortfolio}. 
#' @param data.set
#'  Calculate measure for given data set
#' 
#' @return [\code{numeric}]
#'  Value between 0 and 1: Total difference of dominated hypervolume for selected set of algorithms with
#'  optimal set of algorithms for data set.
#'  
#' @export
evaluateWithHVDiff = function(validation.obj, portfolio, data.set){
  landscape = validation.obj$landscape.list[[data.set]]
  f.list = landscape$f.list
  
  best.algo.order = portfolio$best.algo.order
  split.vals = portfolio$split.vals
  
  ## frankensteins function of portfolio algorithms
  f.portfolio = function(x) {
    f.sub = f.list[best.algo.order]
    selected.f = f.sub[[1 + sum(x > split.vals)]]
    
    return(selected.f(x))
  }
  f.portfolio = Vectorize(f.portfolio)
  
  ## frankensteins function of optimal set of algorithms (on given data.set)
  f.optimal = function(x) {
    f.paretoOpt = f.list[sapply(f.list, isAlgoParetoOpt)]
    true.splits = landscape$split.points
    selected.f = f.paretoOpt[[1 + sum(x > true.splits)]]
    
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