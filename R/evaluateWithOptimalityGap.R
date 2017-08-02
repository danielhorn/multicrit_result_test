#' Measure for the comparison of a true and an estimated common pareto front
#' Uses optimality gap (based on hypervolume).
#'
#' @param validation.obj
#'   Result of \code{generateParetoLandscape}.
#' @param portfolio  
#'   Result of \code{MOSAP::selectPortfolio}. 
#' 
#' @return [\code{numeric}]
#'  Array of optimality gap-based measure for each data set,
#'  values between 0 and 1.
#'  
#' @export
evaluateWithOptimalityGap = function(validation.obj, portfolio, data.set){
  landscape = validation.obj$landscape.list[[data.set]]
  f.list = landscape$f.list
  
  best.algo.order = portfolio$best.algo.order
  # Um Randfälle direkt mit zu behandeln wie alle anderen auch
  split.vals = c(0, portfolio$split.vals, 1.1)
  
  raster.seq = seq(0, 1, length.out = 1e5)

  i = 1
  y.sel = numeric(length = 1e5)
  for(alg in best.algo.order) {
    ind.area = which(raster.seq >= split.vals[i] 
                    & raster.seq < split.vals[i + 1])
    
    names(f.list) = landscape$algo.order
    y.sel[ind.area] = f.list[[alg]](raster.seq[ind.area])
    i = i + 1
  }
  hv.portfolio = dominated_hypervolume(points = rbind(raster.seq, y.sel), ref = c(1,1))
  
  split.points = c(0, landscape$split.points, 1.1)
  algo.order = landscape$algo.order
  
  j = 1
  y.glo = numeric(length = 1e5)
  for(alg in algo.order) {
    ind.area = which(raster.seq >= split.points[i] 
                     & raster.seq < split.points[i + 1])
    y.glo[ind.area] = f.list[[alg]](raster.seq[ind.area])
    j = j + 1
  }
  hv.validation = dominated_hypervolume(points = rbind(raster.seq, y.glo), ref = c(1,1))
  
  result = 1 - (hv.portfolio / hv.validation)
  return(result)
}