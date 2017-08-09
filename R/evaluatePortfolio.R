#' Combined measures for the comparison of a true and an estimated common pareto front
#' Returns global error in X dimension as well as optimality gap (based on hypervolume)
#' for each data set.
#'
#' @param validation.obj
#'   Result of \code{generateParetoLandscape}.
#' @param portfolio  
#'   Result of \code{MOSAP::selectPortfolio}. 
#' 
#' @return [\code{numeric}]
#'  Result is a list of two elements:
#'  - (global) X-Error
#'  - optimality gap measure for each data set
#'  
#' @export
evaluatePortfolio = function(validation.obj, portfolio){
  x.error = evaluateWithXIntegral(validation.obj, portfolio)
  all.ds = unique(validation.obj$valid.data$dataset)
  #opt.gaps = numeric(length = length(all.ds))
  hv.diffs = numeric(length = length(all.ds))
  
  for(ds in seq_along(all.ds)) {
    #opt.gaps[ds] = evaluateWithOptimalityGap(validation.obj, portfolio, all.ds[ds])
    hv.diffs[ds] = evaluateWithHVDiff(validation.obj, portfolio, all.ds[ds])
  }
  
  result = list(x.error = x.error, hv.diffs = hv.diffs)
  return(result)
}