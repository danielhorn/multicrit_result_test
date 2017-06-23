`$.landscape` = function(landscape, element) {
  if (element %in% c("id", "f.list")) {
    return(landscape[[element]])
  }
  
  if (element == "split.points") {
    all.splits = lapply(landscape$f.list, getAlgoRangeX)
    all.splits = sort(unique(unlist(all.splits)))[2:(getAlgoCount(landscape) - 1)]
    return(all.splits)
  }
  
  if (element == "algo.order") {
    left.border = unlist(lapply(landscape$f.list, function(x) 
      c(na.omit(getAlgoRangeX(x)[1]))))
    order = order(left.border)
    return(sapply(landscape$f.list, function(f) attributes(f)$id)[order])
  }
  
  stopf("Landscape does not include the requested element %s.", element)
}

getAlgoCount = function(landscape) {
  length(landscape$f.list)
}

getFrontAlgoCount = function(landscape) {
  sum(sapply(landscape$f.list, isAlgoParetoOpt))
}

#' print test result
#' 
#' @param x [\code{mosap_result}]\cr
#'   Landscape object
#' @param ... [\code{any}] \cr
#'   Not used.
#'   
#' @export
print.landscape = function(x, ...) {
  cat("Landscape:", x$id, "\n")
  cat("------------------------- \n")
  
  nm = length(x$f.list)
  n = sum(sapply(x$f.list, isAlgoParetoOpt))
  m = nm - n

  cat("Containing", nm, "algorithms: \n")
  cat(n, "pareto optimal algorithm(s) and", m, "dominated algorithm(s). \n")
  cat("\n")
  for(f in x$f.list) {
    print(f)
    cat("\n")
  }
}