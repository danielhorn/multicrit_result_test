#' print validation data object
#' 
#' @param x [\code{mosap_result}]\cr
#'   validation data object
#' @param ... [\code{any}] \cr
#'   Not used.
#'   
#' @export
print.validation.obj = function(x, ...) {
  
  d = length(x$landscape.list)
  nm = length(x$algos)
  sl = length(x$split.points)
  
  cat("Validation data consists of", d, "data set(s). \n")
  cat("It features", nm, "algorithms. \n")
  
  cat("-------------------------------\n")
  cat("The global setting is: \n")
  cat("Split points for X-variable:", x$split.points, "\n")
  cat("Algorithm order on common pareto front:", x$algos[1:(sl+1)], "\n")
  
  cat("-------------------------------\n")
  cat("The simulated scenario type is", x$type, "- indicating that: \n")
  
  if(x$type == 1) {
    cat("The global setting is found on every data set.\n")
  }
  if(x$type == 2) {
    cat("Split points for X-variable are normally distributed on each data set;\n")
    cat("their means are like the globally set split points, sigma:", x$sigma, "\n")
    cat("Algorithm order is like the globally set algorithm order. \n")
  }
  if(x$type == 3) {
    cat("Split points for X-variable are chaotic on each data set. \n")
    cat("Algorithm order is like the globally set algorithm order. \n")
  }
  if(x$type == 4) {
    cat("Algorithm order is changed on", x$p*100, "% of data sets:\n")
    cat("A globally set non-dominated algorithm is now dominated.\n")
    cat("Split points remain (mainly) the same as globally set.\n")
  }
  if(x$type == 5) {
    cat("Algorithm order is changed on", x$p*100, "% of data sets:\n")
    cat("A globally set dominated algorithm is now non-dominated.\n")
    cat("Split points remain (mainly) the same as globally set.\n")
  }
  if(x$type == 6) {
    cat("Algorithm order is changed on", x$p*100, "% of data sets:\n")
    cat("A globally set dominated and a globally set non-dominated algorithm are swapped.\n")
    cat("Split points remain (mainly) the same as globally set.\n")
  }
  if(x$type == 7) {
    cat("Algorithm order is changed on", x$p*100, "% of data sets:\n")
    cat("Two globally set non-dominated algorithms are swapped.\n")
    cat("Split points remain (mainly) the same as globally set.\n")
  }
  if(x$type == 8) {
    cat("Algorithm order is random on each data set.\n")
    cat("Split points are like globally set.\n")
  }
  if(x$type == 9) {
    cat("Algorithm order is random on each data set.\n")
    cat("Split points are random on each data set.\n")
  }
}