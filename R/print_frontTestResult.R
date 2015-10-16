#' print test result
#' 
#' @export

print.frontTestResult = function(x) {
  algo.col = x$args$algo.col
  var.cols = x$args$var.col
  repl.col = x$args$repl.col
  
  catf("Welcome to package PFPT main test procedure.\n")
  catf("Let's have a look the parameters you specified:")
  catf("Indicator: %s", x$args$indicator)
  cat("\n")
  
  catf("The variables you want to analyse are %s and %s,
with respect to your algorithm variable %s and your replication variable %s.\n",
    var.cols[1], var.cols[2], algo.col, repl.col)
  
  
  algos = names(x$non.dominated.algos)
  catf("Found %i different algorithms: ", length(algos))
  print(algos)
  catf("\n")
  
  catf("%i algorithms have non-dominated points in more than 50 percent of your replications:",
    sum(x$non.dominated.algos))
  print(names(which(x$non.dominated.algos)))
  catf("\n")
  
  catf("The best trade-off between contribution to the common front and
number of algorithms is made by these %i algorithms:", sum(x$relevant.algos))
  print(names(which(x$relevant.algos)))
  catf("\n")
  
  catf("Favored order of algorithms, printed with low values of %s first:",
    var.cols[1L])
  catf("[1] %s", collapse(as.character(x$best.algo.order), sep = " - "))
}