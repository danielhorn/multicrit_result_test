#' print test result
#' 
#' @export

print.frontTestResult = function(x) {
  sign.perms = x$significant.permutations
  if (!is.list(sign.perms))
    sign.perms = list(sign.perms)
  
  
  algo.col = as.character(x$args$formula[[2]])
  var.cols = as.character(x$args$formula[[3]][[2]])[-1]
  repl.col = as.character(x$args$formula[[3]][[3]])
  
  catf("Welcome to package PFPT main test procedure.\n")
  catf("Let's have a look the parameters you specified:")
  catf("Indicator: %s", x$args$indicator)
  catf("Forwardselection? %s", if (x$args$sel.fun == "forward") TRUE else FALSE)
  catf("Favored orders via: %s", x$args$perm.test)
  
  cat("\n")
  
  catf("The variables you want to analyse are %s and %s,
with respect to your algorithm variable %s and your replication variable %s.\n",
    var.cols[1], var.cols[2], algo.col, repl.col)
  
  
  algos = names(x$relevant.algos)
  catf("Found %i different algorithms: ", length(algos))
  print(algos)
  catf("\n")
  catf("Computing contributions of the algorithms to the common front with respect to %s indicator.",
    x$args$indicator)
  catf("Using all algorithms with contribution significant greater than %1.0e.", x$args$kappa)
  catf("%i algorithms have an significant contribution to the common pareto front:",
    sum(x$relevant.algos))
  print(names(which(x$relevant.algos)))
  catf("\n")
  catf("Computing favored orders of algorithms with respect to the method %s.", x$args$perm.test)
  if (length(sign.perms) == 0L)
    catf("No order of algorithms is favored about all others.")
  else {
    catf("%i order(s) of algorithms is / are favored about all others, printed with low values of %s first:",
      length(sign.perms), as.character(formula[[3]][[2]])[2])
    for (i in seq_along(sign.perms)) {
      catf("[%d] %s; Qualitiy index: %1.2f", i, collapse(as.character(sign.perms[[i]]), sep = " - "),
        x$qualitiy.index[i])
    }
  }
}