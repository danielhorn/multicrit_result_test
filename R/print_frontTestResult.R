

print.frontTestResult = function(x) {
  sign.perms = x$significant.permutations
  if (!is.list(sign.perms))
    sign.perms = list(sign.perms)
  
  
  algos = names(x$relevant.algos)
  catf("Found %i different algorithms: ", length(algos))
  print(algos)
  catf("\n")
  catf("%i algorithms have an significant contribution to the common pareto front:", sum(x$relevant.algos))
  print(names(which(x$relevant.algos)))
  catf("\n")
  if (length(sign.perms) == 0L)
    catf("No order of algorithms is favored about all others.")
  else {
    catf("%i order(s) of algorithms is / are favored about all others, printed with low values of %s first:",
      length(sign.perms), as.character(formula[[3]][[2]])[2])
    for (i in seq_along(sign.perms)) {
      catf("%s; Qualitiy index: %1.2f", collapse(as.character(sign.perms[[i]]), sep = " - "),
        x$qualitiy.index[i])
    }
  }
}