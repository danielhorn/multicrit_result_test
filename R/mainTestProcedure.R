#' Main Test Procedure
#'
#' @param formula [\code{formula}] \cr
#'   Formula to specify the variables: algorithms ~ var1 + var2 | repl
#' @param data [\code{data.frame}] \cr
#'   Data for the test. Must contain all variables of the formula, all other
#'   variables are ignored.
#' @param alpa [\code{numeric(1)}] \cr
#'   Alpha value for the tests.
#' @export

mainTestProcedure = function(formula, data, alpha, indicator = "hv", sel.fun = "forward",
  perm.test = "mean.invs", kappa = NULL, normalize = TRUE, ref.point = c(1.1, 1.1),
  lambda = 100, n = 1e5, cp = 0.1) {
  
  if (is.null(kappa))
    kappa = switch(indicator,
      hv = 5e-3, 
      epsilon = 2.5e-2,
      r2 = 1e-3)
  
  requirePackages(c("emoa", "combinat"))  
  # Extract informations from formula
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]
  algos = unique(data[, algo.col])
  data.old = data
  
  # Normalize Data
  if (normalize)
    for (i in seq_along(unique(data[, repl.col])))
      data[data[, repl.col] == i, var.cols] = normalize(data[data[, repl.col] == i, var.cols],
        method = "range", range = c(0, 1))
  
  contrFun = switch(indicator,
    hv = function(points, o)
      hypervolume_indicator(t(points), t(o), ref = ref.point),
    epsilon = function(points, o)
      epsilon_indicator(t(points), t(o)),
    r2 = function(points, o)
      r2_indicator(t(points), t(o), lambda = lambda)
  )
  
  selFun = switch(sel.fun,
    ind = relevantAlgosSelection,
    forward = relevantAlgosForwardSelection
    )
  
  sel.algs = selFun(data = data, formula = formula,
    contrFun = contrFun, kappa = kappa, alpha = alpha)
  
  algos = names(sel.algs$relevant.algos)[sel.algs$relevant.algos]
  data = subset(data, data[, algo.col] %in% algos)
  # drop some factor levels
  data[, algo.col] = factor(data[, algo.col], levels = algos)
  algos = factor(algos, levels = algos)
  
  # Now the permutation test. Depending on perm.test arg ...
  # If only 1 algo is relevant, we don't need this
  #(if no algorithm is relevant, somthing went wront)
  if (length(algos) < 2L) {
    perms = algos
    split.vals = NULL
    qualitiy.index = 0
  } else {
    if (perm.test %in% c("mean.invs", "test.invs")) {
      # For every replication and every possible order of the remaining algos
      # perform sortedFrontPermutationTest
      # mean over p-Values
      perm.p.values = lapply(permn(algos), sortedParetoFrontTest, formula = formula, data = data, n = 1000)
      names(perm.p.values) = sapply(permn(algos), function(x) collapse(as.character(x), sep = " - "))
      
      if (perm.test == "mean.invs") 
        qualitiy.index = sapply(perm.p.values, mean)
      
      if(perm.test == "test.invs") 
        qualitiy.index = sapply(perm.p.values, function(x)
          wilcox.test(x, alternative = "less", mu = alpha)$p.value)
      
      perms = which(qualitiy.index < alpha)
      perms = permn(algos)[perms]
      qualitiy.index = qualitiy.index[qualitiy.index < alpha]
      split.vals = lapply(perms, function(perm) getTestSplitVals(data, perm, var.cols, repl.col, algo.col))
    }
    if (perm.test == "rpart") {
      perms = sortedParetoFrontClassification(formula, data, cp = cp)
      split.vals = perms$split.vals
      qualitiy.index = perms$mmce
      perms  = perms$perm
    }
  }
  # Build result object
  args = list(
    formula = formula,
    data = data.old,
    indicator = indicator,
    sel.fun = sel.fun,
    alpha = alpha, 
    perm.test = perm.test,
    kappa = kappa,
    normalize = normalize,
    ref.point = ref.point,
    lambda = lambda,
    n = n,
    cp = cp
  )
  
  res = list(
    args = args,
    front.contribution = sel.algs$algo.contrs,
    relevant.algos = sel.algs$relevant.algos,
    split.vals = split.vals,
    significant.permutations = perms,
    qualitiy.index = qualitiy.index
  )
  res = addClasses(res, "frontTestResult")
  
  return(res)
}
