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

mainTestProcedure = function(formula, data, alpha, indicator = "hv",
  normalize = TRUE, ref.point = c(1.1, 1.1), lambda = 100, cp = 0.1) {
  
  requirePackages(c("emoa", "combinat"))  
  # Extract informations from formula
  algo.col = as.character(formula[[2]])
  repl.col = as.character(formula[[3]][[3]])
  var.cols = as.character(formula[[3]][[2]])[-1]
  algos = factor(unique(data[, algo.col]))
  data.old = data
  
  # Normalize Data
  if (normalize)
    data[, var.cols] = normalize(data[, var.cols], method = "range", range = c(0, 1))
    #for (i in seq_along(unique(data[, repl.col])))
    #  data[data[, repl.col] == i, var.cols] = normalize(data[data[, repl.col] == i, var.cols],
    #    method = "range", range = c(0, 1))
  
  # Select contribution function with respect to indicator character
  contrFun = switch(indicator,
    hv = function(points, o)
      hypervolume_indicator(t(points), t(o), ref = ref.point),
    epsilon = function(points, o)
      epsilon_indicator(t(points), t(o)),
    r2 = function(points, o)
      r2_indicator(t(points), t(o), lambda = lambda)
  )
  
  # First selection: Remove all algorithms that are dominated in at least 50% repls
  non.dom.algos = relevantAlgosDominationSelection(data, formula)
  # Now drop non-selected algos and set new factor levels
  algos = factor(algos[non.dom.algos$relevant.algos])
  data = subset(data, data[, algo.col] %in% algos)
  data[, algo.col] = factor(data[, algo.col], levels = algos)
  
  # Second Selection: Multicrit Selection
  selected.algos = relevantAlgosMulticritSelection(data, formula, contrFun)
  # Now drop non selected algos and set new factor levels
  algos = factor(algos[selected.algos$relevant.algos])
  data = subset(data, data[, algo.col] %in% algos)
  data[, algo.col] = factor(data[, algo.col], levels = algos)
  
  # Third Step: Find the best order of algorithms
  if (length(algos) < 1L) {
    stop("Found no relevant algorithm. Should not happen.")
  }
  if (length(algos) == 1L) {
    perms = algos
    split.vals = NULL
    qualitiy.index = 0
  }
  if (length(algos) > 1L) {
    perms = sortedParetoFrontClassification(formula, data, contrFun, cp = cp)
    split.vals = perms$split.vals
    #qualitiy.index = perms$mmce
    perms  = perms$perm
  }

  # Build result object
  # First a list with all input args
  args = list(
    formula = formula,
    data = data.old,
    indicator = indicator,
    normalize = normalize,
    ref.point = ref.point,
    lambda = lambda,
    cp = cp
  )
  
  res = list(
    non.dominated.algos = non.dom.algos$relevant.algos,
    algos.domination.count = non.dom.algos$algo.contrs,
    relevant.algos = selected.algos$relevant.algos,
    algos.selection.vals = selected.algos$algo.contrs,
    best.algo.order = perms,
    split.vals = split.vals,
    args = args
  )
  res = addClasses(res, "frontTestResult")
  
  return(res)
}
