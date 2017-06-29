#' Generates validation data for the algorithm portfolio selection.
#'
#' @param N [\code{integer}] \cr
#'   Number of algorithms on the common pareto front.
#' @param M [\code{integer}] \cr
#'  Number of additional algorithms that are not on the commo pareto front.   
#' @param split.points [\code{numeric(N - 1)}] \cr
#'  Sets the split points, must be in [0, 1] and sorted. If argument is missing, generates uniformly distributed split points.
#'  @param algo.order [\code{integer(N + M)}]\cr
#'  Determines the order of algorithms, e.g. for plotting purpose or to generate
#'  multiple sets of validation data with algorithms appearing in different ordering.
#'  It has to be a permutation of 1,2,...,(N+M).
#' @param discretize.type [\code{character}] \cr
#'  Determines how the discrete approximation of the pareto front is done.
#'  The values \code{deterministic}, \code{random}, \code{NSGA-II} and \code{NSGA-II_g}
#'  are possible.
#' @param replications.type [\code{character}] \cr
#'  Determines whether noise is added to the parameters of the functions that form
#'  the fronts (\code{parameter-noise}) or to the points (\code{point-noise}) to get 
#'  replications of the discrete approximation. It is also possible to add no
#'  noise (\code{replications.type = without-noise}).
#' @param k [\code{integer}] \cr
#'  Number of points that is generated for every algorithm.
#' @param replications [\code{integer}] \cr
#'  Number of replications of the discrete approximation.
#' 
#' @return [\code{list}]
#'  List that contains the true (original) pareto landscape 
#'  (functions that form the pareto fronts, true splitpoints, ...), the names
#'  of the algorithms and the validation data.
#'  
#'  
generateSingleValidationData = function(N = 3, M = 1, split.points = c(1 / 3, 2 / 3),
  algo.order = 1:(N + M), discretize.type = "deterministic",
  replications.type = "parameter-noise", k = 20L, replications = 10L) {
  
  N = asInt(N)
  M = asInt(M)
  if (missing(split.points))
    split.points = sort(runif(N - 1))
  assert_numeric(split.points, lower = 0, upper = 1, len = N - 1)
  if (is.unsorted(split.points))
    stop("split.points must be increasing.")
  assertChoice(discretize.type, choices = c("deterministic", "random",
    "NSGA-II", "NSGA-II_g"))
  assertChoice(replications.type, choices = c("parameter-noise", "point-noise"))
  k = asInt(k)
  replications = asInt(k)
  
  algo.order = asInteger(algo.order, unique = TRUE, len = (N + M))
  assertSetEqual(algo.order, 1:(N + M))
  
  #
  generateDiscreteParetoLandscape = switch(discretize.type, 
    "deterministic" = generateDiscreteParetoLandscapeDeterministic, 
    "random" = generateDiscreteParetoLandscapeRandom, 
    "NSGA-II" = generateDiscreteParetoLandscapeNSGAII, 
    "NSGA-II_g" = generateDiscreteParetoLandscapeNSGAII_g)
  
  
  landscape = generateParetoLandscape(N = N, M = M, split.points = split.points,
    algo.order = algo.order)


  if (replications.type == "parameter-noise") {
    landscape.list = lapply(1:replications, function(i) makeNoisy(landscape))
    
    tmp = lapply(seq_along(landscape.list), function(i)
      cbind(generateDiscreteParetoLandscape(landscape.list[[i]], k), i))
    X = do.call(rbind, tmp)
  }
  
  if (replications.type == "point-noise") {
    disc.ls = generateDiscreteParetoLandscape(landscape, k)
    tmp = lapply(seq_along(landscape.list), function(i) {
      randi = matrix(abs(rnorm(2 * nrow(disc.ls), 0, 0.02)), ncol = 2)
      cbind(disc.ls[, 1], disc.ls[, 2:3] + randi, i)
    }
    )
    X = do.call(rbind, tmp)
  }
  
  if (replications.type == "without-noise") {
    tmp = lapply(seq_along(landscape.list), function(i)
      cbind(generateDiscreteParetoLandscape(landscape, k), i))
    X = do.call(rbind, tmp)
  }
  
  colnames(X) = c("algorithm", "x", "y", "repl")
  
  #generate names for algorithms corresponding to given algorithm order
  algoNames = paste0("algo", algo.order)
  
  res.obj = list(landscape = landscape,
    validationData = X
  )
  
  return(res.obj)
}
