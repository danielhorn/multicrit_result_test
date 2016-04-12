#' Generates validation data for the algorithm portfolio selection.
#'
#' @param N [\code{integer}] \cr
#'   Number of algorithms on the common pareto front.
#' @param M [\code{integer}] \cr
#'  Number of additional algorithms that are not on the commo pareto front.   
#' @param split.type [\code{character}] \cr
#'  Determines whether the split points between the algorithms are chosen in  a 
#'  \code{uniform} or \code{non-uniform} way.
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
#' @export
generateValidationData = function(N, M, split.type = "uniform", 
  discretize.type = "deterministic", replications.type = "parameter-noise", 
  k = 20L, replications = 10L) {
  
  if (split.type == "uniform") {
    split.points = switch(N, NULL, 0.5, c(0.33, 0.66), NULL, c(0.2, 0.4, 0.6, 0.8))
  }
  if (split.type == "non-uniform") {
    split.points = switch(N, NULL, 0.2, c(0.3, 0.5), NULL, c(0.18, 0.2, 0.55, 0.75))
  }
  
  landscape = generateParetoLandscape(N = N, M = M, split.points = split.points)
  
  landscape.list = list()
  
  for (i in 1:replications) {
    landscape.list[[i]] = makeNoisy(landscape)
  }
  
  generateDiscreteParetoLandscape = switch(discretize.type, 
    "deterministic" = generateDiscreteParetoLandscapeDeterministic, 
    "random" = generateDiscreteParetoLandscapeRandom, 
    "NSGA-II" = generateDiscreteParetoLandscapeNSGAII, 
    "NSGA-II_g" = generateDiscreteParetoLandscapeNSGAII_g)
  
  if (replications.type == "parameter-noise") {
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
  
  return(list(landscape = landscape,
    algos = paste("algo", 1:(N + M), sep = ""),
    validationData = X
  ))
}
