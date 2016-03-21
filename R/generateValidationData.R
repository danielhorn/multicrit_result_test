#' Generates validation data for the algorithm portfolio selection.
#'
#' @param N [\code{integer}] \cr
#'   Number of algorithms on the common pareto front.
#' @param M [\code{integer}] \cr
#'  Number of additional algorithms that are not on the commo pareto front.   
#' @param split.points.type [\code{character}] \cr
#'  Determines whether the split.points between the algorithms are chosen in  a 
#'  \code{uniform} or \code{nonuniform} way.
#' @param type [\code{character}] \cr
#'  Determines how the discrete approximation of the pareto front is done.
#'  The values \code{deterministic}, \code{uniform}, \code{NSGA2} and \code{NSGA2_g}
#'  are possible.
#' @param randomness [\code{character}] \cr
#'  Determines whether noise is added to the parameters of the functions that form
#'  the fronts (\code{parambased}) or to the points (\code{pointbased}) to get 
#'  replications of the discrete approximation. 
#' @param number.of.points [\code{integer}] \cr
#'  Number of points that is generated for every algorithm.
#' @param replications [\code{integer}] \cr
#'  Number of replications of the discrete aproximation.
#' 
#' @return [\code{list}]
#'  List that contains information about the true (original) pareto landscape 
#'  (functions that form the pareto fronts, true splitpoints, names of the algorithms)
#'  and the validation data.
#'  
#' @export

generateValidationData = function(N, M, split.points.type = "uniform", 
  type = "deterministic", randomness = "parambased", number.of.points = 20L, 
  replications = 10L) {
  if (split.points.type == "uniform") {
    split.points = switch(N, NULL, 0.5, c(0.33, 0.66), NULL, c(0.2, 0.4, 0.6, 0.8))
  }
  if (split.points.type == "nonuniform") {
    split.points = switch(N, NULL, 0.2, c(0.3, 0.5), NULL, c(0.18, 0.2, 0.55, 0.75))
  }
  
  landscape = generateParetoLandscape(N = N, M = M, split.points = split.points)
  
  landscape.list = list()
  for (i in 1:replications) {
    landscape.list[[i]] = makeNoisy(landscape)
  }
  
  generateDiscreteParetoLandscape = switch(type, 
    "deterministic" = generateDiscreteParetoLandscapeDeterministic, 
    "uniform" = generateDiscreteParetoLandscapeUniform, 
    "NSGA2" = generateDiscreteParetoLandscapeNSGA2, 
    "NSGA2_g" = generateDiscreteParetoLandscapeNSGA2_g)
  
  if (randomness == "parambased") {
    tmp = lapply(seq_along(landscape.list), function(i)
      cbind(generateDiscreteParetoLandscape(landscape.list[[i]], number.of.points), i))
    X = do.call(rbind, tmp)
  }
  
  if (randomness == "pointbased") {
    disc.ls = generateDiscreteParetoLandscape(landscape, number.of.points)
    tmp = lapply(seq_along(landscape.list), function(i) {
      randi = matrix(abs(rnorm(2 * nrow(disc.ls), 0, 0.02)), ncol = 2)
      cbind(disc.ls[, 1], disc.ls[, 2:3] + randi, i)
    }
    )
    X = do.call(rbind, tmp)
  }
  
  colnames(X) = c("algorithm", "x", "y", "repl")
  
  return(list(landscape = list(
    f.list = landscape$f.list, 
    split.points = landscape$split.points,
    algos = paste("algo", 1:(N + M), sep = "")
    ), 
    validationData = X
  ))
}
