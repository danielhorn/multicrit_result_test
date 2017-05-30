#' Simulates various data situations (each with multiple data sets).
#'
#' @param N [\code{integer}] \cr
#'   Number of algorithms on the common pareto front.
#' @param M [\code{integer}] \cr
#'  Number of additional algorithms that are not on the common pareto front.
#'  @param D [\code{integer}] \cr
#'  Number of data sets.
#' @param k [\code{integer}] \cr
#'  Number of points that is generated for every algorithm.
#' @param replications [\code{integer}] \cr
#'  Number of replications of the discrete approximation.
#' @param type [\code{integer}] \cr
#'  Number of data situation to generate:
#'  1: identical order of algorithms, split points identical
#'  2: identical order of algorithms, split points noisy (same mean)
#'  3: identical order of algorithms, split points differ ('complicated')
#'  4: changed order, (mainly) identical split points; for each data set one algorithm that is
#'     normally found on the common pareto front is missing (not on the front).
#'  5: changed order: a normally dominated algorithm appears randomly on the common pareto front (reverse to 4)
#'  6: changed order: dominated and non-dominated algorithm swapped (combines 4 and 5)
#'  7: changed order: on p% of data sets two (non-dominated) algorithms are swapped, split points identical
#'  8: random order, split points identical
#'  9: random order and split points random
#'  @param p [\code{numeric}] \cr
#'  Parameter for data.situation 4-7: probability for a missing/ inserted/ swapped algorithm on each data set. Between 0 and 0.4 (?)
#'  @param sigma [\code{numeric}] \cr
#'  Parameter for data.situation 2 - noise strength
#'  
#' 
#' @return [\code{list}]
#'  List that contains true split points, the names of the algorithms and validation data; and a list of lists (one for each data set), each containing the true (original) pareto landscape 
#'  (functions that form the pareto fronts, true splitpoints, ...) - INCLUDING the names
#'  of the algorithms.
#'  
#' @export
generateDataSituation = function(N, M, D, type, p, sigma, ...) {
  
  ##################################
  # some helper functions to later be moved elsewhere:
  # adds algorithm names to a landscape
  extendLandscape = function(x){
    landscape = x$landscape
    algos = x$algos
    landscape$algos = algos
    
    return(landscape)
  }
  
  # Default Orders und Splits
  split.points = seq(0, 1, length.out = N + 1)[2:N]
  algo.order = 1:(N + M)
  
  # Initialize result Data frame
  valid.data = BBmisc::makeDataFrame(nrow = 0, ncol = 5, col.types = "numeric",
    col.names = c("algorith", "x", "y", "repl", "dataset"))
  landscapes = list()
  
  ##################################
  for (ds.id in seq_len(D)) {
    ds = singleDataSituationData(type, N, M, split.points, algo.order, p, sigma)
    N.i = length(ds$split.points) + 1
    M.i = N + M - N.i
    #generateValidationData(N = N.i, M = M.i,
    #  split.points = ds$split.points, algo.order = ds$algo.order, ...)
    dat = generateValidationData(N = N.i, M = M.i,
      split.points = ds$split.points, algo.order = ds$algo.order,
      discretize.type = "NSGA-II_g", replications.type = "parameter-noise",
      k = 20L, replication = 10L)
    
    dat$validationData$dataset = ds.id
    valid.data = rbind(valid.data, dat$validationData)
    landscapes[[ds.id]] = extendLandscape(dat)
  }
  
  
  return(list(
    valid.data = valid.data,
    split.points = split.points,
    algos = algos,
    landscape.list = landscapes)
  )
}


