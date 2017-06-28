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
#'  4: changed order, (mainly) identical split points; on p% of data sets one algorithm that is
#'     normally found on the common pareto front is missing (not on the front).
#'  5: changed order: (on p% of data sets) a normally dominated algorithm appears randomly on the common pareto front (reverse to 4)
#'  6: changed order: on p% of data sets a dominated algorithm is swapped with a non-dominated algorithm (combines 4 and 5)
#'  7: changed order: on p% of data sets two (non-dominated) algorithms are swapped, split points identical
#'  8: random order, split points identical
#'  9: random order and split points random
#'  @param p [\code{numeric}] \cr
#'  Parameter for type 4-7: probability for a missing/ inserted/ swapped algorithm on each data set. Between 0 and 1.
#'  @param sigma [\code{numeric}] \cr
#'  Parameter for type 2 - noise strength
#'  
#' 
#' @return [\code{list}]
#'  List that contains true split points, the names of the algorithms and validation data; and a list of lists (one for each data set), each containing the true (original) pareto landscape 
#'  (functions that form the pareto fronts, true splitpoints, ...) - INCLUDING the names
#'  of the algorithms.
#'  
#' @export
generateValidationData = function(N, M, D, type, p, sigma, ...) {
  
  # Default Orders und Splits
  split.points = seq(0, 1, length.out = N + 1)[2:N]
  algo.order = 1:(N + M)
  
  # Initialize result Data frame
  valid.data = BBmisc::makeDataFrame(nrow = 0, ncol = 5, col.types = "numeric",
    col.names = c("algorithm", "x", "y", "repl", "dataset"))
  landscapes = list()
  
  
  
  for (ds.id in seq_len(D)) {
    
    # Generate split points and algorithm order for certain single data situation
    ds = singleDataSituationData(type, N, M, split.points, algo.order, p, sigma)
    
    # Get specific N and M for ith data set
    N.i = length(ds$split.points) + 1
    M.i = N + M - N.i
    
    dat = generateSingleValidationData(N = N.i, M = M.i,
      split.points = ds$split.points, algo.order = ds$algo.order[1:N.i],
      discretize.type = "NSGA-II_g", replications.type = "parameter-noise",
      k = 20L, replication = 10L)
    
    # Add dataset column to validationData
    dat$validationData$dataset = ds.id
    
    # Store validation data and landscape in result objects
    valid.data = rbind(valid.data, dat$validationData)
    landscapes[[ds.id]] = dat$landscape
  }
  
  result = makeS3Obj(
    valid.data = valid.data,
    type = type,
    sigma = sigma,
    p = p,
    split.points = split.points,
    algos = paste0("algo", algo.order),
    landscape.list = landscapes,
    
    classes = "validation.obj"
  )
  
  return(result)
}


