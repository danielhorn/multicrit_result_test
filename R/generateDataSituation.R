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
#' @param data.situation [\code{integer}] \cr
#'  Number of data situation to generate:
#'  1: identical order of algorithms, split points identical
#'  2: identical order of algorithms, split points noisy (same mean)
#'  3: identical order of algorithms, split points differ ('complicated')
#'  4: identical order, identical split points; for each data set one algorithm that is
#'     normally found on the common pareto front is missing (not on the front).
#'  5: a normally dominated algorithm appears randomly on the common pareto front
#'  6: order changed: on p% of data sets two algorithms are swapped, split points identical
#'  7: dominated and non-dominated algorithm swapped
#'  8: order random, split points identical
#'  9: order and split points random
#' 
#' @return [\code{list}]
#'  List that contains true split points, the names of the algorithms and validation data; and a list of lists (one for each data set), each containing the true (original) pareto landscape 
#'  (functions that form the pareto fronts, true splitpoints, ...) - INCLUDING the names
#'  of the algorithms.
#'  
#' @export
generateDataSituation = function(N, M, D, k = 20L, replications = 10L, data.situation) {
  rep.type = "parameter-noise"
  disc.type = "NSGA-II_g"

  if(data.situation == 1) {
    
    
    # generate exact, deterministic split points, used for every data set:
    split.points = rep(0, (N-1))
    split.dist = 1/N
    split.points = sapply(split.points, cumsum(rep(split.dist, (N-1))))
    
    # iterate over simulator to generate that part of the result:
    for(i in 1:D) {
      tmp = generateValidationData(N = N, M = M, k = k, replications = replications, discretize.type = disc.type, replications.type = rep.type, 
                             split.points = split.points)
      
      extendLandscape = function(x){
        landscape = x$landscape
        algos = x$algos
        landscape$algos = algos
        
        return(landscape)
      }
      
    }
    
    result = list(split.points = split.points, )
  }
  
}