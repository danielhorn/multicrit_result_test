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
  
  ##################################
  # some helper functions to later be moved elsewhere:
  # adds algorithm names to a landscape
  extendLandscape = function(x){
    landscape = x$landscape
    algos = x$algos
    landscape$algos = algos
    
    return(landscape)
  }
  
  # generates a result object
  getResult = function(split.points, algos, data, landscape.list){
    return(list(split.points = split.points, algs = algos, landscape.list = landscape))
  }
  
  ##################################

  if(data.situation == 1) {
    
    
    # generate exact, deterministic split points, used for every data set:
    split.points = rep(0, (N-1))
    split.dist = 1/N
    split.points = sapply(split.points, cumsum(rep(split.dist, (N-1))))
    
    all.data = NULL
    landscape.list = list()
    
    # iterate over simulator to generate that part of the result:
    for(i in 1:D) {
      
      ith.result = generateValidationData(N = N, M = M, k = k, replications = replications, discretize.type = disc.type, replications.type = rep.type, 
                             split.points = split.points)
      
      # add landscape of current set to landscape list
      ith_landscape = extendLandscape(tmp)
      landscape.list[[i]] = ith.landscape
      
      # add data of current set to big data frame
      ith.data = tmp$validationData
      ith.data$data.set = i
      all.data = rbind(all.data, ith.data)
    }
    
    algo.names = paste0("algo", 1:(N+M))
    
    result = getResult(split.points = split.points, algos = algo.names, data = all.data, landscape.list = landscape.list)
    return(result)
  }
  
}