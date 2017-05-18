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
#'  4: (mainly) identical order, identical split points; for each data set one algorithm that is
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
    return(list(split.points = split.points, algos = algos, landscape.list = landscape.list))
  }
  
  ##################################

  if(data.situation == 1) {
    
    
    # generate exact, deterministic split points:
    split.points = generateSplitpoints(N = N, D = D)
    split.points = as.list(data.frame(split.points))

    # make N and M vectors
    N = rep(N, D)
    M = rep(M, D)
    
    # generate fix order for all data sets
    algo.order = generateOrder(N = N, M = M, D = D, type = "fix")

    ###############################
    # FIXME: create helper function
    all.data = NULL
    landscape.list = list()
    
    # iterate over simulator to generate main part of the result:
    for(i in 1:D) {
      
      ith.result = generateValidationData(N = N[i], M = M[i], k = k, replications = replications, discretize.type = disc.type, replications.type = rep.type, 
                             split.points = split.points[[i]], algo.order = algo.order[,i])
      
      # add landscape of current set to landscape list
      ith.landscape = extendLandscape(ith.result)
      landscape.list[[paste0("data", i)]] = ith.landscape
      
      # add data of current set to big data frame
      ith.data = ith.result$validationData
      ith.data$data.set = i
      all.data = rbind(all.data, ith.data)
    }
    #####################################
    
    # resulting true order
    algo.names = paste0("algo", 1:(N+M))
    
    result = getResult(split.points = split.points, algos = algo.names, data = all.data, landscape.list = landscape.list)
    return(result)
  }
  
  
  if(data.situation == 2) {
    
    # generate D times (N-1) split points with same mean:
    split.points.matrix = generateSplitpoints(N = N, D = D, type = "noisy")
  }
}


