#' Simulates various data situations (each with multiple data sets).
#'
#' @param N [\code{integer}] \cr
#'   Number of algorithms on the common pareto front.
#' @param M [\code{integer}] \cr
#'  Number of additional algorithms that are not on the common pareto front.
#'  @param D.train [\code{integer}] \cr
#'  Number of data sets for training.
#'  @param D.test [\code{integer}] \cr
#'  Number of data sets for test.
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
#'  Parameter for type 4-7: probability for a missing/ inserted/ swapped algorithm on each data set. Between 0 and 1, defaults to 0.5
#'  @param sigma [\code{numeric}] \cr
#'  Parameter for type 2 - noise strength, defaults to 1 / (4 * N)
#'  
#' 
#' @return [\code{list}]
#'  List that contains true split points, the names of the algorithms and validation data; and a list of lists (one for each data set), each containing the true (original) pareto landscape 
#'  (functions that form the pareto fronts, true splitpoints, ...) - INCLUDING the names
#'  of the algorithms.
#'  
#' @export
generateValidationData = function(N, M, D.train, D.test,
  replications, type, p = 0.5, sigma = 1 / (4 * N), ...) {
  
  # Default Orders und Splits
  if (N == 1L) {
    split.points = numeric(0)
  } else {
    split.points = seq(0, 1, length.out = N + 1)[2:N]
  }
  algo.order = 1:(N + M)
  
  # Initialize result Data frame
  valid.data = BBmisc::makeDataFrame(nrow = 0, ncol = 5, col.types = "numeric",
    col.names = c("algorithm", "x", "y", "repl", "dataset"))
  landscapes = list()
  general.changes = logical()
  
  ## Ueberpruefen, ob N oder M zu klein fuer die jeweilige Situation ist
  
  if(type == 1 || type == 2 || type == 3 || type == 8 || type == 9){
    if(N < 1 || M < 0){
      stop("N should be >= 1 and M must not be negative.")
    }
  } else if(type == 4 || type == 7){
    if(N < 2 || M < 0){
      stop("N has to be > 1 and M must not be negative.")
    }
  } else if(type == 5 || type == 6){
    if(M < 1 || N < 1){
      stop("N should be >= 1 and M must be positive.")
    }
  }
  
  for (ds.id in seq_len(D.train + D.test)) {
    
    # Generate split points and algorithm order for certain single data situation
    ds = singleDataSituationData(type, N, M, split.points, algo.order, p, sigma)
    
    # Get specific N and M for ith data set
    N.i = length(ds$split.points) + 1
    M.i = N + M - N.i
    
    dat = generateSingleValidationData(id.num = ds.id, N = N.i, M = M.i,
      split.points = ds$split.points, algo.order = ds$algo.order,
      discretize.type = "NSGA-II_g", replications.type = "parameter-noise",
      k = 20L, replications = replications)
    
    # Add dataset column to validationData
    dat$validationData$dataset = ds.id
    
    # Store validation data and landscape in result objects
    valid.data = rbind(valid.data, dat$validationData)
    landscapes[[ds.id]] = dat$landscape
    general.changes[ds.id] = ds$general.change
  }
  
  names(general.changes) = 1:(D.train + D.test)
  
  result = makeS3Obj(
    train.data = subset(valid.data, dataset %in% 1:D.train),
    train.landscapes = landscapes[1:D.train],
    train.general.changes = general.changes[1:D.train],
    
    split.points = split.points,
    algos = paste0("algo", algo.order),
    
    test.data = subset(valid.data, dataset %in% (D.train + 1):(D.train + D.test)),
    test.landscapes = landscapes[(D.train + 1):(D.train + D.test)],
    test.general.changes = general.changes[(D.train + 1):(D.train + D.test)],
    
    type = type,
    sigma = sigma,
    p = p,
    
    classes = "validation.obj"
  )
  
  result$train.data$dataset = as.factor(result$train.data$dataset)
  result$test.data$dataset = as.factor(result$test.data$dataset)
  
  return(result)
}


