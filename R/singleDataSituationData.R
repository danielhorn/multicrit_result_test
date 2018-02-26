#' Creates split points and algorithm order parameters to generate single data situations
#'
#' @param N [\code{integer}] \cr
#'   Number of algorithms on the common pareto front.
#' @param M [\code{integer}] \cr
#'  Number of additional algorithms that are not on the common pareto front.
#' @param situation [\code{integer}] \cr
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
#'  @param split.points [\code{numeric}]
#'  Vector containing standard (true) split points
#'  @param algo.order
#'  Vector that contains the standard order of algorithms; integers from 1:(N+M) without duplicates.
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

singleDataSituationData = function(situation, N, M, split.points, algo.order, p, sigma) {
  
  counter = 0
  repeat {
    counter = counter + 1
    if (situation == 1L) {
      # Return standard split points and algorithm order
      res.list = list(
        split.points = split.points,
        algo.order = algo.order
      )
    }
    
    if (situation == 2L) {
      # Return noisy split points and standard algorithm order
      res.list = list(
        split.points = rnorm((N - 1), split.points, sigma),
        algo.order = algo.order
      )
    }
    
    if (situation == 3L) {
      # Return chaotic split points and standard algorithm order
      res.list = list(
        split.points = sort(runif(N - 1)),
        algo.order = algo.order
      )
    }
    
    # Remove one random algo from front
    if (situation == 4L) {
      # Only with prob p switch
      if (rbinom(1, 1, p)) {
        # Which algorithm to remove?
        remove.id = sample(N, 1)
        # Border cases 1 and N:
        if (remove.id == 1L) {
          new.split = split.points[-1L]
        } else if (remove.id == N) {
          new.split = split.points[-(N - 1L)]
        } else{
          # Sample the new split.point
          left.split = split.points[remove.id - 1]
          right.split = split.points[remove.id]
          new.split = split.points
          new.split[remove.id ] = runif(1, left.split, right.split)
          new.split = new.split[-(remove.id - 1)]
        }
        res.list = list(
          algo.order = c(algo.order[-remove.id], remove.id),
          split.points = new.split
        )
        # ELSE: return defaults (no algorithm removed)
      } else {
        res.list = list(
          split.points = split.points,
          algo.order = algo.order
        )
      }
    }
    
    # Add one random algo to front
    if (situation == 5L) {
      # Only with prob p switch
      if (rbinom(1, 1, p)) {
        
        index.in = sample((N + 1), 1) - 1
        add.id = sample((N + 1):(N + M), 1)
        algo.order = algo.order[-add.id]
        algo.order = append(algo.order, add.id, after = index.in)
        
        if (index.in != 0)
          temp.sp = split.points[-index.in]
        sp.margin = c(0, split.points, 1)
        #FIXME Rosa wusste nicht was zu tun ist
        if (index.in == 0) {
          new.split = runif(1, 0, split.points[1])
          split.points = c(new.split, split.points)
        } else if (index.in == N) {
          
          new.split = runif(1, split.points[N - 1], 1)
          split.points = c(split.points, new.split)
        } else {
          new.splits = runif(2, sp.margin[index.in + 0:1] + 1e-2,
            sp.margin[index.in + 1:2] - 1e-2)
          split.points = append(temp.sp, new.splits, after = index.in - 1)
        }
        
        res.list = list(
          split.points = split.points,
          algo.order = algo.order
        )
        
        # ELSE: return defaults (no algorithm added)
      } else{
        res.list = list(
          split.points = split.points,
          algo.order = algo.order
        )
      }
    }
    
    if (situation == 6L) {
      # Only with prob p switch
      if (rbinom(1, 1, p)) {
        
        id.out = sample(N, 1)
        id.in = sample((N + 1):(N + M), 1)
        
        algo.order[id.out] = id.in
        algo.order[id.in] = id.out
        
        res.list = list(
          split.points = split.points,
          algo.order = algo.order
        )
        
      } else{
        res.list = list(
          split.points = split.points,
          algo.order = algo.order
        )
      }
    }
    
    if (situation == 7L) {
      # Only with prob p switch
      if (rbinom(1, 1, p)) {
        
        switch.ids = sample(N, 2)
        
        algo.order[switch.ids[1]] = switch.ids[2]
        algo.order[switch.ids[2]] = switch.ids[1]
        
        res.list = list(
          split.points = split.points,
          algo.order = algo.order
        )
        
      } else{
        res.list = list(
          split.points = split.points,
          algo.order = algo.order
        )
      }
    }
    
    if (situation == 8L) {
      res.list = list(
        split.points = split.points,
        algo.order = c(sample(N), (N + 1) : (N + M))
      )
    }
    
    if (situation == 9L) {
      res.list = list(
        split.points = sort(runif(N - 1)),
        algo.order = c(sample(N), (N + 1) : (N + M))
      )
    }
    
    # Sicherheitscheckt und notfalls Recall:
    dists = c(res.list$split.points, 1) - c(0, res.list$split.points)
    if (all(dists > 0.025)) {
      break
    }
    if (counter == 10L) {
      stop("Not able to find proper split.points. Please adjust sigma.")
    }
  }
  
  
  print(res.list$split.points)
  return(res.list)
}