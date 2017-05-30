singleDataSituationData = function(situation, N, M, split.points, algo.order, p, sigma) {
  
  if(situation == 1L) {
    
    
    
  }
  
  # Remove one random Algo from Front
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
      # ELSE: Wir machen keine Vertauschung, sondern geben die
      # Defaults zurueck
    } else {
      res.list = list(
        split.points = split.points,
        algo.order = algo.order
      )
    }
  }
  
  
  if (situation == 9L) {
    res.list = list(
      split.points = sort(runif(N - 1)),
      algo.order = c(sample(N), (N + 1) : (N + M))
    )
  }
  return(res.list)
}