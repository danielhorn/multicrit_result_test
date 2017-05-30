# FIXME: Rosa Dokumentiert noch

singleDataSituationData = function(situation, N, M, split.points, algo.order, p, sigma) {
  
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
      
      index.in = sample((N + 1), 1) -1
      add.id = sample((N + 1):(N + M), 1)
      algo.order = algo.order[-add.id]
      algo.order = append(algo.order, add.id, after = index.in)
      
      temp.sp = split.points[-index.in]
      sp.margin = c(0, split.points, 1)
      
      new.splits = runif(2, sp.margin[index.in + 0:1], sp.margin[index.in + 1:2])
      split.points = append(temp.sp, new.splits, after = index.in)
      
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
  return(res.list)
}