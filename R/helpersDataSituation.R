# Helper functions for generateDataSituation

# Generates split points for different szenarios, where type determines whether they are fixed, noisy or without structure
generateSplitpoints = function(N, D, type = "normal", expected.splits, sigma) {
  if(type == "normal") {
    split.dist = 1/N
    split.points = matrix(rep(cumsum(rep(split.dist, (N-1))), D), ncol = D)
    
    return(split.points)
  }
  
  if(type == "noisy") {
    if(missing(expected.splits)) {
      expected.splits = generateSplitpoints(N)
    }
    
    if(missing(sigma)) {
      sigma = 0.01
    }
    
    noisy.split.points = matrix(rnorm(D * (N - 1), expected.splits, sigma), ncol = D)
    #FIXME: make sure split points are still sorted! (e.g. determine max sigma)
    
    return(noisy.split.points)
  }
  
  if(type == "chaotic") {
    chaotic.split.points = matrix(runif(D * (N - 1)), ncol = D)
    chaotic.split.points = apply(chaotic.split.points, 2, sort)
    
    return(chaotic.split.points)
  }
}

# Generates algorithm orders for different szenarios, where type can be:
# fix: fix order, no changes
# 

generateOrder = function(N, M, D, type = "fix") {
  
  if(type == "fix") {
    return(matrix(rep(1:(N[1]+M[1]),D), ncol = D)))
  }
}