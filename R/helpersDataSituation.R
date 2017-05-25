# Helper functions for generateDataSituation

# Generates split points for different szenarios, where type determines whether they are fixed, noisy or without structure
generateSplitpoints = function(N, D, type = "normal", standard.splits, sigma) {
  if(type == "normal") {
    
    if(missing(standard.splits)) {
      split.dist = 1/N[1]
      splits = cumsum(rep(split.dist, (N[1]-1)))
    } else {
      splits = standard.splits
    }
    #split.points = matrix(rep(cumsum(rep(split.dist, (N-1))), D), ncol = D)
    split.points = NULL
    
    for(i in 1:D) {

      split.points = c(split.points, list(splits))
    }

    return(split.points)
  }
  
  if(type == "noisy") {
    if(missing(standard.splits)) {
      standard.splits = generateSplitpoints(N)
    }
    
    if(missing(sigma)) {
      sigma = 0.01
    }
    
    #noisy.split.points = matrix(rnorm(D * (N - 1), standard.splits, sigma), ncol = D)
    noisy.split.points = NULL
    
    for(i in 1:D) {
      noisy.split.points = c(noisy.split.points, list(rnorm((N-1), standard.splits, sigma)))
    }
    #FIXME: make sure split points are still sorted! (e.g. determine max sigma)
    
    return(noisy.split.points)
  }
  
  if(type == "chaotic") {
    chaotic.split.points = matrix(runif(D * (N - 1)), ncol = D)
    chaotic.split.points = apply(chaotic.split.points, 2, sort)
    chaotic.split.points = as.list(data.frame(chaotic.split.points))
    
    return(chaotic.split.points)
  }
}

# Generates algorithm orders for different szenarios, where type can be:
# fix: fix order, no changes
# in: p% of data sets has one normally dominated algorithm now non-dominated
# out: p% of data sets has one normally non-dominated algorithm now dominated
# switch: p% of data sets has an in & out switch (nd to d, d to nd)
# swap: p% of data sets have two non-dominated algorithms swapped
# random: completely random order for every data set

generateOrder = function(N, M, D, type, p) {
  
  if(type == "fix") {
    #return(matrix(rep(1:(N[1]+M[1]),D), ncol = D)))
    orderlist = NULL
    for(i in 1:D) {
      
      if(type == "fix") {
        orderlist = c(orderlist, list(1:(N[i]+M[i])))
        
      } else if(type == "out") {
        p.out = runif(1)
        if(p.out < p) {
          switched.alg = sample(size = 1, x = 1:N[i])
          
          current.order = 1:(N[i]+M[i])
          current.order = current.order[-switched.alg]
          current.order = c(current.order,switched.alg)
          
        } else current.order = 1:(N[i]+M[i])
        
        orderlist = c(orderlist, list(current.order))
        
        orderlist = c(orderlist, list(current.order))
        
      } else if(type == "in") {
        
        p.in = runif(1)
        if(p.in < p) {
          index.in = sample(size = 1, x = 1:N[i])
          switched.alg = sample(size = 1, x = (N[i]+1):M[i])
          
          current.order = 1:(N[i]+M[i])
          current.order = current.order[-switched.alg]
          current.order = c(current.order[1:index.in],switched.alg,current.order[(index.in+1):(N[i]+M[i]-1)])
        } else current.order = 1:(N[i]+M[i])
        
        orderlist = c(orderlist, list(current.order))
        
      } else if(type == "switch") {
        
        p.switch = runif(1)
        if(p.switch < p) {
          switch.out = sample(size = 1, x = 1:N[i])
          switch.in = sample(size = 1, x = (N[i]+1):M[i])
          
          current.order = 1:(N[i]+M[i])
          current.order = current.order[-c(switch.out,switch.in)]
          current.order = c(current.order[1:(switch.out-1)],switch.in,current.order[(switch.out+1):(N[i]+M[i]-2)], switch.out)
          
        } else current.order = 1:(N[i]+M[i])
        
        orderlist = c(orderlist, list(current.order))
        
      } else if(type == "swap") {
        p.swap = runif(1)
        if(p.swap < p) {
          swaps = sample(size = 2, x = 1:N[i], replace = FALSE)
          swaps = sort(swaps)
          current.order = 1:(N[i]+M[i])
          current.order = c(current.order[1:(swaps[1]-1)],swaps[2],current.order[(swaps[1]+1):(swaps[2]-1)], swaps[1], current.order[-(1:swaps[2])])
        }
      }
    }
    return(orderlist)
  }
}