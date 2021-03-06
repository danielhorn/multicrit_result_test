portfolioToParetoFront = function(data, best.algo.order, split.vals, var.cols,
  algo.col, data.col, repl.col) {
  
  split.vals = c(0, split.vals, 1)
  
  # for each data set
  oneDataset = function(single.data) {
    
    data.splitted = split(single.data, single.data[, repl.col])
    
    # Each algo is only interesting in its own "area" defined by split.vals 
    # Values for "denormalization"
    min.val = min(single.data[, var.cols[1]])
    max.val = max(single.data[, var.cols[1]])
    reduceData = function(d) {
      split.vals2 = min.val + split.vals * (max.val - min.val)
      #d = subset(d, d[, algo.col] %in% best.algo.order)
      do.call(rbind, lapply(seq_along(best.algo.order), function(i) {
        is.in = d[, var.cols[1]] >= split.vals2[i] & d[, var.cols[1]] <= split.vals2[i + 1]
        subset(d, d[, algo.col] %in% best.algo.order[i] & is.in)
      }))
    }
    reduced.fronts = do.call(rbind, lapply(data.splitted, reduceData))
    reduced.fronts[, algo.col] = factor(reduced.fronts[, algo.col])
    
    # Hack the EAF package - use the underlying eaf function to get points
    # for my own eaf plotting
    d = eaf:::eafs(points = reduced.fronts[, var.cols],
      sets = reduced.fronts[, repl.col], groups = reduced.fronts[, algo.col], percentiles = 50)
    d = d[order(d[, 1L]), ]
    # I need another pareto filter now
    d = d[!is_dominated(t(d[, 1:2])), ]
    d[, data.col] = single.data[1, data.col]
    return(d)
  }
  
  d = do.call(rbind, lapply(split(data, data[, data.col]),
    function(single.data) oneDataset(single.data)))
  
  # Now, reduce over data sets, again, median
  d = eaf:::eafs(points = d[, c("X1", "X2")], sets = d[, data.col], percentiles = 50)
  d = as.data.frame(d)
  d$algo = sapply(d$V1, function(x) best.algo.order[sum(x >= split.vals)])
  
  # now it gets a little bit crazy ... add "middlepoints" between groups
  # so a nice colored line can be plotted
  # only necessary if there is more than 1 relevant algo
  if (length(unique(d$algo)) > 1L)  {
    # first, get "border" points. works since d is orderd
    borders = which(d[-1L, 4L]  != d[-nrow(d), 4L])
    # now, for each border, add 2 identical points, with mean of border and border + 1
    # with different algos, this means different colours
    new.points = (d[borders, 1:2] + d[borders + 1L, 1:2]) / 2L
    if (length(split.vals) - 2 == nrow(new.points)) {
      new.points$V1 = split.vals[2:(length(split.vals) - 1)]
    }
    new.points = data.frame(
      V1 = rep(new.points$V1, each = 2L),
      V2 = rep(new.points$V2, each = 2L),
      V3 = 50,
      algo = d[sort(c(borders, borders + 1L)), 4L]
    )
    d = rbind(d, new.points)
    d = d[order(d[, 1L]), ]
  }
  
  # another nasty hack. if an algo has 2 non-connected parts of the front ...
  # we have to assign 2 different group levels for ggplot
  last.els.per.algo = c(0, which(d[-nrow(d), 4] != d[-1, 4]), nrow(d))
  # now give a unique id for every part
  counts.per.part = diff(last.els.per.algo)
  ids = unlist(lapply(seq_along(counts.per.part), function(i) rep(i, counts.per.part[i])))
  # an add this id to the group coloum
  d$group = factor(paste(d[, 4], ids))
  names(d)[4] = "colour"
  
  # last but not least - we loose the max values atm, so add points for them
  # at point with best x1 value:
  # use 3. quartile of raw data for max values
  #max.vals = apply(sapply(data.splitted, function(dd) apply(dd[, var.cols], 2, max)),
  #                 1, quantile, probs = 0.75)
  #best.x1 = d[1L, ]
  #best.x1$X2 = max.vals[2L]
  #best.x2 = d[nrow(d), ]
  #best.x2$X1 = max.vals[1L]
  #d = rbind(best.x1, d, best.x2)
  
  return(d)
}