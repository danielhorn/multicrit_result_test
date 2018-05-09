
eafGGPlot = function(data, var.names, group.name, replication.name, data.name,
  percentiles = 50) {
  
  d = do.call(rbind, lapply(split(data, data[, data.name]), function(d) {
    eaf = eaf:::eafs(points = d[, var.names], sets = d[, replication.name], 
      groups = d[, group.name], percentiles = percentiles)
    names(eaf) = c(var.names, "percentiles", group.name)
    eaf$dataset = d[1, data.name]
    eaf
  }))

  x.max = max(d[, var.names[1L]])
  y.max = max(d[, var.names[2L]])
  
  d.splitted = split(d, d[, c(group.name, "percentiles", "dataset")])
  
  f = function(x) {
    x.min2 = min(x[, var.names[1L]])
    y.min2 = min(x[, var.names[2L]])
    group = x[1L, group.name]
    percentiles = x[1L, "percentiles"]
    
    points = data.frame(c(x.min2, x.max), c(y.max, y.min2), percentiles, group)
    names(points) = c(var.names, "percentiles", group.name)
    points$dataset = x[1, "dataset"]
    
    rbind(points[1L, ], x, points[2L, ])
    
  }
  
  d.splitted2 = lapply(d.splitted, f)
  d2 = do.call(rbind, d.splitted2)
  d2$percentiles = as.factor(d2$percentiles)
  
  p = ggplot(data = d2, ggplot2::aes_string(var.names[1L], var.names[2L], 
    colour = group.name, linetype = "percentiles")) +
    geom_line() + facet_wrap("dataset")
  p
}

