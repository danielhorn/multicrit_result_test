
eafGGPlot = function(data, var.names, group.name, replication.name, 
  percentiles = 50) {
  
  d = eaf:::eafs(points = data[, var.names], sets = data[, replication.name], 
    groups = data[, group.name], percentiles = percentiles)
  names(d) = c(var.names, "percentiles", group.name)
  d$percentiles = as.factor(d$percentiles)
  
  x.max = max(d[, var.names[1L]])
  y.max = max(d[, var.names[2L]])

  d.splitted = split(d, d[, c(group.name, "percentiles")])
  
  f = function(x) {
    x.min2 = min(x[, var.names[1L]])
    y.min2 = min(x[, var.names[2L]])
    group = x[1L, group.name]
    percentiles = x[1L, "percentiles"]
    
    points = data.frame(c(x.min2, x.max), c(y.max, y.min2), percentiles, group)
    names(points) = c(var.names, "percentiles", group.name)

    rbind(points[1L, ], x, points[2L, ])
    
  }
     
  d.splitted2 = lapply(d.splitted, f)
  d2 = do.call(rbind, d.splitted2)
  

  p = ggplot(data = d2, ggplot2::aes_string(var.names[1L], var.names[2L], 
    colour = group.name, linetype = "percentiles"))
  p = p + geom_line()
  p
}

