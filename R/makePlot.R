makePlot = function(landscape, points = NULL, ...) {
  n = length(landscape$f.list)
  k = nrow(points)/n
  
  col = rainbow(n)
  f = landscape$f.list[[1]]
  curve(f, 0, 1, ylim = c(0,1), col = col[1], ...)
  
  for (i in 2:n) {
    f = landscape$f.list[[i]]
    curve(f, 0, 1, ylim = c(0,1), add = TRUE, col = col[i])
  }
  
  if (!is.null(points)) {
    points(points$x, points$y, col = col[points$algorithm], pch = 16)
  }
  
  legend("topright", paste("algo", 1:n), col = col, lty = 1, pch = 16)
  
}  

