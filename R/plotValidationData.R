# val.data: result of generateValidationData
plotValidationData = function(val.data, repl = 1L, grey = FALSE) {
  n = length(val.data$landscape$f.list)

  dat = val.data$validationData[val.data$validationData$repl == repl, ]
  algo.names = val.data$landscape$algos

  pl = ggplot(data = dat)
  pl = pl + ylim(0, 1) + xlim(0, 1)
  pl = pl + geom_point(mapping = aes(x = x, y = y, colour = algorithm, shape = algorithm), size = 2)
  pl = pl + scale_shape_manual("algorithm", values = c(16, 17, 15, 18, 3, 4, 7, 8, 9, 12))
  pl = pl + guides(colour = guide_legend("algorithm", override.aes = list(size = 2, linetype = 0)))
  
  for (i in 1:n) {
    pl = pl + stat_function(aes(x, colour = algorithm), 
      data = data.frame(x = c(0, 1), algorithm = algo.names[i]), 
      fun = val.data$landscape$f.list[[i]], size = 1)
  }
  
  if (grey) {
    pl = pl + scale_color_grey()
  }
  
  print(pl)
}

