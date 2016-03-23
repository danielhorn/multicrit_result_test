# val.data: result of generateValidationData
#' @export
plotValidationData = function(val.data, repl = 1L, grey = FALSE, return.plot = FALSE, 
  title = "", legend = TRUE) {
  n = length(val.data$landscape$f.list)

  dat = val.data$validationData[val.data$validationData$repl == repl, ]
  algo.names = val.data$landscape$algos

  pl = ggplot(data = dat)
  pl = pl + ylim(0, 1) + xlim(0, 1)
  pl = pl + geom_point(mapping = aes(x = x, y = y, colour = algorithm, shape = algorithm), size = 2)
  
  if (legend) {
  pl = pl + scale_shape_manual("algorithm", values = c(16, 17, 15, 18, 3, 4, 7, 8, 9, 12))
  pl = pl + guides(colour = guide_legend("algorithm", override.aes = list(size = 2, linetype = 0)))
  } else {
    pl = pl + scale_shape_manual("algorithm", values = c(16, 17, 15, 18, 3, 4, 7, 8, 9, 12), guide = FALSE)
  }
  pl = pl + ggtitle(title)

  for (i in 1:n) {
    if (grey) {
      pl = pl + stat_function(aes(x), data = data.frame(x = c(0, 1)), 
        fun = val.data$landscape$f.list[[i]], size = 1, alpha = 0.5)
      
    } else {
      pl = pl + stat_function(aes(x, colour = algorithm), 
        data = data.frame(x = c(0, 1), algorithm = algo.names[i]), 
        fun = val.data$landscape$f.list[[i]], size = 1)
    }
  }
  
  if (grey) {
    pl = pl + scale_color_manual(values = rep("black", length(val.data$landscape$f.list)), guide = legend)
    pl = pl + theme_bw()
  }
  
  if (return.plot) {
    return(pl)
  } else {
    print(pl)
  }
}
