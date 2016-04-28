#' Plots validation data for the algorithm portfolio selection.
#'
#' @param val.data 
#'   Result of \code{generateValidationData}.
#' @param repl [\code{integer}] \cr
#'   Which replication should be plotted? Default is the first replication.
#' @param grey [\code{logical}] \cr
#'   Should the plot be in grey shades? Default is \code{FALSE}.
#' @param return.plot [\code{logical}] \cr
#'   Should the plot be returned by the function (or just plotted)? Default
#'   is \code{FALSE}.
#' @param title [\code{character}] \cr
#'   Title for the plot. Default is no title.
#' @param legend [\code{logical}] \cr
#'   Should a legend be plotted? Default is \code{TRUE}.

#' @export
plotValidationData = function(val.data, repl = 1L, grey = FALSE, return.plot = FALSE, 
  title = "", legend = TRUE) {
  n = length(val.data$landscape$f.list)

  dat = val.data$validationData[val.data$validationData$repl == repl, ]
  algo.names = val.data$algos

  pl = ggplot(data = dat)
  pl = pl + ylim(min(dat$y), max(dat$y)) + xlim(min(dat$x), max(dat$x))
  pl = pl + geom_point(size = 2, mapping = aes_string(x = "x", y = "y", colour = "algorithm", 
    shape = "algorithm"))
  
  if (legend) {
  pl = pl + scale_shape_manual("algorithm", values = c(16, 17, 15, 18, 3, 4, 7, 8, 9, 12))
  pl = pl + guides(colour = guide_legend("algorithm", override.aes = list(size = 2, linetype = 0)))
  } else {
    pl = pl + scale_shape_manual("algorithm", values = c(16, 17, 15, 18, 3, 4, 7, 8, 9, 12), guide = FALSE)
  }
  pl = pl + ggtitle(title)

  for (i in 1:n) {
    if (grey) {
      pl = pl + stat_function(aes_string("x"), data = data.frame(x = c(0, 1)), 
        fun = val.data$landscape$f.list[[i]], size = 1, alpha = 0.5)
      
    } else {
      pl = pl + stat_function(aes_string("x", colour = "algorithm"), 
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
