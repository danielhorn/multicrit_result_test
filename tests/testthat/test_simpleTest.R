context("simpleTest")

test_that("procedure + plot + print runs without error", {
  data(apprSVMParetoFronts)
  
  # Avaible datasets: codrna, mnist, protein, vehicle
  data = subset(apprSVMParetoFronts, apprSVMParetoFronts$dataset == "codrna")
  
  # Start the front analysis with the main procedure
  res = selectPortfolio(
    data = data,
    var.cols = c("error", "execTime"),
    algo.col = "solver",
    repl.col = "repl",
    indicator = "hv",
    ref.point = c(1.1, 1.1),
    eta = 0.5,
    w = c(0.05, 0.95),
    cp = 0.1,
    normalize = TRUE
  )
  plot(res, colors = c("violet", "turquoise", "green", "red", "black", "blue"),
    make.pause = FALSE)
  
})
