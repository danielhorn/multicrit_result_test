# Maß zum Vergleich von der wahren und der geschaetzten gemeinsamen 
# Paretofront. Nimmt Werte zwischen 0 (schlecht) und 1 (gut) an.

## landscape: Ergebnis von generateParetoLandscape
## portfolio: Ergebnis von MOSAP::selectPortfolio
#'@export
compareCommonParetoFronts = function(landscape, portfolio) {
  N = length(landscape$split.points) - 1
  n = length(portfolio$best.algo.order)
  
  true.split.points = landscape$split.points
  estimated.split.points = c(0, portfolio$split.vals, 1)
  
  rel.algo.names = paste("algo", 1:N, sep = "")
  estimated.algo.order = portfolio$best.algo.order
  
  res = 0
  
  for (i in 1:N) {
    for (j in 1:n) {
      if (rel.algo.names[i] == estimated.algo.order[j]) {
        l = max(true.split.points[i], estimated.split.points[j])
        u = min(true.split.points[i + 1], estimated.split.points[j + 1])
        res = res + ifelse(u - l > 0, u - l, 0)
      }
    }
  }
  return(res) 
}
