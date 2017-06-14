# FIXME: Rosa dokumentiert noch
getAlgoPar = function(algo.obj, par.name) {
  environment(algo.obj)[[par.name]]
}


setAlgoPar = function(algo.obj, par.name, value) {
  environment(algo.obj)[[par.name]] = value
  invisible(NULL)
}


getAlgoRangeY = function(algo.obj) {
  algo.obj(getAlgoRangeX(algo.obj))
}


getAlgoRangeX = function(algo.obj) {
  attr(algo.obj, which = "range.x")
}


isAlgoParetoOpt = function(algo.obj) {
  !is.null(getAlgoRangeX(algo.obj))
}


print.algo.obj = function(x) {
  print("Diest ist eine algo obj")
}
