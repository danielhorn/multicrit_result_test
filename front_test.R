library(BBmisc)
library(devtools)
load_all()

# my bit data preproc
load("replResults_vehicle.RData")
data.vehicle = results[, c("error", "execTime", "repl", "solver", "subsampling")]
load("replResults_mnist.RData")
data.mnist = results[, c("error", "execTime", "repl", "solver", "subsampling")]
load("replResults_protein.RData")
data.protein = results[, c("error", "execTime", "repl", "solver", "subsampling")]

# remove "expired" jobs and jobs with to high error, add loged exec.time and
# make dataset without subsampling
myDataPreproc = function(data, subsampling = FALSE) {
  data = data[data$execTime < 28800L, ]
  data = do.call(rbind, 
    lapply(split(data, data$repl), function(d) d[d$error < min(d$error) + 0.1, ]))
  data$execTime = log(data$execTime)
  
  if (subsampling) {
    data = subset(data, data$subsampling)
    data$solver = factor(paste(data$solver, data$subsampling, sep = "_"))
  }
  else {
    data = subset(data, !data$subsampling)
    data$solver = factor(data$solver)
  }
  return(data)
}

data.vehicle.false = myDataPreproc(data.vehicle, FALSE)
data.mnist.false = myDataPreproc(data.mnist, FALSE)
data.protein.false = myDataPreproc(data.protein, FALSE)
data.vehicle.true = myDataPreproc(data.vehicle, TRUE)
data.mnist.true = myDataPreproc(data.mnist, TRUE)
data.protein.true = myDataPreproc(data.protein, TRUE)

formula = solver ~ error + execTime | repl
indicators = c("hv", "epsilon", "r2")
normalize = TRUE
ref.point = c(1.1, 1.1)
lambda = 100
colors = c("violet", "turquoise", "green", "red", "black", "blue")

formula = solver ~ error + execTime | repl
data = data.protein.false
indicator = "hv"
normalize = TRUE
ref.point = c(1.1, 1.1)
lambda = 100
cp = 0.1

res = mainTestProcedure (formula = formula, data = data, 
  indicator = indicator, normalize = normalize,
  ref.point = ref.point, lambda = lambda, cp = cp)
print(res)
plot(res, colors = colors, FALSE)
