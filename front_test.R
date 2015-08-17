library(BBmisc)
library(devtools)
load_all()

# my bit data preproc
load("replResults_vehicle.RData")
data.vehicle = results[, c("error", "execTime", "repl", "solver", "subsampling")]
load("replResults_mnist.RData")
data.mnist = results[, c("error", "execTime", "repl", "solver", "subsampling")]

# remove "expired" jobs and jobs with to high error, add loged exec.time and
# make dataset without subsampling
data.vehicle = data.vehicle[data.vehicle$execTime < 28800L, ]
data.vehicle = do.call(rbind, 
  lapply(split(data.vehicle, data.vehicle$repl), function(d) d[d$error < min(d$error) + 0.1, ]))
data.vehicle$execTime = log(data.vehicle$execTime)
data.vehicle.false = subset(data.vehicle, !data.vehicle$subsampling)
data.vehicle.true = subset(data.vehicle, data.vehicle$subsampling)
rm(data.vehicle)
data.vehicle.true$solver = factor(paste(data.vehicle.true$solver, data.vehicle.true$subsampling, sep = "_"))
data.vehicle.true$solver = factor(data.vehicle.true$solver)

data.mnist = data.mnist[data.mnist$execTime < 28800L, ]
data.mnist = do.call(rbind, 
  lapply(split(data.mnist, data.mnist$repl), function(d) d[d$error < min(d$error) + 0.1, ]))
data.mnist$execTime = log(data.mnist$execTime)
data.mnist.false = subset(data.mnist, !data.mnist$subsampling)
data.mnist.true = subset(data.mnist, data.mnist$subsampling)
rm(data.mnist)
data.mnist.true$solver = factor(paste(data.mnist.true$solver, data.mnist.true$subsampling, sep = "_"))
data.mnist.true$solver = factor(data.mnist.true$solver)

formula = solver ~ error + execTime | repl
data = data.mnist.false
indicator = "epsilon"
normalize = TRUE
ref.point = c(1.1, 1.1)
lambda = 100
cp = 0.1

res = mainTestProcedure (formula = formula, data = data, 
  indicator = indicator, normalize = normalize,
  ref.point = ref.point, lambda = lambda, cp = cp)
print(res)
plot(res, colors = c("violet", "turquoise", "green", "red", "black", "blue"))
