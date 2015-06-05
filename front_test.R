library(BBmisc)
library(devtools)
load_all()

# my bit data preproc
load("replResults_vehicle.RData")
data.vehicle = results[, c("error", "execTime", "repl", "solver", "subsampling")]
load("replResults_mnist.RData")
data.mnist = results[, c("error", "execTime", "repl", "solver", "subsampling")]
# remove "expired" jobs and jobs with to high error
data.vehicle = data.vehicle[data.vehicle$execTime < 28800L, ]
data.vehicle = data.vehicle[data.vehicle$error < min(data.vehicle$error) + 0.1, ]
data.vehicle$log.execTime = log(data.vehicle$execTime)
data.vehicle$solver2 = factor(paste(data.vehicle$solver, data.vehicle$subsampling, sep = "_"))
data.mnist = data.mnist[data.mnist$execTime < 28800L, ]
data.mnist = data.mnist[data.mnist$error < min(data.mnist$error) + 0.1, ]
data.mnist$log.execTime = log(data.mnist$execTime)
data.mnist$solver2 = factor(paste(data.mnist$solver, data.mnist$subsampling, sep = "_"))

formula = solver ~ error + execTime | repl
alpha = 0.05
perm.test = "rpart"
kappa = 1e-08
normalize = TRUE
ref.point = c(1.1, 1.1)
n = 1e4

#res = mainTestProcedure(solver ~ error + log.execTime | repl,
#  data.vehicle, 0.05, "hv", "mean.invs", 1e-4, TRUE, c(1.1, 1.1), 1e4)
#res2 = mainTestProcedure(solver ~ error + log.execTime | repl,
#  data.vehicle, 0.05, "hv", "test.invs", 1e-4, TRUE, c(1.1, 1.1), 1e4)
res3 = mainTestProcedure(solver2 ~ error + log.execTime | repl,
  data.vehicle, 0.05, "hv", "rpart", 1e-4, TRUE, c(1.1, 1.1), 1e4)
#print(res)
#plot(res)
#print(res2)
#plot(res2)
print(res3)
plot(res3)


