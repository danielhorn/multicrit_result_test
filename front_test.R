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

data.vehicle = do.call(rbind, 
  lapply(split(data.vehicle, data.vehicle$repl), function(d) d[d$error < min(d$error) + 0.1, ]))
data.vehicle = data.vehicle[data.vehicle$error < min(data.vehicle$error) + 0.1, ]
data.vehicle$execTime = log(data.vehicle$execTime)
data.vehicle$solver2 = factor(paste(data.vehicle$solver, data.vehicle$subsampling, sep = "_"))

data.mnist = data.mnist[data.mnist$execTime < 28800L, ]
data.mnist = do.call(rbind, 
  lapply(split(data.mnist, data.mnist$repl), function(d) d[d$error < min(d$error) + 0.1, ]))
data.mnist$execTime = log(data.mnist$execTime)
data.mnist$solver2 = factor(paste(data.mnist$solver, data.mnist$subsampling, sep = "_"))

formula = solver2 ~ error + execTime | repl
alpha = 0.05
indicator = "hv"
sel.fun = "forward"
perm.test = "rpart"
kappa = 1e-02
normalize = TRUE
ref.point = c(1.1, 1.1)
lambda = 100
n = 1e4
cp = 0.1

res = mainTestProcedure (formula = formula, data = data.vehicle, alpha = alpha,
  indicator = indicator, sel.fun = sel.fun, perm.test = perm.test, kappa = kappa, normalize = normalize,
  ref.point = ref.point, lambda = lambda, n = n, cp = cp)
print(res)
plot(res, FALSE)

