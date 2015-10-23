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
load("replResults_codrna.RData")
data.codrna = results[, c("error", "execTime", "repl", "solver", "subsampling")]

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
data.codrna.false = myDataPreproc(data.codrna, FALSE)
data.vehicle.true = myDataPreproc(data.vehicle, TRUE)
data.mnist.true = myDataPreproc(data.mnist, TRUE)
data.protein.true = myDataPreproc(data.protein, TRUE)
data.codrna.true = myDataPreproc(data.codrna, TRUE)

data = data.codrna.true[, 1:4]
var.cols = c("error", "execTime")
algo.col = "solver"
repl.col = "repl"
indicator = "hv"
ref.point = c(1.1, 1.1)
lambda = 100
eta = 0.5
w = c(0.05, 0.95)
cp = 0.01
normalize = TRUE
colors = c("violet", "turquoise", "green", "red", "black", "blue")

res = mainTestProcedure(data = data, var.cols = var.cols, algo.col = algo.col,
  repl.col = repl.col, indicator = indicator, ref.point = ref.point,
  lambda = lambda, eta = eta, w = w, cp = cp, normalize = normalize) 
print(res)
plot(res, colors = colors, make.pause = TRUE)
