# intern R script for loading and preprocessing the data
library(emoa)
path = "../../svm_large_data_publications_and_orga/results/expWithRepls/"

# my bit data preproc
load(paste(path, "replResults_vehicle.RData", sep = ""))
data.vehicle = results[, c("error", "execTime", "repl", "solver", "subsampling")]
data.vehicle$dataset = "vehicle"
load(paste(path, "replResults_mnist.RData", sep = ""))
data.mnist = results[, c("error", "execTime", "repl", "solver", "subsampling")]
data.mnist$dataset = "mnist"
load(paste(path, "replResults_protein.RData", sep = ""))
data.protein = results[, c("error", "execTime", "repl", "solver", "subsampling")]
data.protein$dataset = "protein"
load(paste(path, "replResults_codrna.RData", sep = ""))
data.codrna = results[, c("error", "execTime", "repl", "solver", "subsampling")]
data.codrna$dataset = "codrna"

# remove "expired" jobs and jobs with to high error, add loged exec.time and
# make dataset without subsampling
myDataPreproc = function(data, subsampling = FALSE) {
  # Remove Expired points
  data = data[data$execTime < 28800L, ]
  # Remove points with bad accuracy and log execTime
  data = do.call(rbind, 
    lapply(split(data, data$repl), function(d) d[d$error < min(d$error) + 0.1, ]))
  data$execTime = log(data$execTime)
  
  # Apply Pareto Filter:
  #data.splitted = split(data, data[, c("repl", "solver")])
  #data.splitted = lapply(data.splitted, function(d) {
  #  if (nrow(d) == 0L)
  #    d
  #  else
  #    d[nds_rank(as.matrix(t(d[, c("error", "execTime")]))) == 1L, ]
  #})
  #data = do.call(rbind, data.splitted)
  
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

# First Dataset - without subsampling
apprSVMParetoFronts = rbind(
  data.codrna.false,
  data.mnist.false,
  data.protein.false,
  data.vehicle.false
)[, c(6, 4, 3, 2, 1)]

# Second Dataset - wit subsampling
apprSubsampleSVMParetoFronts = rbind(
  data.codrna.true,
  data.mnist.true,
  data.protein.true,
  data.vehicle.true
)[, c(6, 4, 3, 2, 1)]

rownames(apprSVMParetoFronts) = 1:nrow(apprSVMParetoFronts)
rownames(apprSubsampleSVMParetoFronts) = 1:nrow(apprSubsampleSVMParetoFronts)

save(apprSVMParetoFronts, file = "data/apprSVMParetoFronts.RData")
save(apprSubsampleSVMParetoFronts, file = "data/apprSubsampleSVMParetoFronts.RData")

