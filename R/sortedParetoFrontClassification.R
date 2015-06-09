# Sorted Pareto Front: rpart Analysis
# 
# Gets a dataset with 4 variables: 2 variables are the objective space of
# a multicriteria optimization problem. 1 variable assigns eachs observation
# to a class (algorithm), the last variable specifies multiple replications.
# Since we look at the pareto fronts of the replication we have to consider
# only one of the 2 variables, we use the first one. First, for each replication
# the difference to the best value of this variable is calculated. Than all
# replications are merged and the combined vector of all differences is used
# to predict the algorithm labels via a decision tree (rpart).
# Last, the order of the algorithms on the front is extract from the model. 
# Note that here an algorithms must not occur exactly once in the order, but
# can occur multiple times or never. 
# 
# @param formula \cr
#   Input vector.
# @param data [\code{dataframr}]\cr
#   Vector specifying the order for the element in x. Must have the same type
#   as x. Default is for numeric vectors the numerical order, for factors
#   the order of the levels, for character vectors the first occurance in x.
# @return [\code{character}]
#   The preferred order of algorithms.

sortedParetoFrontClassification = function(formula, data) {
  requirePackages("rpart")  
  
  algo = as.character(formula[[2]])
  repl = as.character(formula[[3]][[3]])
  vars = as.character(formula[[3]][[2]])[-1]
  
  # Split dataset into it replications
  data.splitted = split(data, data[, repl])
  
  # Apply Pareto-Filt
  data.splitted = lapply(data.splitted, function(d)
    d[nds_rank(as.matrix(t(d[, vars]))) == 1, ])
  
  # normalize to [0, 1] and we only need var value and algo
  data.splitted = lapply(data.splitted, function(d)
    data.frame(value = normalize(d[, vars[1]], method = "range"), algo = d[, algo]))
  
  # now merge everything and sort along value
  data.classif = Reduce(rbind, data.splitted)
  data.classif = data.classif[order(data.classif$value), ]
  
  # now use rpart to get permutation
  mod = rpart(algo ~ value, data = data.classif, maxsurrogate = 0)
  
  # i don't see a "good" way to get the perm vector from the mod
  # so, get the vector of split values from the model, sort them, add 2 new
  # max / min values, predict with 1 value in each interval and tada
  split.vals = sort(mod$splits[, "index"])
  pred.vals = data.frame(value = rowMeans(cbind(
    c(min(split.vals) - 1, split.vals),
    c(split.vals, max(split.vals) + 1))
    ))
  
  # Build return vector
  perm = predict(mod, pred.vals, type = "class")
  
  # Neighboring classes can have the same label. Merge them.
  unequal.inds = perm[-1] != perm[-length(perm)]
  perm = perm[c(TRUE, unequal.inds)]
  split.vals = split.vals[unequal.inds]
  
  # Quick 10 fold crossvalidation for error estimation
  ids = sample(rep(1:10, length.out = nrow(data.classif)))
  mmce = 0
  for (i in 1:10) {
    mod = rpart(algo ~ value, data = data.classif[ids != i, ], maxsurrogate = 0)
    mmce = mmce + mean(predict(mod, data.classif[ids == i, ], type = "class") != data.classif[ids == i, "algo"]) / 10
  }
  return(list(perm = perm, split.vals = split.vals, mmce = mmce))
}
