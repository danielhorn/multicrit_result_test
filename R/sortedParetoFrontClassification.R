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
# @param cp [\code{numeric(1)}]\cr
#  Complexity parameter for rpart tuning
# @return [\code{character}]
#   The preferred order of algorithms.

sortedParetoFrontClassification = function(data, var.cols, algo.col, repl.col,
  data.col, contrFun, cp) {
  requirePackages("rpart")  
  
  # Use the EAF points to learn the rpart.
  # So, first, calculate them, for each data set, exclude percentile coloumn
  # FIXME magic number
  eaf.front = do.call(rbind, lapply(split(data, data[, data.col]), function(d) {
    # Calculate EAF for each data set
    eaf = eaf:::eafs(points = d[, var.cols], sets = d[, repl.col],
      groups = d[, algo.col], percentiles = 50)[, -(length(var.cols) + 1)]
    # apply Pareto filter
    eaf = eaf[nds_rank(as.matrix(t(eaf[, 1:2]))) == 1L, ]
    # calculate weights for the points - each point has its own contribution
    # as its weight
    eaf$weights = sapply(seq_row(eaf), function(i) contrFun(eaf[-i, 1:2], eaf[, 1:2]))
    eaf$dataset = d[1, data.col]
    eaf
  }))
  
  # now use rpart on one variable to get order of algorithms
  mod = rpart::rpart(groups ~ X1, data = eaf.front, weights = eaf.front$weights, minsplit = 1)
  mod = rpart::prune(mod, cp = cp)
  
  # Also calculate the training-accuracy
  acc = mean(predict(mod, eaf.front, type = "class") == eaf.front$groups)
  
  # i don't see a "good" way to get the perm vector from the mod
  # so, get the vector of split values from the model, sort them, add 2 new
  # max / min values, predict with 1 value in each interval and tada
  split.vals = sort(mod$splits[, "index"])
  pred.vals = data.frame(X1 = rowMeans(cbind(
    c(min(split.vals) - 1, split.vals),
    c(split.vals, max(split.vals) + 1))
    ))
  
  # Build return vector
  perm = predict(mod, pred.vals, type = "class")
  
  # Neighboring classes can have the same label. Merge them.
  unequal.inds = perm[-1] != perm[-length(perm)]
  perm = perm[c(TRUE, unequal.inds)]
  split.vals = split.vals[unequal.inds]
  
  return(list(perm = perm, split.vals = split.vals, acc = acc))
}
