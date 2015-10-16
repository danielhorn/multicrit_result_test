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

sortedParetoFrontClassification = function(data, var.cols, algo.col, repl.col, contrFun, cp) {
  requirePackages("rpart")  
  
  # Use the EAF points to learn the rpart
  # So, first, calculate them, exclude percentile coloumn
  eaf.front = eaf:::eafs(points = data[, var.cols], sets = data[, repl.col],
    groups = data[, algo.col], percentiles = 50)[, -(length(var.cols) + 1)]
  
  # Apply Pareto-filter
  var.ids = 1:(ncol(eaf.front) - 1L)
  eaf.front = eaf.front[nds_rank(as.matrix(t(eaf.front[, var.ids]))) == 1L, ]
  
  # calculate weights for the points - each point has its own contribution
  # as its weight
  weights = sapply(seq_row(eaf.front),
    function(i)
      contrFun(eaf.front[-i, -ncol(eaf.front)], eaf.front[, -ncol(eaf.front)])
    )
  
  # exclude one variable
  eaf.front = eaf.front[, -max(var.ids)]
  # now use rpart to get order of algorithms
  mod = rpart(groups ~ ., data = eaf.front, weights = weights, minsplit = 1)
  mod = prune(mod, cp = cp)
  
  # i don't see a "good" way to get the perm vector from the mod
  # so, get the vector of split values from the model, sort them, add 2 new
  # max / min values, predict with 1 value in each interval and tada
  # FIXME: This works only for 2 var.cols atm. 
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
  
  return(list(perm = perm, split.vals = split.vals))
}
