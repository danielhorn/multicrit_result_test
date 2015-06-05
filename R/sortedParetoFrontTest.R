#' Sorted Pareto Front Test
#' 
#' Gets a dataset with 4 variables: 2 variables are the objective space of
#' a multicriteria optimization problem. 1 variable assigns eachs observation
#' to a class (algorithm), the order of all classes must be defined via order.
#' The last variable specifies multiple replications - the test is performed
#' for every replication.
#' First, for every replication, the combined Pareto front of all algorithm
#' is computed and sorted with respect to one of the objective variables.
#' (Sorting increasingly with respect to one is equivalent to sorting decreasingly
#'  with respect to the other). For each associated vector of algorithms the 
#'  sortedVectorTest is performed, a vector of p-Values is returned.
#' 
#' 
#' @param formula \cr
#'   Input vector.
#' @param data [\code{dataframr}]\cr
#'   Vector specifying the order for the element in x. Must have the same type
#'   as x. Default is for numeric vectors the numerical order, for factors
#'   the order of the levels, for character vectors the first occurance in x.
#' @param order [\code{numeric | factor}]\cr
#'   Vector specifying the order for the element in x. Must have the same type
#'   as x. Default is for numeric vectors the numerical order, for factors
#'   the order of the levels, for character vectors the first occurance in x.   
#' @param n [\code{integer(1)}]\cr
#'   Count of permutations used for the test.
#' @return [\code{numeric}].
#'   Vector of p-Value
#' @export

sortedParetoFrontTest = function(formula, data, order, n) {
  # Extract informations from formula
  algo = as.character(formula[[2]])
  repl = as.character(formula[[3]][[3]])
  vars = as.character(formula[[3]][[2]])[-1]
  
  # Split dataset into it replications
  data.splitted = split(data, data[, repl])
  
  # Apply Pareto-Filt
  data.splitted = lapply(data.splitted, function(d)
    d[nds_rank(as.matrix(t(d[, vars]))) == 1, ])
  
  # sort data via first variable
  data.splitted = lapply(data.splitted, function(d) d[order(d[, vars[1]]), ])
  
  # extract algo vectors
  orderd.algo.vectors = extractSubList(data.splitted, algo)
  
  # Now, for every replication, apply the permutation test
  perms = sapply(orderd.algo.vectors, sortedVectorTest, order = order, n = n)
}
