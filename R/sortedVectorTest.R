#' Sorted Vector Test
#' 
#' Is the input vector sorted with respect to the in order given order?
#' Verified via a permutation test:
#' H_0: The elements in the vector are randomly permutated
#' H_1: The element in the vector follow the order given in order.
#' 
#' @param x [\code{numeric | factor}]\cr
#'   Input vector.
#' @param order [\code{numeric | factor}]\cr
#'   Vector specifying the order for the element in x. Must have the same type
#'   as x. Default is for numeric vectors the numerical order, for factors
#'   the order of the levels, for character vectors the first occurance in x.   
#' @param [\code{integer(1)}]\cr
#'   Count of permutations used for the test.
#' @return [\code{numeric(1)}].
#'   p-Value
#'   
#' @details
#'   n random permutations of x are drawn and for each permutation the number
#'   of inversions with respect to the given order is calculated. The rate of
#'   random permutations having less inversions than x is returned.
#'   For this test do make sense, length(unique(x)) << length(x) should hold.
#' @export

sortedVectorTest = function(x, order = NULL, n = 1e5) {
  
  assertVector(x, any.missing = FALSE)
  x.class = class(x)
  assertChoice(class(x), choices = c("numeric", "integer", "factor", "character"))
  
  if (is.null(order)) {
    if (x.class %in% c("numeric", "integer"))
      order = sort(unique(x))
    if (x.class == "factor")
      order = levels(x)
    if (x.class == "character")
      order = unique(x)
  } else {
    assertVector(order, any.missing = FALSE, unique = TRUE)
    if (x.class != class(order))
      stop("x and order must have the same class!")
  }
  
  new.x = numeric(length(x))
  for (i in seq_along(order))
    new.x[x == order[i]] = i
  
  distr = replicate(n, countInversions(sample(new.x)))
  value = countInversions(new.x)
  mean(distr < value)
}
