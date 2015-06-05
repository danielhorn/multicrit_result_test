#' relevantAlgosTest
#'
#' Gets matrix of contribution values, rows are replications, cols are algos
#' Tests via wilcox.test which algos perform significantly better than kappa
#' each test has nivea alpha / #tests (bonferroni)
#'
#' @param hv.matrix [\code{numeric}] \cr
#'   Input matrix. Rows are replication, cols are algos
#' @param kappa [\code{numeric[1]}]\cr
#'   Contribution lesser lesser than alpha?
#' @param alpha [\code{numeric | factor}]\cr
#'   Used niveau for bonerroni correction
#' @return [\code{logical}].
#'   Vector test results
#' @export
#' 
relevantAlgosTest = function(hv.matrix, kappa, alpha) {
  # to ensure the distribution is symmetric, log the data
  hv.matrix = log(hv.matrix)
  # now we can use wilcoxon tests:
  test.res = apply(hv.matrix, 2, wilcox.test, alternative = "greater", mu = log(kappa))
  p.vals = extractSubList(test.res, "p.value")
  # Now, which of them are significant? No niveau correction! We are doing "parallel"
  # tests with a given niveau. No overall significance interpretation is possible.
  return(p.vals < alpha)
}