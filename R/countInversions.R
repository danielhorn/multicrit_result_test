#' countInversions
#'
#' Counts the number of inversions in the numeric vector x. An inversion occurs if
#' \eqn{x[i] > x[j]} and \eqn{i < j}.
#'
#' @param x [\code{numeric}]\cr
#'   Numeric vector.
#' @return [\code{integer(1)}].
#'   number of inversions
#' @export
countInversions = function(x) {
  assertNumeric(x)
  .Call("do_countInversions", as.numeric(x), PACKAGE = "PFPT")
}
