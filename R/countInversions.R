
#' Find the index of first/last \code{TRUE} value in a logical vector.
#'
#' @param predicted [\code{numeric}]\cr
#'   Numeric vector vector.
#' @param true [\code{numeric}]\cr
#'   Numeric vector vector.
#' @return [\code{numeric(1)}].
#'   MSE
#' @export
countInversions = function(x) {
  .Call("do_countInversions", as.numeric(x), PACKAGE = "PFPT")
}
