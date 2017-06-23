#' print validation data object
#' 
#' @param x [\code{mosap_result}]\cr
#'   validation data object
#' @param ... [\code{any}] \cr
#'   Not used.
#'   
#' @export
print.validation.obj = function(x, ...) {
  d = length(x$landscape.list)
  cat("Validation data consists of", d, "data sets")
}