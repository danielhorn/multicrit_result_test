#' Performance of different approximative SVM solvers
#'
#' A dataset containing the Pareto Fronts of differen approximative SVM sovlers
#' with respect to the objectives accuary and training time. A priori a multi-
#' objective parameter tuning has been done for every solver, the resulting
#' Pareto fronts of 10 independent optimizations runs on 4 data sets are given 
#'
#' @format A data frame with 994 rows and 5 variables:
#' \describe{
#'   \item{dataset}{Pareto front on which datatset}
#'   \item{solver}{Pareto front for which SVM solver}
#'   \item{repl}{number of replication}
#'   \item{error}{first performance measure - error = 1 - accuary}
#'   \item{execTime}{second performance measure - the training time}
#' }
"apprSVMParetoFronts"

#' Performance of different approximative SVM solvers
#'
#' A dataset containing the Pareto Fronts of differen approximative SVM sovlers
#' with respect to the objectives accuary and training time. A priori a multi-
#' objective parameter tuning has been done for every solver, the resulting
#' Pareto fronts of 10 independent optimizations runs on 4 data sets are given.
#' In contrast to the dataset \code{apprSVMParetoFronts} here each solver was
#' allowed to use subsampling as addition approximation strategy. The
#' subsampling rate itself was a parameter of the multi-objective tuning.
#'
#' @format A data frame with 3109 rows and 5 variables:
#' \describe{
#'   \item{dataset}{Pareto front on which datatset}
#'   \item{solver}{Pareto front for which SVM solver}
#'   \item{repl}{number of replication}
#'   \item{error}{first performance measure - error = 1 - accuary}
#'   \item{execTime}{second performance measure - the training time}
#' }
"apprSubsampleSVMParetoFronts"