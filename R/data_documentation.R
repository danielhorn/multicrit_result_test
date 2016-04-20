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

#' 
#' Validation of the method \link{selectPortfolio} using artificial
#' data from \link{generateValidationData}.
#' 
#' Details of the experiment are given in the paper Multi-Objective Selection of
#' AlgorithmPortfolios: Experimental Validation, submitted to the PPSN2016.
#'
#' @format A data frame with 86400 rows and 10 variables:
#' \describe{
#'   \item{id}{Integer. Running experiment id.}
#'   \item{N}{Integer. Number of active fronts}
#'   \item{split.type}{Factor. Uniform or non-unifrom split points between the active fronts?}
#'   \item{M}{Integer. Number of interference fronts.}
#'   \item{k}{Integer. Size of the discrete approximations.}
#'   \item{discretize.type}{Factor. Which of the 4 discretize methods was used?}
#'   \item{replications.type}{Factor. Which of the 3 replication methods was used?}
#'   \item{repl}{Integer. Differentiates the 100 replications per setting.}
#'   \item{z-value}{The resulting z-value of the experiment}
#' }
"validateMOSAPData"