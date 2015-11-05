#' selectPortfolio
#'
#' Method used to start the multi-objective algorithm portfolio selection
#'
#' @param data [\code{data.frame}] \cr
#'   Data to be analysed. Must contain all \code{var.cols}, \code{algo.col}
#'   and \code{repl.col}. \code{var.cols} should form a Pareto front for every
#'   combination of \code{repl.col} and \code{algo.col}.
#' @param var.cols [\code{character(2)}] \cr
#'   Names of columns with containing the values of the Pareto fronts.
#' @param algo.col [\code{character(1)}] \cr
#'   Name of column containing the particular algorithm.
#' @param repl.col [\code{character(1)}] \cr
#'   Name of column containing the particular algorithm.
#' @param indicator [\code{character(1)}] \cr
#'   Which multi-objective indicator should be used? Possible values are: \cr
#'    \dQuote{hv}: Dominated Hypervolume (S-Metric), the default \cr
#'    \dQuote{epsilon}: epsilon-indicator \cr
#'    \dQuote{r2}: R2-indicator \cr
#' @param ref.point [\code{numeric(2)}] \cr
#'   Reference point for the hypervolume indicator. Default is c(1.1, 1.1)
#' @param lambda [\code{numeric(1)}] \cr
#'   Number of vector used in the calculation of the R2-indicator. Default is 100.
#' @param eta [\code{numeric(1)}] \cr
#'   In how many replications each algorithm must have non-dominated points
#'   to be selected in the first selection step? Must be in [0, 1] - 0.5
#'   requres non-dominated points more than in half of all replications. Default is 0.5
#' @param w [\code{numeric(2)}] \cr
#'   Weight vector for the augmented Tschebbyscheff Norm in the second selection
#'   stept. The first elements corresponds to the number of algorithms, the second
#'   to the optimality gap. If you want to have a low optimality gap, the second
#'   element of w should be set to an high value, if a low number of algorithms
#'   is more important the first element must be high. All positive real weights
#'   are allowed and will be normalized to sum(w) = 1. Default is c(0.05, 0.95).
#' @param cp [\code{numeric(1)}] \cr
#'   Complexity parameter of the decision tree used in step 3. Same parameter as
#'   in \link[rpart]{rpart}. Default is 0.1.
#' @param normalize [\code{logical(1)}] \cr
#'   Should the data be normalized to [0, 1]? Default is TRUE. 
#'   
#' @return 
#' Object of class \code{frontTestResult}. Named list with the elements:
#'
#' \itemize{
#'   \item{non.dominated.algos [\code{logical}]}{Named vector, each element
#'     corresponds to one algorithm. TRUE if algorithm is selected in step 1.}
#'   \item{algos.domination.count [\code{numeric}]}{Named vector, each element
#'     corresponds to one algorithm. Number of replications with non-dominated
#'     points for each algorithm.}
#'   \item{relevant.algos [\code{logical}]}{Named vector, each element
#'     corresponds to one algorithm selected in step 1. TRUE if the algorithm
#'     is selected in step 2.}
#'   \item{algos.selection.vals [\code{data.frame}]}{Data.frame with 4 cols and
#'     2^(number of algorithms after in step 2) - 1 rows. First cols  active in
#'     this subset. Last 2 rows are numeric. First is the number of algos in this
#'     subset, second one the optimality gap..}
#'   \item{best.algo.order [\code{Factor}]}{Vector gives the order of the algorithms
#'     on the common Pareto front, algorithms low values of the first var.col first.}
#'   \item{split.vals [\code{numeric}]}{Split values between the algorithms given
#'     in best.algo.order.}
#'   \item{args [\code{list}] List containing all input arguments}
#' }
#' @examples 
#' \dontrun{
#' # Load data - for the data with subsampling enabled use apprSubsampleSVMParetoFronts
#' data(apprSVMParetoFronts)
#' 
#' # Avaible datasets: codrna, mnist, protein, vehicle
#' data = subset(apprSVMParetoFronts, apprSVMParetoFronts$dataset == "mnist")
#' 
#' # Start the front analysis with the main procedure
#' res = selectPortfolio(
#'   data = data,
#'   var.cols = c("error", "execTime"),
#'   algo.col = "solver",
#'   repl.col = "repl",
#'   indicator = "hv",
#'   ref.point = c(1.1, 1.1),
#'   eta = 0.5,
#'   w = c(0.05, 0.95),
#'   cp = 0.01,
#'   normalize = TRUE
#' ) 
#' print(res)
#' plot(res, colors = c("violet", "turquoise", "green", "red", "black", "blue"))}
#' @export

selectPortfolio = function(data, var.cols, algo.col, repl.col,
  indicator = "hv", ref.point = c(1.1, 1.1), lambda = 100,
  eta = 0.5, w = c(0.05, 0.95), cp = 0.1, normalize = TRUE) {
  
  requirePackages(c("emoa"))  
  
  assertDataFrame(data, any.missing = FALSE)
  assertSubset(var.cols, colnames(data))
  assertChoice(algo.col, colnames(data))
  assertChoice(repl.col, colnames(data))
  assertChoice(indicator, c("hv", "r2", "epsilon"))
  assertNumeric(ref.point, lower = 0L, len = 2L)
  lambda = asCount(lambda)
  assertNumber(eta, lower = 0, upper = 1)
  assertNumeric(w, lower = 0, len = 2L)
  w = w / sum(w)
  assertNumber(cp, lower = 0L)
  assertFlag(normalize)
  
  if (algo.col == repl.col || algo.col %in% var.cols || repl.col %in% var.cols)
    stop("algo.col, var.col and repl.col must be distinct.")
  
  
  algos = factor(unique(data[, algo.col]))
  data.old = data
  
  # Normalize Data
  if (normalize)
    data[, var.cols] = normalize(data[, var.cols], method = "range", range = c(0, 1))
  
  # Select contribution function with respect to indicator character
  contrFun = switch(indicator,
    hv = function(points, o)
      hypervolume_indicator(t(points), t(o), ref = ref.point),
    epsilon = function(points, o)
      epsilon_indicator(t(points), t(o)),
    r2 = function(points, o)
      r2_indicator(t(points), t(o), lambda = lambda)
  )
  
  # First selection: Remove all algorithms that are dominated in at least 50% repls
  non.dom.algos = 
    relevantAlgosDominationSelection(data, var.cols, algo.col, repl.col, eta)
  
  # Now drop non-selected algos and set new factor levels
  algos = factor(algos[non.dom.algos$relevant.algos])
  data = subset(data, data[, algo.col] %in% algos)
  data[, algo.col] = factor(data[, algo.col], levels = algos)
  
  # Second Selection: Multicrit Selection
  selected.algos = 
    relevantAlgosMulticritSelection(data, var.cols, algo.col, repl.col, contrFun, w)
  
  # Now drop non selected algos and set new factor levels
  algos = factor(algos[selected.algos$relevant.algos])
  data = subset(data, data[, algo.col] %in% algos)
  data[, algo.col] = factor(data[, algo.col], levels = algos)
  
  # Third Step: Find the best order of algorithms
  if (length(algos) < 1L) {
    stop("Found no relevant algorithm. Should not happen.")
  }
  if (length(algos) == 1L) {
    perms = list(perm = algos, split.vals = NULL)
  }
  if (length(algos) > 1L) {
    perms =
      sortedParetoFrontClassification(data, var.cols, algo.col, repl.col, contrFun, cp)
  }

  # Build result object
  # First a list with all input args
  args = list(
    data = data.old,
    var.cols = var.cols,
    algo.col = algo.col,
    repl.col = repl.col,
    indicator = indicator,
    ref.point = ref.point,
    lambda = lambda,
    eta = eta,
    w = w,
    cp = cp,
    normalize = normalize
  )
  
  res = list(
    non.dominated.algos = non.dom.algos$relevant.algos,
    algos.domination.count = non.dom.algos$counts,
    relevant.algos = selected.algos$relevant.algos,
    algos.selection.vals = selected.algos$algo.contrs,
    best.algo.order = perms$perm,
    split.vals = perms$split.vals,
    args = args
  )
  res = addClasses(res, "frontTestResult")
  
  return(res)
}
