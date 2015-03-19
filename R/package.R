#' `JAMES' Analysis
#' 
#' This package can be used to further analyze and visualize results produced by
#' the analysis tools from the extensions module of `JAMES', a local search metaheuristic
#' framework implemented in Java (see references).
#' Functions are provided to plot convergence curves, draw box plots of solution
#' quality or convergence times and to summarize, manipulate or extract data
#' from the results.
#' 
#' \tabular{ll}{
#' Package: \tab james.analysis\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0.0\cr
#' Date: \tab 2015-03-19\cr
#' License: \tab MIT\cr
#' }
#' This version is written for use in combination with version 0.2.1 of the `JAMES'
#' extensions module.
#' 
#' @examples
#' # load example data
#' data(james)
#' summary(james)
#' 
#' # plot convergence curves for coconut data set
#' plotConvergence(james, problem = "coconut", min.time = 1000, max.time = 100000)
#' 
#' # create box plots of solution values (quality) and convergence times
#' boxplot(james, problem = "coconut")
#' boxplot(james, problem = "coconut", type = "time")
#' 
#' # extract solution values and convergence times for parallel tempering and random descent
#' values.pt <- getBestSolutionValues(james, problem = "coconut", search = "Parallel Tempering")
#' times.pt <- getConvergenceTimes(james, problem = "coconut", search = "Parallel Tempering")
#' values.rd <- getBestSolutionValues(james, problem = "coconut", search = "Random Descent")
#' times.rd <- getConvergenceTimes(james, problem = "coconut", search = "Random Descent")
#' 
#' # perform wilcoxon test to compare distributions across algorithms
#' values.test <- wilcox.test(values.pt, values.rd)
#' values.test
#' times.test <- wilcox.test(times.pt, times.rd)
#' times.test
#' 
#' # adjust p-values for multiple testing
#' p.adjust(c(values.test$p.value, times.test$p.value))
#' 
#' @section Example data: \code{\link{james}}
#' 
#' @section Data manipulation functions:
#'   \itemize{
#'    \item{\code{\link{readJAMES}}}
#'    \item{\code{\link{reduceJAMES}}}
#'    \item{\code{\link{mergeJAMES}}}
#'    \item{\code{\link{getProblems}}}
#'    \item{\code{\link{getSearches}}}
#'    \item{\code{\link{getSearchRuns}}}
#'    \item{\code{\link{getNumSearchRuns}}}
#'    \item{\code{\link{getBestSolutionValues}}}
#'    \item{\code{\link{getBestSolutions}}}
#'    \item{\code{\link{getConvergenceTimes}}}
#'   }
#'   
#' @section Plot functions:
#'   \itemize{
#'    \item{\code{\link{plotConvergence}}}
#'    \item{\code{\link{boxplot.james}}}
#'   }
#' 
#' @author Herman De Beukelaer <\email{Herman.DeBeukelaer@@UGent.be}>
#' @references `JAMES' Website: \url{http://www.jamesframework.org}
#'       
#' @docType package
#' @keywords package
#' @name james.analysis
NULL