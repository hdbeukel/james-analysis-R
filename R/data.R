#' Read analysis results from JSON file
#'
#' Read results from a JSON file produced by the analysis tools in the JAMES extensions module.
#'
#' @param file character vector: relative path to a JSON file containing results produced by
#'             the analysis tools in the JAMES extensions module
#' @return S3 object of type "analysis.results" containing the results of running a number of
#'         searches on a set of problems, performing a number of repeated runs per search
#'
#' @examples
#' # get path to raw JSON file included in package distribution
#' json.file <- system.file("extdata", "results.json", package = "james.analysis")
#' 
#' # read results from file
#' results <- read.analysis.results(json.file)
#'
#' @export
readAnalysisResults <- function(file) {
  
  # check input
  if (is.null(file) || is.na(file) || !is.character(file)){
    stop("'file' should be a character vector")
  }
  # read JSON file
  results <- rjson::fromJSON(file = file)
  # set class name
  class(results) <- "analysis.results"
  
  return(results)
}

#' Extract best solution evaluations
#' 
#' TODO
#' 
#' @export
extractBestSolutionEvaluations <- function(results, problem, search, ...){
  UseMethod("extractBestSolutionEvaluations")
}

#' TODO
#' 
#' TODO
#'
#'  @export
extractBestSolutionEvaluations.analysis.results <- function(results, problem, search, ...){
  cat("TODO")
}

#' @export
summary.analysis.results <- function(object, ...){
  
  results <- object
  
  # determine column widths
  col.widths <- c(9,7,5)
  problem.names <- names(results)
  search.names <- unique(sapply(results, names))
  col.widths[1] <- max(col.widths[1], max(sapply(problem.names, nchar)) + 1)
  col.widths[2] <- max(col.widths[2], max(sapply(search.names, nchar)) + 1)
  format <- sprintf("%%-%ds %%-%ds %%%ds \n", col.widths[1], col.widths[2], col.widths[3])
  
  # print header
  printf(format, "Problem:", "Search:", "Runs:")
  printf(format, "--------", "-------", "-----")
  
  # iterate over problems
  nprob <- length(results)
  prob.names <- names(results)
  for (p in 1:nprob){
    nsearch <- length(results[[p]])
    search.names <- names(results[[p]])
    for (s in 1:nsearch){
      nruns <- length(results[[p]][[s]])
      printf(format, prob.names[p], search.names[s], nruns)
    }
  }
  
}







