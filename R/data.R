
#################################### I/O ####################################

#' Read analysis results from JSON file
#' 
#' Read results from a JSON file produced by the analysis tools from the JAMES
#' extensions module.
#' 
#' @param file character vector: relative path to a JSON file containing results
#'   produced by the analysis tools from the JAMES extensions module
#' @return S3 object of class "james" containing the results of running a number
#'   of searches on a set of problems, performing a number of repeated runs per
#'   search
#'   
#' @examples
#' # get path to raw JSON file included in package distribution
#' json.file <- system.file("extdata", "data.json", package = "james.analysis")
#' 
#' # read results from file
#' data <- readJAMES(json.file)
#' 
#' @export
readJAMES <- function(file) {
  
  # check input
  if (is.null(file) || is.na(file) || !is.character(file)){
    stop("'file' should be a character vector")
  }
  # read JSON file
  results <- rjson::fromJSON(file = file)
  # set class name
  class(results) <- "james"
  
  return(results)
}

################################## GETTERS ################################## 

#' Get names of analyzed problems
#' 
#' Extracts the names of all problems for which analysis results are contained
#' in the given data. If a \code{filter} is set, only those problem names
#' matching the given \link{regular expression} are returned (pattern matching
#' is done with \code{\link{grep}}). This is a generic S3 method.
#' 
#' @param data data object containing the analysis results
#' @param filter \link{regular expression} (optional). Only problem names that
#'   match the given regex are returned, if any.
#'   
#' @return Vector of strings containing the names of all analyzed problems that
#'   occur in the given data and match the applied filter (if any).
#'   
#' @export
getProblems <- function(data, filter){
  UseMethod("getProblems")
}
#' @export
getProblems.james <- function(data, filter){
  problem.names <- names(data)
  # filter, if set
  if(!missing(filter)){
    problem.names <- grep(pattern = filter, problem.names, value = TRUE)
  }
  return(problem.names)
}

#' Get names of applied searches
#' 
#' Extracts the names of all searches that have been applied to the given
#' \code{problem}. If a \code{filter} is set, only those search names matching
#' the given \link{regular expression} are returned (pattern matching is done
#' with \code{\link{grep}}). This is a generic S3 method.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem
#' @param filter \link{regular expression} (optional). Only search names that
#'   match the given regex are returned, if any.
#'   
#' @return Vector of strings containing the names of all searches that have been
#'   applied to the given problem and match the applied filter (if any).
#'   
#' @export
getSearches <- function(data, problem, filter){
  UseMethod("getSearches")
}
#' @export
getSearches.james <- function(data, problem, filter){
  # extract problem data
  problem.data <- extractProblem(data, problem)
  # get all applied searches
  search.names <- names(problem.data)
  # filter, if set
  if(!missing(filter)){
    search.names <- grep(pattern = filter, search.names, value = TRUE);
  }
  return(search.names)
}

#' Get number of applied search runs
#' 
#' Get the number of applied runs of the given \code{search} when solving the 
#' given \code{problem}. This is a generic S3 method.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem
#' @param search name of the applied search
#'   
#' @return numeric: number of applied search runs
#'   
#' @export
getNumRuns <- function(data, problem, search){
  UseMethod("getNumRuns")
}
#' @export
getNumRuns.james <- function(data, problem, search){
  # extract search runs
  runs <- extractRuns(data, problem, search)
  # count runs
  num.runs <- length(runs)
  return(num.runs)
}

#' Get values of best found solutions
#' 
#' Get the values of the best found solutions during all runs of the given 
#' \code{search} applied to the given \code{problem}. This is a generic S3 
#' method.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem
#' @param search name of the applied search
#'   
#' @return Numeric vector containing the values of the best found solutions
#'   during each run
#'   
#' @export
getBestSolutionValues <- function(data, problem, search){
  UseMethod("getBestSolutionValues")
}
#' @export
getBestSolutionValues.james <- function(data, problem, search){
  # extract search runs
  runs <- extractRuns(data, problem, search)
  # get best solution values
  best.solution.values <- sapply(runs, function(run){ tail(run$values, n=1) })
  return(best.solution.values)
}

################################## INTERNAL #################################

# extract problem
extractProblem <- function(data, problem){
  UseMethod("extractProblem")
}
extractProblem.james <- function(data, problem){
  # extract problem
  problem.data <- data[[problem]]
  # check if data is available
  if(is.null(problem.data)){
    msg <- sprintf("'data' does not contain results for problem \"%s\"", problem)
    stop(msg)
  }
  return(problem.data)
}

# extract search runs
extractRuns <- function(data, problem, search){
  UseMethod("extractRuns")
}
extractRuns.james <- function(data, problem, search){
  # extract search runs
  runs <- data[[problem]][[search]]
  # check if data is available
  if(is.null(runs)){
    msg <- sprintf("'data' does not contain results for search \"%s\" being applied to problem \"%s\"",
                   search,
                   problem)
    stop(msg)
  }
  return(runs)
}

################################## SUMMARY ################################## 

#' @export
summary.james <- function(object, ...){
  
  results <- object
  
  # determine column widths
  col.widths <- c(8, 7, 5, 11, 8)
  problem.names <- getProblems(results)
  search.names <- unique(unlist(sapply(problem.names, function(p){getSearches(results, p)})))
  col.widths[1] <- max(col.widths[1], max(sapply(problem.names, nchar)))
  col.widths[2] <- max(col.widths[2], max(sapply(search.names, nchar)))
  format <- sprintf("%%-%ds  %%-%ds  %%%ds  %%%ds  %%%ds \n",
                    col.widths[1], col.widths[2], col.widths[3],
                    col.widths[4], col.widths[5])
  
  # construct header lines
  lines <- lapply(col.widths, function(w){ paste(rep("-", w), collapse = "") })
  
  # print header
  printf(format, "Problem:", "Search:", "Runs:", "Mean value:", "St. dev:")
  printf(format, lines[[1]], lines[[2]], lines[[3]], lines[[4]], lines[[5]])
  
  # update format for real value printing (last two columns)
  format <- sprintf("%%-%ds  %%-%ds  %%%ds  %%11.3g  %%8.3g \n",
                    col.widths[1], col.widths[2], col.widths[3])
  
  # print info of applied searches per problem
  for (problem in problem.names){
    search.names <- getSearches(results, problem)
    for (search in search.names){
      nruns <- getNumRuns(results, problem, search)
      values <- getBestSolutionValues(results, problem, search)
      mean.value <- mean(values)
      sd.value <- sd(values)
      printf(format, problem, search, nruns, mean.value, sd.value)
    }
  }
  
}







