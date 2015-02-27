
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
#' json.file <- system.file("extdata", "AlgoComparison.json", package = "james.analysis")
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

############################### MERGE - REDUCE ############################## 

#' Merge analysis results
#'
#' Merge results from different analyses. If runs of the same search
#' applied to the same problem are found in both data sets, these runs
#' are merged into a single list. This is a generic S3 method.
#' 
#' @param data1 results from the first analysis
#' @param data2 results from the second analysis
#' 
#' @return merged data
#'
#' @export
mergeJAMES <- function(data1, data2){
  UseMethod("mergeJAMES")
}
#' @export
mergeJAMES.james <- function(data1, data2){
  # check input
  if(!inherits(data2, "james")){
    stop("'data2' should also be of class \"james\"")
  }
  # copy first data set
  merged <- data1
  # merge results from second data set into first
  for(problem in getProblems(data2)){
    for(search in getSearches(data2, problem)){
      # retrieve existing search runs, empty list if none
      existing.runs <- tryCatch(
                          getSearchRuns(merged, problem, search),
                          error = function(e){
                            list()
                          }
                       )
      # retrieve new search runs
      new.runs <- getSearchRuns(data2, problem, search)
      # merge runs
      merged.runs <- c(existing.runs, new.runs)
      # update merged data
      if(length(getProblems(merged, filter = glob2rx(problem))) == 0){
        # first search for this problem: add problem
        merged[[problem]] <- list()
      }
      merged[[problem]][[search]] <- merged.runs
    }
  }
  return(merged)
}

#' Reduce analysis results to selected problems and searches
#' 
#' Reduce the given \code{data} by filtering the analyzed problems and applied 
#' searches based on the given list of names or \link{regular expression} 
#' (pattern matching is done with \code{\link{grep}}). This is a generic S3
#' method.
#' 
#' @param data data object containing the analysis results
#' @param problems \link{regular expression} or list of strings. Only those 
#'   problems that match the regular expression or occur in the list are 
#'   retained.
#' @param searches \link{regular expression} or list of strings. Only those 
#'   searches that match the regular expression or occur in the list are 
#'   retained.
#'   
#' @return Reduced data set containing only those problems and searches whose 
#'   names match the respective regular expression or occur in the respective 
#'   list of strings.
#'   
#' @export
reduceJAMES <- function(data, problems = ".*", searches = ".*"){
  UseMethod("reduceJAMES")
}
#' @export
reduceJAMES.james <- function(data, problems = ".*", searches = ".*"){
  # determine which problems to drop
  problem.names <- getProblems(data)
  if(is.list(problems)){
    drop.problems <- setdiff(problem.names, problems)
  } else {
    drop.problems <- grep(pattern = problems, problem.names, value = TRUE, invert = TRUE)
  }
  # drop those problems
  data[drop.problems] <- NULL
  # iterate over retained problems to filter searches
  for(problem in getProblems(data)){
    # determine which searches to drop
    search.names <- getSearches(data, problem)
    if(is.list(searches)){
      drop.searches <- setdiff(search.names, searches)
    } else {
      drop.searches <- grep(pattern = searches, search.names, value = TRUE, invert = TRUE)
    }
    # drop those searches
    data[[problem]][drop.searches] <- NULL
    # drop problem if all searches have been dropped
    if(length(data[[problem]]) == 0){
      data[[problem]] <- NULL
    }
  }
  # check if data has been retained
  if(length(data) == 0){
    stop("no data is retained")
  }
  return(data)
}

################################## GETTERS ################################## 

#' Get names of analyzed problems
#' 
#' Extracts the names of all problems for which analysis results are contained 
#' in the given data. This is a generic S3 method.
#' 
#' Problem names are sorted using \link[naturalsort]{naturalsort}. If a 
#' \code{filter} is set, only those problem names matching the given 
#' \link{regular expression} are returned (pattern matching is done with 
#' \code{\link{grep}}).
#' 
#' @param data data object containing the analysis results
#' @param filter \link{regular expression} (optional). Only problem names that 
#'   match the given regex are returned, if any.
#'   
#' @return Sorted vector of strings containing the names of all analyzed
#'   problems that occur in the given data and match the applied filter (if
#'   any).
#'   
#' @importFrom naturalsort naturalsort
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
  # sort
  problem.names <- naturalsort::naturalsort(problem.names)
  return(problem.names)
}

#' Get names of applied searches
#' 
#' Extracts the names of all searches that have been applied to the given 
#' \code{problem}.This is a generic S3 method.
#' 
#' Search names are sorted using \link[naturalsort]{naturalsort}. If the
#' \code{data} contains results for a single problem only, the argument 
#' \code{problem} can be omitted. If a \code{filter} is set, only those search 
#' names matching the given \link{regular expression} are returned (pattern 
#' matching is done with \code{\link{grep}}).
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem. Can be omitted if the 
#'   \code{data} contains results for a single problem only.
#' @param filter \link{regular expression} (optional). Only search names that 
#'   match the given regex are returned, if any.
#'   
#' @return Sorted vector of strings containing the names of all searches that
#'   have been applied to the given problem and match the applied filter (if
#'   any).
#'   
#' @importFrom naturalsort naturalsort
#' @export
getSearches <- function(data, problem, filter){
  UseMethod("getSearches")
}
#' @export
getSearches.james <- function(data, problem, filter){
  # fall back to single problem if no problem specified
  if(missing(problem)){
    problem <- getSingleProblem(data)
  }
  # extract problem data
  problem.data <- data[[problem]]
  # check if data is available
  if(is.null(problem.data)){
    msg <- sprintf("'data' does not contain results for problem \"%s\"", problem)
    stop(msg)
  }
  # get all applied searches
  search.names <- names(problem.data)
  # filter, if set
  if(!missing(filter)){
    search.names <- grep(pattern = filter, search.names, value = TRUE);
  }
  # sort
  search.names <- naturalsort::naturalsort(search.names)
  return(search.names)
}

#' Get search run data
#' 
#' Extract the data corresponding to the subsequent runs of a specific 
#' \code{search} being applied to a specific \code{problem}. This is a generic 
#' S3 method.
#' 
#' If the \code{data} contains results for a single problem only, the argument 
#' \code{problem} can be omitted. Likewise, if -- for the considered
#' \code{problem} -- results are available for a single search only, the
#' argument \code{search} can be omitted.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem. Can be omitted if the 
#'   \code{data} contains results for a single problem only.
#' @param search name of the applied search. Can be omitted if the \code{data} 
#'   contains results for a single search only (for the considered 
#'   \code{problem}).
#'   
#' @return A list containing one element for each search run.
#'   
#'   Each run has at least two elements \code{time} and \code{values}, which are
#'   both numeric vectors. The \code{time} vector indicates when the best 
#'   solution was updated during search and the new best solution's value is 
#'   found at the respective index in \code{values}. Times are expressed in 
#'   milliseconds since starting the search. A time of -1 indicates that the 
#'   search was not yet running, which e.g. occurs when a local search adopts a 
#'   random current solution during initialization. Times are always positive 
#'   (or -1) and increasing.
#'   
#'   If contained in the given \code{data}, a run also has an element 
#'   \code{best.solution} representing the final best solution found during that
#'   search run. The last element of \code{values} then indicates the value of 
#'   this best solution. When writing results obtained from the analysis tools 
#'   in the JAMES extensions module to a JSON file, one should provide a JSON 
#'   converter for the solution type of the analyzed problems if it is desired 
#'   that the actual best found solutions are contained in the output file.
#'   
#' @export
getSearchRuns <- function(data, problem, search){
  UseMethod("getSearchRuns")
}
#' @export
getSearchRuns.james <- function(data, problem, search){
  # fall back to single problem if not specified
  if(missing(problem)){
    problem <- getSingleProblem(data)   
  }
  # fall back to single search for considered problem, if not specified
  if(missing(search)){
    search <- getSingleSearch(data, problem)
  }
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

#' Get number of applied search runs
#' 
#' Get the number of applied runs of the given \code{search} when solving the 
#' given \code{problem}. This is a generic S3 method.
#' 
#' If the \code{data} contains results for a single problem only, the argument 
#' \code{problem} can be omitted. Likewise, if -- for the considered 
#' \code{problem} -- results are available for a single search only, the 
#' argument \code{search} can be omitted.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem. Can be omitted if the 
#'   \code{data} contains results for a single problem only.
#' @param search name of the applied search. Can be omitted if the \code{data} 
#'   contains results for a single search only (for the considered 
#'   \code{problem}).
#'   
#' @return numeric: number of applied search runs
#'   
#' @export
getNumSearchRuns <- function(data, problem, search){
  UseMethod("getNumSearchRuns")
}
#' @export
getNumSearchRuns.james <- function(data, problem, search){
  # extract search runs
  runs <- getSearchRuns(data, problem, search)
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
#' If the \code{data} contains results for a single problem only, the argument 
#' \code{problem} can be omitted. Likewise, if -- for the considered
#' \code{problem} -- results are available for a single search only, the
#' argument \code{search} can be omitted.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem. Can be omitted if the 
#'   \code{data} contains results for a single problem only.
#' @param search name of the applied search. Can be omitted if the \code{data} 
#'   contains results for a single search only (for the considered 
#'   \code{problem}).
#'   
#' @return Numeric vector containing the values of the best found solutions
#'   during each run.
#'   
#' @export
getBestSolutionValues <- function(data, problem, search){
  UseMethod("getBestSolutionValues")
}
#' @export
getBestSolutionValues.james <- function(data, problem, search){
  # extract search runs
  runs <- getSearchRuns(data, problem, search)
  # get best solution values
  best.solution.values <- sapply(runs, function(run){ tail(run$values, n=1) })
  return(best.solution.values)
}

#' Get best found solutions
#' 
#' Get the best found solutions during the different runs of the given 
#' \code{search} applied to the given \code{problem}. This is a generic S3 
#' method.
#' 
#' If the \code{data} contains results for a single problem only, the argument 
#' \code{problem} can be omitted. Likewise, if -- for the considered
#' \code{problem} -- results are available for a single search only, the
#' argument \code{search} can be omitted.
#' 
#' When writing results obtained from the analysis tools in the JAMES extensions
#' module to a JSON file, one should provide a JSON converter for the solution 
#' type of the analyzed problems if it is desired that the actual best found 
#' solutions are contained in the output file. Therefore, these solutions might 
#' not be available for all problems, searches or search runs. In case a best 
#' solution is missing for a search run, the corresponding entry in the returned 
#' list will be set to \code{NA}. It is possible that a list of only \code{NA}
#' is returned.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the analyzed problem. Can be omitted if the 
#'   \code{data} contains results for a single problem only.
#' @param search name of the applied search. Can be omitted if the \code{data} 
#'   contains results for a single search only (for the considered 
#'   \code{problem}).
#'   
#' @return List containing the best found solutions during each run. May contain
#'   \code{NA} values.
#'   
#' @export
getBestSolutions <- function(data, problem, search){
  UseMethod("getBestSolutions")
}
#' @export
getBestSolutions.james <- function(data, problem, search){
  # extract search runs
  runs <- getSearchRuns(data, problem, search)
  # get best solutions
  best.solutions <- lapply(runs, function(run){ run$best.solution })
  # replace NULL with NA
  best.solutions[sapply(best.solutions, is.null)] <- NA
  return(best.solutions)
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
      nruns <- getNumSearchRuns(results, problem, search)
      values <- getBestSolutionValues(results, problem, search)
      mean.value <- mean(values)
      sd.value <- sd(values)
      printf(format, problem, search, nruns, mean.value, sd.value)
    }
  }
  
}







