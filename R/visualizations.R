
############################## CONVERGENCE CURVES ##############################

#' Plot convergence curves
#' 
#' Creates a plot showing the convergence curve of each search that has been 
#' applied to the given \code{problem}, averaged over all search runs. If the 
#' \code{data} contains results for a single problem only, the argument 
#' \code{problem} can be omitted.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the problem for which the plot is made. Can be omitted
#'   if the \code{data} contains results for a single problem only.
#'   
#' @export
plotConvergence <- function(data, problem){
  UseMethod("plotConvergence")
}
#' @export
plotConvergence.james <- function(data, problem){
  # check input - fall back to only problem if applicable
  all.problems <- getProblems(data)
  if(missing(problem)){
    if(length(all.problems) > 1){
      stop("'data' contains more than one problem, please specify 'problem'")
    } else {
      # set name of only problem
      problem <- all.problems[1]
    }
  }
  # get names of applied searches
  searches <- getSearches(data, problem)
  # average runs for each search
  avg.curves <- list()
  for(search in searches){
    # extract search runs
    runs <- getSearchRuns(data, problem, search)
    # reduce runs to max value at same time
    runs <- lapply(runs, function(run){
      reduced.times <- unique(run$times)
      reduced.values <- sapply(reduced.times, function(t){
        max(run$values[run$times == t])
      })
      run$times <- reduced.times
      run$values <- reduced.values
      return(run)
    })
    # average runs
    n.runs <- length(runs)
    runs.cur.indices <- rep(1, n.runs)
    runs.last.indices <- sapply(runs, function(run){length(run$times)})
    runs.cur.values <- sapply(runs, function(run){run$values[1]})
    avg.times <- c()
    avg.values <- c()
    while(max(runs.last.indices - runs.cur.indices) >= 0){
      # calculate first point in time when a new value is reached in any run
      next.times <- sapply(1:n.runs, function(r){
        run <- runs[[r]]
        cur.index <- runs.cur.indices[r]
        return(run$times[cur.index])
      })
      min.t <- min(next.times, na.rm = TRUE)
      # gather indices of runs with an update
      updated.runs.indices <- sapply(1:n.runs, function(r){
        run <- runs[[r]]
        cur.index <- runs.cur.indices[r]
        t <- run$times[cur.index]
        if(!is.na(t) && t == min.t){
          return(r)
        } else {
          return(NA)
        }
      })
      updated.runs.indices <- updated.runs.indices[!is.na(updated.runs.indices)]
      # extract new values for updated runs
      updated.runs.values <- sapply(updated.runs.indices, function(r){
        run <- runs[[r]]
        cur.index <- runs.cur.indices[r]
        return(run$values[cur.index])
      })
      # update values
      runs.cur.values[updated.runs.indices] <- updated.runs.values
      # increase current indices of updated runs
      runs.cur.indices[updated.runs.indices] <- runs.cur.indices[updated.runs.indices] + 1
      # register averaged value and corresponding time
      avg.times <- c(avg.times, min.t)
      avg.values <- c(avg.values, mean(runs.cur.values))
    }
    
    # TODO: plot ...
    print(search)
    print(avg.times)
    print(avg.values)
    
  }
}











