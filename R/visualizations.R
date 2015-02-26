
############################## CONVERGENCE CURVES ##############################

#' Plot convergence curves
#' 
#' Creates a plot showing the convergence curve of each search that has been 
#' applied to the given \code{problem}, averaged over all search runs. This is a
#' generic S3 method.
#' 
#' If the \code{data} contains results for a single problem only, the argument 
#' \code{problem} can be omitted. If desired to plot convergence curves for a 
#' selection of the applied searches, use \link{reduceJAMES} to extract the 
#' respective data.
#' 
#' The curves are plotted using \link{matplot}. More information about the 
#' graphical parameters are provided in the documentation of this function.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the problem for which the plot is made. Can be omitted
#'   if the \code{data} contains results for a single problem only.
#' @param col color(s) of the plotted lines and/or symbols, used cyclically when
#'   providing a vector. Defaults to black.
#' @param type plot type, defaults to lines.
#' @param lty line type(s), used cyclically when providing a vector. If set to 
#'   \code{NULL}, line types default to 1:n where n is the number of plotted 
#'   curves.
#' @param title plot title.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param legend logical: indicates whether a legend should be added to the 
#'   plot. Defaults to TRUE.
#' @param legend.pos position of the legend, specified as a keyword  keyword 
#'   from the list \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, 
#'   \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, 
#'   \code{"right"} and \code{"center"}. Defaults to \code{"bottomright"} as 
#'   this area of the plot usually has a lot of available space.
#' @param legend.inset inset distance(s) from the margins as a fraction of the 
#'   plot region. If a single value is given, it is used for both margins; if 
#'   two values are given, the first is used for \code{x-}distance, the second 
#'   for \code{y-}distance.
#' @param legend.names names to be shown in the legend. If set to \code{NULL}, 
#'   the names default to the search names obtained from calling
#'   \code{\link{getSearches}} on the given \code{data} and \code{problem}.
#' @param ... optional other arguments passed to \code{\link{matplot}}.
#'   
#' @export
plotConvergence <- function(data, problem, col = "black", type = "l", lty = NULL,
                            title = "Convergence curve(s)", xlab = "Runtime (ms)",
                            ylab = "Value", legend = TRUE, legend.pos = "bottomright",
                            legend.inset = c(0.02, 0.05), legend.names = NULL,
                            ...){
  UseMethod("plotConvergence")
}
#' @export
plotConvergence.james <- function(data, problem, col = "black", type = "l", lty = NULL,
                                  title = "Convergence curve(s)", xlab = "Runtime (ms)",
                                  ylab = "Value", legend = TRUE, legend.pos = "bottomright",
                                  legend.inset = c(0.02, 0.05), legend.names = NULL,
                                  ...){
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
        return(ifelse(!is.na(t) && t == min.t, r, NA))
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
    
    # register averaged curve
    avg.curve <- list(times = avg.times, values = avg.values)
    avg.curves <- append(avg.curves, list(avg.curve))
    
  }
  
  # extract number of curves and maximum number of data points across all curves
  num.curves <- length(avg.curves)
  max.num.points <- max(sapply(avg.curves, function(curve){length(curve$times)}))
  # initialize matrix of time vectors (columns)
  times.matrix <- matrix(NA, nrow = max.num.points, ncol = num.curves)
  # initialize matrix of value vectors (columns)
  values.matrix <- matrix(NA, nrow = max.num.points, ncol = num.curves)
  
  # fill both matrices
  for(i in 1:num.curves){
    curve <- avg.curves[[i]]
    num.points <- length(curve$times)
    times.matrix[1:num.points, i] <- curve$times
    values.matrix[1:num.points, i] <- curve$values
  }
  
  # set default line types
  if(is.null(lty)){
    lty <- 1:num.curves
  }
  # plot curves
  matplot(x = times.matrix, y = values.matrix,
          col = col, type = type, lty = lty,
          main = title, xlab = xlab, ylab = ylab,
          ...)
  if(legend){
    # set default legend names
    if(is.null(legend.names)){
      legend.names = searches
    }
    # add legend
    legend(x = legend.pos, legend = legend.names,
           inset = legend.inset, col = col, lty = lty)
  }
  
}










