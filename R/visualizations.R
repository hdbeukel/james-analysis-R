
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
#' graphical parameters are provided in the documentation of this function. By 
#' default, a legend is added to the plot. This can be omitted by setting 
#' \code{legend = FALSE}. If desired, a custom legend may then be added. It is
#' possible to zoom in on a specific region of the plot using the parameters
#' \code{min.time} and \code{max.time}.
#' 
#' @param data data object containing the analysis results
#' @param problem name of the problem for which the plot is made. Can be omitted
#'   if the \code{data} contains results for a single problem only.
#' @param col color(s) of the plotted lines and/or symbols, used cyclically when
#'   providing a vector. Defaults to black.
#' @param type plot type, defaults to staircase.
#' @param lty line type(s), used cyclically when providing a vector. If set to 
#'   \code{NULL}, line types default to 1:n where n is the number of plotted 
#'   curves.
#' @param title plot title.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param min.time zoom in on the part of the curve(s) above this time on the 
#'   \code{x-}axis. Defaults to NA in which case no minimum time is set.
#' @param max.time zoom in on the part of the curve(s) below this time on the 
#'   \code{x-}axis. Defaults to NA in which case no maximum time is set.
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
plotConvergence <- function(data, problem, col = "black", type = "s", lty = NULL,
                            title = "Convergence curve(s)", xlab = "Runtime (ms)",
                            ylab = "Value", min.time = NA, max.time = NA,
                            legend = TRUE, legend.pos = "bottomright",
                            legend.inset = c(0.02, 0.05), legend.names = NULL,
                            ...){
  UseMethod("plotConvergence")
}
#' @export
plotConvergence.james <- function(data, problem, col = "black", type = "s", lty = NULL,
                                  title = "Convergence curve(s)", xlab = "Runtime (ms)",
                                  ylab = "Value", min.time = NA, max.time = NA,
                                  legend = TRUE, legend.pos = "bottomright",
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
  num.searches <- length(searches)
  # extract min and max time across all runs of all searches
  observed.time.bounds.per.search <- lapply(searches, function(search){
    runs <- getSearchRuns(data, problem, search)
    obs.min <- min(sapply(runs, function(run){min(run$times)}))
    obs.max <- max(sapply(runs, function(run){max(run$times)}))
    return(c(obs.min, obs.max))
  })
  observed.time.bounds <- c(
    min(sapply(observed.time.bounds.per.search, function(s){s[1]})),
    max(sapply(observed.time.bounds.per.search, function(s){s[2]}))
  )
  
  # initialize time points and value matrix
  plotted.time.bounds <- observed.time.bounds
  if(!is.na(min.time)){
    plotted.time.bounds[1] <- max(plotted.time.bounds[1], min.time)
  }
  if(!is.na(max.time)){
    plotted.time.bounds[2] <- min(plotted.time.bounds[2], max.time)
  }
  time.points <- seq(plotted.time.bounds[1], plotted.time.bounds[2])
  num.points <- length(time.points)
  values.matrix <- matrix(NA, nrow = num.points, ncol = num.searches)
  
  # fill value matrix
  for(s in 1:num.searches){
    # extract search runs
    search <- searches[s]
    runs <- getSearchRuns(data, problem, search)
    # map time points on average value across runs
    avg.values <- sapply(time.points, function(t){
      # get last observed value per run at or before time point t
      run.values <- sapply(runs, function(run){
        obs.values <- run$values[run$time <= t]
        last.obs.value <- ifelse(length(obs.values) > 0, tail(obs.values, 1), NA)
        return(last.obs.value)
      })
      # average run values
      run.values <- run.values[!is.na(run.values)]
      avg.value <- ifelse(length(run.values) > 0, mean(run.values), NA)
      return(avg.value)
    })
    # store in matrix
    values.matrix[ , s] <- avg.values
  }

  # set default line types
  if(is.null(lty)){
    lty <- 1:num.searches
  }
  # plot curves
  matplot(x = time.points, y = values.matrix,
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










