#' Open PDF file
#' 
#' Open a PDF file in the standard PDF viewer.
#'
#' Opens the given PDF \code{file} in the default PDF viewer. Under Windows,
#' this is done by calling \code{shell.exec} and whatever program associated
#' with the PDF file extension will be used. On Unix (including Mac OS X) the
#' function will use the program named in the option "pdfviewer" (see
#' \code{help(options)} for information on how this option is set).
#' 
#' The \code{bg} argument is ignored on Windows.
#' 
#' @param file relative path to the PDF file to be opened, of type "character"
#' @param bg Indicates whether the PDF viewer should be opened in the background (Unix only)
#' @export
openPDF = function(file, bg = TRUE) 
{
  # quote file name for in case it contains spaces
  file = paste('"', file, '"', sep="")
  # implementation depends on operating system
  OST = .Platform$OS.type
  if (OST == "windows") 
    shell.exec(file)
  else if (OST == "unix") {
    # check whether pdfviewer is set
    pdf = getOption("pdfviewer")
    msg = NULL
    if (is.null(pdf)) 
      msg = 'getOption("pdfviewer") is NULL'
    else if (is.na(pdf))
      msg = 'getOption("pdfviewer") is NA'
    else if (!is.character(pdf))
      msg = 'getOption("pdfviewer") should be of type "character"'
    else if (nchar(pdf) == 0) 
      msg = 'getOption("pdfviewer") is ""'
    if (!is.null(msg)) 
      stop(msg, "; please set pdf viewer using 'options(pdfviewer = ...)'")
    # create command to open pdf
    cmd <- paste(pdf, file)
    if (bg) 
      cmd <- paste(cmd, "&")
    # run command
    system(cmd)
  } else {
    stop("Unknown operating system (not Windows nor Unix).")
  }
}