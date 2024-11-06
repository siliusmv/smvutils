
#' Turn an R expression completely quiet
#'
#' This function can be used for silencing the output of functions
#' that contain print statements inside, such as most of the
#' functions of the ncdf4 package.
#' @export
smv_quiet = function(expr) {
  # Create a tempfile, and sink all output from the R session to that file
  tmpfile = tempfile()
  sink(tmpfile)
  # When this function has finished, stop sinking all output from the R session to the tempfile
  on.exit(sink())
  # When this function has finished, also delete the tempfile that we sent all our output to
  on.exit(unlink(tmpfile), add = TRUE)
  # Actually execute the provided R-expression
  invisible(force(expr))
}

