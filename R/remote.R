#' Execute code on a remote server
#'
#' This function takes in an R-expression, a server name and a username for that server.
#' Then, it executes the R-expression on the remote server, using the future package.
#' Users are advised to setup passwordless login to the servers they are executing
#' their code on, to not have to retype their password every single time this function
#' is used.
#' @export
smv_remote = function(expr, user, server, substitute = TRUE, ...) {
  # The substitute argument defaults to TRUE, which means that expr is R code that can be run
  # and that must be substituted to not be evaluated early. If, however, expr has already
  # been substituted before it was given as input to this function, then substitute must
  # be set to FALSE, so we avoid double substitution
  if (substitute) expr = substitute(expr)
  # Use the future package to connect to the server
  old_plan = future::plan(
    future::cluster,
    workers = paste0(user, "@", server)
  )
  # Ensure that we deconnect from the server when this function exits
  on.exit(future::plan(old_plan))
  # Try to execute the expression on the remote server
  tryCatch({
    res = future::future(
      expr = expr,
      seed = TRUE,
      substitute = FALSE,
      ...
    )
    future::value(res)
  },
  # If the code is interrupted, then kill the connection to the remote server before returning
  interrupt = function(e) {
    future::plan(old_plan)
    stop("Remote execution was interrupted")
  })
}

#' Get a function for executing code on remote servers
#'
#' This function takes in a remote server address and username, and
#' then it outputs a function that takes in an R-expression, executes
#' that expression on the remote server, and returns the output.
#' This is basicly a small wrapper around the remote_run function
#' @export
smv_get_remote_func = function(server = NULL, user = NULL, ...) {
  if (is.null(server) && is.null(user)) {
    func = force
  } else {
    func = function(expr) {
      expr = substitute(expr)
      envir = parent.frame()
      smv_remote(
        expr = expr,
        user = user,
        envir = envir,
        server = server,
        substitute = FALSE,
        ...)
    }
  }
  func
}
