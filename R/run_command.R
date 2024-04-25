
#' Try to run command, and if it fails, then wait sleep_time seconds and try again
#' @export
smv_run = function(command,
                   args = character(),
                   n_attempts = 1,
                   sleep_time = 5,
                   ...) {
  attempts = 0
  while (attempts < n_attempts) {
    err_code = system2(command = command, args = args, ...)
    if (err_code == 0) break
    attempts = attempts + 1
    if (attempts < n_attempts) Sys.sleep(sleep_time)
  }
  success = attempts < n_attempts
  invisible(success)
}
