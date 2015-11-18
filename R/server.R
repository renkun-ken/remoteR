#' @export
sessionServer <- function(host = "localhost", port = 6001) {

  # server commands
  server_eval <- function(expr) {
    withVisible(eval(expr, globalenv()))
  }
  server_send <- function(expr) {
    eval(expr, globalenv())
    NULL
  }
  server_assign <- function(symbol, value) {
    assign(symbol, value, globalenv())
    NULL
  }
  server_kill <- function(...) {
    closeAllConnections()
    q(...)
  }

  cat("R Server Terminal\n")
  while (TRUE) {
    con <- try(socketConnection(host = host, port = port,
      blocking = TRUE, server = TRUE, open = "r+", timeout = 86400L), silent = TRUE)
    if (inherits(con, "try-error")) {
      message(con, "\n")
    } else {
      tryCatch({
        request <- unserialize(con)
        cat(sprintf("[%s] %s > %s\n",
          format(request$time, "%Y-%m-%d %H:%M:%S"), request$client, request$command))
        f <- get(sprintf("server_%s", request$command), environment(),
          mode = "function", inherits = FALSE)
        res <- try(do.call(f,
          request$args, quote = TRUE, envir = globalenv()), silent = TRUE)
        serialize(res, con)
      }, error = function(e) message(e, "\n"),
        warning = function(w) message(w, "\n"),
        finally = close(con))
    }
  }
}
