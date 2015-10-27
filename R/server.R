server_eval <- function(expr) {
  withVisible(eval(expr, globalenv()))
}

server_assign <- function(symbol, value) {
  assign(symbol, value, globalenv())
  list(visible = FALSE)
}

server_kill <- function(...) {
  closeAllConnections()
  q(...)
}

#' @export
sessionServer <- function(host = "localhost", port = 6001) {
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
        res <- do.call(sprintf("server_%s", request$command), request$args, quote = TRUE, envir = globalenv())
        obj <- if (inherits(res, "try-error")) res
        else if (res$visible) res
        else list(visible = FALSE)
        serialize(obj, con)
      }, error = function(e) message(e, "\n"),
        warning = function(w) message(w, "\n"),
        finally = close(con))
    }
  }
}
