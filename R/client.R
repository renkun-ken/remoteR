get_client_name <- function(info = Sys.info()) {
  sprintf("%s@%s", info[["user"]], info[["nodename"]])
}

#' @export
sessionClient <- function(name = get_client_name(), server = "localhost", port = 6001) {
  e <- new.env(FALSE, emptyenv(), 10L)
  e$name <- name
  e$server <- server
  e$port <- port
  e$assign <- function(symbol, value, timeout = 10L) {
    con <- socketConnection(host = server, port = port, blocking = TRUE,
      server = FALSE, open = "r+", timeout = timeout)
    on.exit(close(con))
    request <- list(client = name, time = Sys.time(), command = "assign",
      args = list(symbol = symbol, value = value))
    serialize(request, con)
    response <- unserialize(con)
    if (inherits(response, "try-error")) stop(response, "\n")
    else invisible(value)
  }
  e$send <- function(expr, timeout = 10L) {
    expr <- substitute(expr)
    con <- socketConnection(host = server, port = port, blocking = TRUE,
      server = FALSE, open = "r+", timeout = timeout)
    on.exit(close(con))
    request <- list(client = name, time = Sys.time(), command = "send", args = list(expr = expr))
    serialize(request, con)
    response <- unserialize(con)
    if (inherits(response, "try-error")) stop(response, "\n")
    else invisible()
  }
  e$eval <- function(expr, timeout = 10L) {
    expr <- substitute(expr)
    con <- socketConnection(host = server, port = port, blocking = TRUE,
      server = FALSE, open = "r+", timeout = timeout)
    on.exit(close(con))
    request <- list(client = name, time = Sys.time(), command = "eval", args = list(expr = expr))
    serialize(request, con)
    response <- unserialize(con)
    if (inherits(response, "try-error")) stop(response, "\n")
    else if (is.character(response)) cat(response, "\n")
    else if (is.list(response)) {
      if (response$visible) response$value
      else invisible(response$value)
    } else {
      warning("Invalid returned value from server", call. = FALSE)
      response
    }
  }
  e$kill <- function(..., timeout = 10L) {
    con <- socketConnection(host = server, port = port, blocking = TRUE,
      server = FALSE, open = "r+", timeout = timeout)
    on.exit(close(con))
    request <- list(client = name, time = Sys.time(), command = "kill", args = list(...))
    serialize(request, con)
    invisible()
  }
  class(e) <- "sessionClient"
  lockEnvironment(e, bindings = TRUE)
  e
}

print.sessionClient <- function(x, ...) {
  cat(sprintf("<sessionClient>\nname: %s\nserver: %s\nport: %d\n", x$name, x$server, x$port))
}
