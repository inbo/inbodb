on_connection_closed <- function(connection) {
  # make sure we have an observer
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  # provide information no DWH or database
  if (grepl("08", connection@info$servername)) {
    type <- "INBO DWH Server"
  } else if (grepl("07", connection@info$servername)) {
    type <- "INBO PRD Server"
  }

  observer$connectionClosed(type, connection@info$dbname)
}

#' Close database connection
#'
#' This method is an adaptation to the INBO databases from the eponymous function in the `odbc` package and is an implementation of the method `dbDisconnect` defined in the `DBI` package.
#'
#' @inheritParams DBI::dbDisconnect
#'
#' @importFrom odbc dbIsValid
#' @importFrom utils getFromNamespace
#' @export
setMethod(
  "dbDisconnect", "OdbcConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      warning("Connection already closed.", call. = FALSE)
    }

    on_connection_closed(conn)
    conn_release = getFromNamespace("connection_release", "odbc")
    conn_release(conn@ptr)
    invisible(TRUE)
  })
