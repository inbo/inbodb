#' @rdname OdbcConnection
#' @inheritParams DBI::dbGetQuery
#' @inheritParams DBI::dbFetch
#' @inheritParams odbc::dbSendQuery
#' @inheritParams odbc::dbClearResults
#' @inheritParams odbc::dbFetch
#' @inheritParams odbc::dbHasCompleted
#' @export
setMethod(
  "dbGetQuery", signature("OdbcConnection", "character"),
  function(conn, statement, n = -1, params = NULL, ...) {
    rs <- dbSendQuery(conn, statement, params = params, ...)
    on.exit(dbClearResult(rs))

    df <- dbFetch(rs, n = n, ...),

    if (!dbHasCompleted(rs)) {
      warning("Pending rows", call. = FALSE)
    }

    df
  }
)
