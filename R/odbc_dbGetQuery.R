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

    tryCatch(
      df <- dbFetch(rs, n = n, ...),
      error = function(e) {
        if (grepl("Descriptor", e) & grepl("index", tolower(e))) {
          stop("Query problem: put columns with long data (text, image, varchar(max),... at the end of the select statement")
        } else {
          stop(e)
        }
      }
    )

    if (!dbHasCompleted(rs)) {
      warning("Pending rows", call. = FALSE)
    }

    df
  }
)
