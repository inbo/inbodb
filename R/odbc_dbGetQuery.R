#' @rdname OdbcConnection
#' @inheritParams DBI::dbGetQuery
#' @inheritParams DBI::dbFetch
#' @inheritParams odbc::dbSendQuery
#' @inheritParams odbc::dbClearResults
#' @inheritParams odbc::dbFetch
#' @inheritParams odbc::dbColumnInfo
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
          info <- dbColumnInfo(rs)
          columns <- paste0(info[info$type %in% c("-1", "-2", "-3", "-4", "-10", "-151", "-152"), "name"], collapse = "', '")
          if (.Platform$OS.type == "unix") {
            columns <- paste0(columns, "' and fields of the datatypes 'nvarchar(max)' and 'varchar(max)")
          }
          stop(paste0("Query problem: put field(s) '", columns, "' at the end of the select statement."))
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
