#' Fetch query result from database
#'
#' This method is an adaptation from the eponymous function in the `odbc`
#' package and is an implementation of the method `dbFetch` defined in the
#' `DBI` package.  Additional to the `odbc` package, it replaces a cryptic
#' error message by an informative error message.
#'
#' @inheritParams DBI::dbFetch
#' @importFrom DBI dbFetch
#' @importFrom methods setMethod
#' @importFrom  odbc dbColumnInfo
#' @importFrom utils getFromNamespace
#' @export
setMethod(
  "dbFetch", "OdbcResult",
  function(res, n = -1, ...) {
    result_fetch <- getFromNamespace("result_fetch", "odbc")
    tryCatch(
      df <- result_fetch(res@ptr, n),
      error = function(e) {
        if (grepl("Descriptor", e) & grepl("index", tolower(e))) {
          info <- dbColumnInfo(res)
          columns <-
            paste0(
              info[info$type %in% c("-1", "-2", "-3", "-4", "-10", "-151"),
                   "name"],
              collapse = "', '"
            )
          if (.Platform$OS.type == "unix") {
            columns <-
              paste0(
                columns,
                "' and fields of the datatypes 'nvarchar(max)' and 'varchar(max)" #nolint
              )
          }
          stop(
            paste0(
              "Query problem: install the latest version of odbc (>= 1.2.3)
              or put field(s) '",
              columns,
              "' at the end of the select statement
              (info in vignet odbc query problem,
              run vignette('odbc query problem', package = 'inbodb'))"
            )
          )
        } else {
          stop(e)
        }
      }
    )

    df
  })
