#' Fetch query result from database
#'
#' This method is an adaptation from the eponymous function in the `odbc` package and is an implementation of the method `dbFetch` defined in the `DBI` package.  Additional to the `odbc` package, it replaces an cryptic error message by an informative error message.  In Windows OS and if chosen encoding = "utf-8" in connect_inbo_dbase(), it moreover assures that characters are interpreted correctly (while their presentation depends on the settings of the locale).
#'
#' @inheritParams DBI::dbFetch
#' @importFrom DBI dbFetch
#' @importFrom  odbc dbColumnInfo
#' @importFrom utils getFromNamespace
#' @importFrom dplyr %>% filter mutate_at
#' @importFrom rlang .data
#' @export
setMethod(
  "dbFetch", "OdbcResult",
  function(res, n = -1, ...) {
    result_fetch = getFromNamespace("result_fetch", "odbc")
    tryCatch(
      df <- result_fetch(res@ptr, n),
      error = function(e) {
        if (grepl("Descriptor", e) & grepl("index", tolower(e))) {
          info <- dbColumnInfo(res)
          columns <- paste0(info[info$type %in% c("-1", "-2", "-3", "-4", "-10", "-151"), "name"], collapse = "', '")
          if (.Platform$OS.type == "unix") {
            columns <- paste0(columns, "' and fields of the datatypes 'nvarchar(max)' and 'varchar(max)")
          }
          stop(paste0("Query problem: put field(s) '", columns, "' at the end of the select statement (info in vignet odbc_query_problem)"))
        } else {
          stop(e)
        }
      }
    )

    #convert columns of data type varchar to UTF-8
    if (.Platform$OS.type == "windows" &
        tolower(res@connection@encoding) == "utf-8") {
      info <- dbColumnInfo(res) %>%
        filter(.data$type %in% c("-1", "12"))
      df <- df %>%
        mutate_at(info$name, `Encoding<-`, "latin1")
    }

    df
  })
