
#' Connect to an INBO database
#'
#' Connects to an INBO database by simply providing the database's name as an
#' argument.
#' The function can only be used from within the INBO network.
#'
#' For more information, refer to
#' \href{https://tutorials.inbo.be/tutorials/r_database_access/}{this
#' tutorial}.
#'
#' @param database_name char Name of the INBO database you want to connect
#' @param autoconvert_utf8 this argument is not needed any more thanks to better
#' encoding in R for Windows and is therefore deprecated, do not use it any more
#'
#' @return odbc connection
#'
#' @export
#' @family connection
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc odbcListDrivers
#' @importFrom utils tail
#' @importFrom assertthat assert_that
#' @importFrom lifecycle deprecated
#'
#' @author Stijn Van Hoey \email{stijnvanhoey@@gmail.com}
#' @author Els Lommelen \email{els.lommelen@@inbo.be}
#' @examples
#' \dontrun{
#' connection <- connect_inbo_dbase("D0152_00_Flora")
#' connection <- connect_inbo_dbase("W0003_00_Lims")
#' }
connect_inbo_dbase <- function(database_name, autoconvert_utf8 = deprecated()) {

  if (lifecycle::is_present(autoconvert_utf8)) {
    lifecycle::deprecate_warn(
      when = "0.0.10",
      what = "inbodb::connect_inbo_dbase(autoconvert_utf8)",
      details = paste(
        "Argument autoconvert_utf8 used to solve encoding-related problems",
        "in Windows, but as these problems are gone in recent R versions,",
        "this argument is not needed anymore.",
        "Please contact the package maintainer if you still have problems."
      )
    )
  }

  # datawarehouse databases (sql08) start with an M, S or W; most
  # transactional (sql07) with a D (by agreement with dba's)
  if (any(startsWith(database_name, c("M", "S", "W")))) {
    server_new <- "inbo-sql10-prd.inbo.be"
    server_old <- "inbo-sql08-prd.inbo.be"  # DWH server
    type <- "INBO DWH Server"
  } else {
    server_new <- "inbo-sql09-prd.inbo.be"
    server_old <- "inbo-sql07-prd.inbo.be"  # SQL transactional server
    type <- "INBO PRD Server"
  }

  # look up most recent ODBC Driver for SQL Server
  driversvec <- unique(odbcListDrivers()$name)
  drivers_sql <- driversvec[grepl("SQL Server", driversvec)]
  drivers_sql_odbc <-
    drivers_sql[grepl("ODBC Driver", drivers_sql)]
  sql_driver <- tail(sort(drivers_sql_odbc), 1)
  if (length(sql_driver) == 0) {
    stop("The 'ODBC Driver for SQL Server' is missing. Please install it or contact your system administrator.") #nolint
  }

  # connect to database
  conn <- tryCatch(
    dbConnect(
      odbc(),
      driver = sql_driver,
      server = server_new,
      port = 1433,
      database = database_name,
      trusted_connection = "yes",
      encrypt = "no"
    ),
    error = function(e) {
      tryCatch(
        dbConnect(
          odbc(),
          driver = sql_driver,
          server = server_old,
          port = 1433,
          database = database_name,
          trusted_connection = "yes",
          encrypt = "no"
        ),
        error = function(e) {
          assert_that(
            !grepl("connection to SQL Server", e),
            msg = paste(
              e,
              "[INBO] Are you connected to the internet?",
              "Are you connected to the INBO network?",
              "Is the VPN connection active when not @ INBO?",
              "Did you open a tunnel through the bastion?"
            )
          )
          assert_that(
            !grepl("login failed", e),
            msg =
              paste(
                e,
                "[INBO] Is the database name written correct?",
                "Do you have read permissions on the database?"
              )
          )
          stop(e)
        }
      )
    }
  )

  # derived from the odbc package Viewer setup to activate the RStudio Viewer
  code_call <- c(match.call())
  code_call <- paste(c("library(inbodb)",
                       paste("con <-", gsub(", ", ",\n\t", code_call))),
                     collapse = "\n")
  on_connection_opened(conn, code_call, type)

  return(conn)
}


#' RStudio Viewer integration
#'
#' See https://stackoverflow.com/questions/48936851/calling-odbc-connection-within-function-does-not-display-in-rstudio-connection and https://rstudio.github.io/rstudio-extensions/connections-contract.html#persistence #nolint
#' @param connection `odbc` connection
#' @param code `dbase` connection code
#' @param type INBO database server name
#'
#' @importFrom odbc odbcListObjectTypes odbcListObjects odbcListColumns
#' @importFrom odbc odbcPreviewObject
#' @importFrom DBI dbDisconnect
#' @noRd
on_connection_opened <- function(connection, code, type) {
  # make sure we have an observer
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  # use the database name as the display name
  display_name <- paste("INBO Database -", connection@info$dbname)

  # let observer know that connection has opened
  observer$connectionOpened(
    # connection type
    type = type,

    # name displayed in connection pane
    displayName = display_name,

    # host key
    host = connection@info$dbname,

    # icon for connection
    icon = system.file(file.path("static", "logo.png"),
                       package = "inbodb"),

    # connection code
    connectCode = code,

    # disconnection code
    disconnect = function() {
      dbDisconnect(connection)
    },

    listObjectTypes = function() {
      odbcListObjectTypes(connection)
    },

    # table enumeration code
    listObjects = function(...) {
      odbcListObjects(connection, ...)
    },

    # column enumeration code
    listColumns = function(...) {
      odbcListColumns(connection, ...)
    },

    # table preview code
    previewObject = function(rowLimit, ...) { #nolint: object_name_linter.
      odbcPreviewObject(connection, rowLimit, ...)
    },

    # raw connection object
    connectionObject = connection
  )
}
