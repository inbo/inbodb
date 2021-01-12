
#' Connect to an INBO database
#'
#' Connects to an INBO database by simply providing the database's name as an
#' argument.
#' The function can only be used from within the INBO network.
#'
#' For more information, refer to
#' \href{https://inbo.github.io/tutorials/tutorials/r_database_access/}{this tutorial}.
#'
#' @param database_name char Name of the INBO database you want to connect
#' @param autoconvert_utf8 Should the encoding of the tables that are retrieved
#' from the database be adapted to ensure correct presentation?  Defaults to TRUE.
#'
#' @return odbc connection
#'
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc odbcListDrivers
#' @importFrom utils tail
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#'
#' @author Stijn Van Hoey \email{stijnvanhoey@@gmail.com}
#' @author Els Lommelen \email{els.lommelen@@inbo.be}
#' @examples
#' \dontrun{
#' connection <- connect_inbo_dbase("D0021_00_userFlora")
#' connection <- connect_inbo_dbase("W0003_00_Lims")
#' }
connect_inbo_dbase <- function(database_name, autoconvert_utf8 = TRUE) {

    assert_that(is.flag(autoconvert_utf8), noNA(autoconvert_utf8))
    encoding <-
        ifelse(autoconvert_utf8 & .Platform$OS.type == "windows", "latin1", "")

    # datawarehouse databases (sql08) start with an M, S or W; most
    # transactional (sql07) with a D (by agreement with dba's)
    if (any(startsWith(database_name, c("M", "S", "W")))) {
        server = "inbo-sql08-prd.inbo.be"  # DWH server
        type <- "INBO DWH Server"
    } else {
        server = "inbo-sql07-prd.inbo.be"  # SQL transactional server
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
    conn <- dbConnect(odbc(),
                      driver = sql_driver,
                      server = server,
                      port = 1433,
                      database = database_name,
                      encoding = encoding,
                      trusted_connection = "YES")

    # derived from the odbc package Viewer setup to activate the Rstudio Viewer
    code_call <- c(match.call())
    code_call <- paste(c("library(inbodb)",
                         paste("con <-", gsub(", ", ",\n\t", code_call))),
                       collapse = "\n")
    on_connection_opened(conn, code_call, type)

    return(conn)
}


#' Rstudio Viewer integration
#'
#' See https://stackoverflow.com/questions/48936851/calling-odbc-connection-within-function-does-not-display-in-rstudio-connection and https://rstudio.github.io/rstudio-extensions/connections-contract.html#persistence
#' @param connection odbc connection
#' @param code dbase connection code
#' @param type INBO database server name
#'
#' @importFrom odbc odbcListObjectTypes odbcListObjects odbcListColumns
#' odbcPreviewObject odbcConnectionActions
#' @importFrom DBI dbDisconnect
#' @noRd
on_connection_opened <- function(connection, code, type) {
    # make sure we have an observer
    observer <- getOption("connectionObserver")
    if (is.null(observer))
        return(invisible(NULL))

    # use the database name as the display name
    display_name <- paste("INBO Database -", connection@info$dbname)
    server_name <- connection@info$servername

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
        previewObject = function(rowLimit, ...) {
            odbcPreviewObject(connection, rowLimit, ...)
        },

        # other actions that can be executed on this connection
        actions = odbcConnectionActions(connection),

        # raw connection object
        connectionObject = connection
    )
}
