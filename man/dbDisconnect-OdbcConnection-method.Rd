% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odbc_dbdisconnect.R
\name{dbDisconnect,OdbcConnection-method}
\alias{dbDisconnect,OdbcConnection-method}
\title{Close database connection}
\usage{
\S4method{dbDisconnect}{OdbcConnection}(conn, ...)
}
\arguments{
\item{conn}{A \linkS4class{DBIConnection} object, as returned by
\code{\link[DBI:dbConnect]{dbConnect()}}.}

\item{...}{Other parameters passed on to methods.}
}
\description{
This method is an adaptation to the INBO databases from the eponymous
function in the \code{odbc} package and is an implementation of the method
\code{dbDisconnect} defined in the \code{DBI} package.
}
