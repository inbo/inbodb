# Close database connection

This method is an adaptation to the INBO databases from the eponymous
function in the `odbc` package and is an implementation of the method
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
defined in the `DBI` package.

## Usage

``` r
# S4 method for class 'OdbcConnection'
dbDisconnect(conn, ...)
```

## Arguments

- conn:

  A
  [`DBI::DBIConnection`](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
  object, as returned by
  [`connect_inbo_dbase()`](https://inbo.github.io/inbodb/reference/connect_inbo_dbase.md).

- ...:

  Other parameters passed on to methods.

## See also

Other connection:
[`connect_inbo_dbase()`](https://inbo.github.io/inbodb/reference/connect_inbo_dbase.md),
[`dbFetch,OdbcResult-method`](https://inbo.github.io/inbodb/reference/dbFetch-OdbcResult-method.md)
