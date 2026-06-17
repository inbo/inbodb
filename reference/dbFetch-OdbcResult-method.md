# Fetch query result from database

This method is an adaptation from the eponymous function in the `odbc`
package and is an implementation of the method `dbFetch` defined in the
`DBI` package. Additional to the `odbc` package, it replaces a cryptic
error message by an informative error message.

## Usage

``` r
# S4 method for class 'OdbcResult'
dbFetch(res, n = -1, ...)
```

## Arguments

- res:

  An object inheriting from
  [`DBI::DBIResult`](https://dbi.r-dbi.org/reference/DBIResult-class.html),
  created by
  [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html).

- n:

  maximum number of records to retrieve per fetch. Use `n = -1` or
  `n = Inf` to retrieve all pending records. Some implementations may
  recognize other special values.

- ...:

  Other arguments passed on to methods.

## See also

Other connection:
[`connect_inbo_dbase()`](https://inbo.github.io/inbodb/reference/connect_inbo_dbase.md),
[`dbDisconnect,OdbcConnection-method`](https://inbo.github.io/inbodb/reference/dbDisconnect-OdbcConnection-method.md)
